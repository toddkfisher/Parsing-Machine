#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <endian.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "util.h"
#include "machine.h"
#include "prototypes.h"

char *instruction_names[] = {
  /* 00 : */ "OP_END_OF_LIST",
  /* 01 : */ "OP_FAIL",
  /* 02 : */ "OP_CHAR",
  /* 03 : */ "OP_ANY",
  /* 04 : */ "OP_CHOICE",
  /* 05 : */ "OP_JUMP",
  /* 06 : */ "OP_CALL",
  /* 07 : */ "OP_RETURN",
  /* 08 : */ "OP_COMMIT",
  /* 09 : */ "OP_PARTIAL_COMMIT",
  /* 10 : */ "OP_MAKE_CAPTURE_SLOTS",
  /* 11 : */ "OP_BEGIN_CAPTURE",
  /* 12 : */ "OP_END_CAPTURE",
  /* 13 : */ "OP_INVALIDATE_ACTIVE_CAPTURES",
  /* 14 : */ "OP_HALT_SUCCESSFULLY",
};

/// Save/load

#define BUFFER_SIZE (sizeof(INSTRUCTION)*1024)  // in bytes - must be a multiple of sizeof(INSTRUCTION);
#define BUFFER_MAX_INSTUCTIONS (BUFFER_SIZE/sizeof(INSTRUCTION))

void io_write_instructions_to_file(char *file_name, INSTRUCTION *prog, int n_instructions)
{
  uint8_t output_buffer[BUFFER_SIZE];
  INSTRUCTION output_instruction;
  int buff_idx;
  int ip;
  FILE *fout = fopen(file_name, "wb");
  if (NULL == fout) {
    fprintf(stderr, "Failed to open '%s' for writing.\n", file_name);
    exit(0);
  }
  for (ip = 0, buff_idx = 0; ip < n_instructions; ++ip) {
    //print_instruction(&prog[ip]);
    output_instruction.instr_opcode = htole16(prog[ip].instr_opcode);
    output_instruction.instr_union = htole16(prog[ip].instr_union);
    memcpy(&output_buffer[buff_idx], &output_instruction, sizeof(INSTRUCTION));
    buff_idx += sizeof(INSTRUCTION);
    if (buff_idx >= BUFFER_SIZE || ip + 1 >= n_instructions) {
      if (!fwrite(output_buffer, sizeof(uint8_t), buff_idx, fout)) {
        fprintf(stderr, "Write error to '%s'.\n", file_name);
        exit(0);
      }
      buff_idx = 0;
    }
  }
  fclose(fout);
}

int io_read_instructions_from_file(char *file_name, INSTRUCTION *prog, int n_max_instructions)
{
  FILE *fin;
  int n_instructions_to_read;
  int n_instructions_in_file;
  int ip;
  INSTRUCTION *input_instruction_list;
  struct stat statbuf;
  off_t file_size_bytes;
  if (-1 == stat(file_name, &statbuf)) {
    fprintf(stderr, "Error getting file size of '%s'.\n", file_name);
    exit(0);
  }
  file_size_bytes = statbuf.st_size;
  if (0 != file_size_bytes % sizeof(INSTRUCTION)) {
    fprintf(stderr, "Error : file size of '%s' is not a multiple of instruction size.\n", file_name);
    exit(0);
  }
  n_instructions_in_file = file_size_bytes/sizeof(INSTRUCTION);
  n_instructions_to_read = MIN(n_max_instructions, n_instructions_in_file);
  if (NULL == (fin = fopen(file_name, "rb"))) {
    fprintf(stderr, "Unable to open file '%s' for reading.\n", file_name);
    exit(0);
  }
  if (n_instructions_to_read != fread(prog, sizeof(INSTRUCTION), n_instructions_to_read, fin)) {
    fprintf(stderr, "Error reading instruction list (count mismatch) on file '%s'.\n", file_name);
    fclose(fin);
    exit(0);
  }
  fclose(fin);
  for (ip = 0; ip < n_instructions_to_read; ++ip, ++prog) {
    prog->instr_opcode = le16toh(prog->instr_opcode);
    prog->instr_union = le16toh(prog->instr_union);
  }
  return n_instructions_to_read;
}

/// Stack

void stk_push_return_addr(MACHINE *mp, int addr)
{
  mp->machine_ret_stack[mp->machine_stk_ptr].stkent_type = ENTRY_CALL_FRAME;
  mp->machine_ret_stack[mp->machine_stk_ptr].stkent_return_addr = addr;
  mp->machine_stk_ptr += 1;
}

void stk_push_backtrack_entry(MACHINE *mp, int addr, char *tp)
{
  mp->machine_ret_stack[mp->machine_stk_ptr].stkent_type = ENTRY_BACKTRACK;
  mp->machine_ret_stack[mp->machine_stk_ptr].stkent_backtrack_addr = addr;
  mp->machine_ret_stack[mp->machine_stk_ptr].stkent_backtrack_tp = tp;
  mp->machine_stk_ptr += 1;
}

void stk_pop(MACHINE *mp, STACKENTRY *e)
{
  memcpy(e, &mp->machine_ret_stack[--(mp->machine_stk_ptr)], sizeof(STACKENTRY));
}

int stk_topentry_type(MACHINE *mp)
{
  return mp->machine_ret_stack[mp->machine_stk_ptr - 1].stkent_type;
}

void stk_change_backtrack_tp(MACHINE *mp, char *new_tp)
{
  if (mp->machine_stk_ptr <= 0) {
    exec_flag_halt(mp, H_FATAL, "Stack is empty. (stk_change_backtrack_tp()).");
  } else if (ENTRY_BACKTRACK != stk_topentry_type(mp)) {
    exec_flag_halt(mp, H_FATAL, "Topmost stack entry is not type ENTRY_BACKTRACK (stk_change_backtrack_tp()).");
  } else {
    mp->machine_ret_stack[mp->machine_stk_ptr - 1].stkent_backtrack_tp = new_tp;
  }
}

/// Printing

void print_instruction(int ip, INSTRUCTION *pinstr)
{
  printf("%04d : %-25s ", ip, instruction_names[pinstr->instr_opcode]);
  switch (pinstr->instr_opcode) {
    case OP_COMMIT:
    case OP_CHOICE:
    case OP_JUMP:
    case OP_PARTIAL_COMMIT:
      printf("%04d", pinstr->instr_jump_addr);
      break;
    case OP_CALL:
      printf("%04d", pinstr->instr_call_addr);
      break;
    case OP_CHAR:
      printf("'%c'", pinstr->instr_char);
      break;
  }
  printf("\n");
}

void print_stackentry(int stkptr, STACKENTRY *e)
{
  printf("%03d : ", stkptr);
  switch (e->stkent_type) {
    case ENTRY_BACKTRACK:
      printf("Backtrack addr   = %04d, ", e->stkent_backtrack_addr);
      printf("Backtrack to text: ");
      print_summary_string(e->stkent_backtrack_tp, 4);
      printf("\n");
      break;
    case ENTRY_CALL_FRAME:
      printf("Ret addr: %04d", e->stkent_return_addr);
      printf("\n");
      break;
    default:
      break;
  }
}

void print_machine_state(MACHINE *mp, int print_flags)
{
  int sp;
  if (P_INSTRUCTION & print_flags) {
    printf("ip = %04d\n", mp->machine_ip);
    print_instruction(mp->machine_ip, &mp->machine_prog[mp->machine_ip]);
  }
  if (P_RET_STACK & print_flags) {
    for (sp = mp->machine_stk_ptr; sp; --sp) {
      print_stackentry(sp, &(mp->machine_ret_stack[sp - 1]));
      if (sp) {
        printf("----------------\n");
      }
    }
  }
}

void print_all_instructions(INSTRUCTION *pinst, int n_instructions)
{
  int ip;
  for (ip = 0; ip < n_instructions; ++ip) {
    print_instruction(ip, pinst++);
  }
}

/// Execution and struct MACHINE related.

MACHINE *m_new_machine(INSTRUCTION *prog)
{
  MACHINE *mp = (MACHINE *) malloc(sizeof(MACHINE));
  zero_mem(mp, sizeof(MACHINE));
  mp->machine_prog = prog;
  mp->machine_ip = 0;
  mp->machine_stk_ptr = 0;
  mp->machine_tp = NULL;
  mp->machine_target_text = NULL;
  mp->machine_halt = 0;
  mp->machine_halt_code = -1;
  strcpy(mp->machine_halt_message, "");
  return mp;
}

void m_set_target_text(MACHINE *mp, char *target_text)
{
  mp->machine_target_text, target_text;
  mp->machine_tp = target_text;
}

void exec_fail_backtrack(MACHINE *mp)
{
  STACKENTRY e;
  while (mp->machine_stk_ptr && ENTRY_CALL_FRAME == stk_topentry_type(mp)) {
    mp->machine_stk_ptr -= 1;
  }
  if (mp->machine_stk_ptr) {
    stk_pop(mp, &e);
    mp->machine_tp = e.stkent_backtrack_tp;
    mp->machine_ip = e.stkent_backtrack_addr;
  } else {
    exec_flag_halt(mp, H_PARSE_FAILED, "Parse failed.\n");
  }
}

// Note: c == 0 --> match any single character.
int exec_test_char(MACHINE *mp, char c)
{
  if (!*mp->machine_tp) {
    return 0;
  } else if (!c) {
    return 1;
  } else {
    return c == *mp->machine_tp;
  }
}

void exec_flag_halt(MACHINE *mp, int halt_code, char *halt_message)
{
  mp->machine_halt = 1;
  mp->machine_halt_code = halt_code;
  strcpy(mp->machine_halt_message, halt_message);
}

void exec_run(MACHINE *mp, int run_mode, int print_flags)
{
  mp->machine_halt = 0;
  while (!mp->machine_halt) {
    printf("%04d : ", mp->machine_ip);
    print_instruction(mp->machine_ip, mp->machine_prog + mp->machine_ip);
    switch (mp->machine_prog[mp->machine_ip].instr_opcode) {
      case OP_END_OF_LIST:
        exec_flag_halt(mp, H_FATAL, "End of instruction list (?).");
        return;
        break;
      case OP_FAIL:
        exec_fail_backtrack(mp);
        break;
      case OP_CHAR:
        if (!exec_test_char(mp, mp->machine_prog[mp->machine_ip].instr_char)) {
          exec_fail_backtrack(mp);
        } else {
          mp->machine_ip += 1;
          mp->machine_tp += 1;
        }
        break;
      case OP_ANY:
        if (!exec_test_char(mp, '\0')) {
          exec_fail_backtrack(mp);
        } else {
          mp->machine_ip += 1;
          mp->machine_tp += 1;
        }
        break;
      case OP_CHOICE:
        stk_push_backtrack_entry(mp, mp->machine_prog[mp->machine_ip].instr_jump_addr, mp->machine_tp);
        mp->machine_ip += 1;
        break;
      case OP_JUMP:
        mp->machine_ip = mp->machine_prog[mp->machine_ip].instr_jump_addr;
        break;
      case OP_CALL:
        stk_push_return_addr(mp, mp->machine_ip + 1);
        mp->machine_ip = mp->machine_prog[mp->machine_ip].instr_call_addr;
        break;
      case OP_RETURN: {
        STACKENTRY e;
        stk_pop(mp, &e);
        if (ENTRY_CALL_FRAME != e.stkent_type) {
          exec_flag_halt(mp, H_FATAL, "Topmost stack entry is not type ENTRY_CALL_FRAME on "
                         "execution of OP_RETURN.\n");
        } else {
          mp->machine_ip = e.stkent_return_addr;
        }
        break;
      }
      case OP_COMMIT: {
        STACKENTRY e;
        stk_pop(mp, &e);
        // Just a check.  If rule was compiled properly, then ENTRY_BACKTRACK is guaranteed to be topmost on the stack.
        if (ENTRY_BACKTRACK != e.stkent_type) {
          print_stackentry(-1, &e);
          exec_flag_halt(mp, H_FATAL, "Topmost stack entry is not type ENTRY_BACKTRACK on "
                         "execution of OP_COMMIT.\n");
        } else {
          mp->machine_ip = mp->machine_prog[mp->machine_ip].instr_jump_addr;
        }
        break;
      }
      case OP_PARTIAL_COMMIT:
        stk_change_backtrack_tp(mp, mp->machine_tp);
        mp->machine_ip = mp->machine_prog[mp->machine_ip].instr_jump_addr;
        break;
      case OP_HALT_SUCCESSFULLY:
        if (!*mp->machine_tp) {
          exec_flag_halt(mp, H_SUCCESS, "Successful parse.\n");
        } else {
          exec_flag_halt(mp, H_PARSE_FAILED, "Parse failed. Not all text consumed.\n");
        }
        break;
      default:
        exec_flag_halt(mp, H_FATAL, "Unknown opcode encountered.\n");
        break;
    }
  }
  printf("HALT: %s", mp->machine_halt_message);
  print_machine_state(mp, P_RET_STACK);
}
