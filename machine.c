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
  OPCODE_NAME_AT_IDX(OP_END_OF_LIST),
  OPCODE_NAME_AT_IDX(OP_FAIL),
  OPCODE_NAME_AT_IDX(OP_CHAR),
  OPCODE_NAME_AT_IDX(OP_ANY),
  OPCODE_NAME_AT_IDX(OP_CHOICE),
  OPCODE_NAME_AT_IDX(OP_JUMP),
  OPCODE_NAME_AT_IDX(OP_CALL),
  OPCODE_NAME_AT_IDX(OP_RETURN),
  OPCODE_NAME_AT_IDX(OP_COMMIT),
  OPCODE_NAME_AT_IDX(OP_PARTIAL_COMMIT),
  OPCODE_NAME_AT_IDX(OP_BEGIN_CAPTURE),
  OPCODE_NAME_AT_IDX(OP_END_CAPTURE),
  OPCODE_NAME_AT_IDX(OP_REENTER_INVALIDATION_SCOPE),
  OPCODE_NAME_AT_IDX(OP_LEAVE_INVALIDATION_SCOPE),
  OPCODE_NAME_AT_IDX(OP_RUN_SEMANTIC_ACTION),
  OPCODE_NAME_AT_IDX(OP_CREATE_CAPTURE_SLOTS),
  OPCODE_NAME_AT_IDX(OP_RANGE),
  OPCODE_NAME_AT_IDX(OP_HALT_SUCCESSFULLY),
};


/// Stack

STACKENTRY *stk_push_call_frame(MACHINE *mp, int addr)
{
  STACKENTRY *pentry = mp->machine_ret_stack + mp->machine_stk_ptr;
  pentry->stkent_type = ENTRY_CALL_FRAME;
  pentry->stkent_return_addr = addr;
  pentry->stkent_n_captures = 0;
  pentry->stkent_capture_list = NULL;
  pentry->stkent_prev_call_frame = mp->machine_top_call_frame;
  mp->machine_stk_ptr += 1;
  return pentry;
}

void stk_push_backtrack_entry(MACHINE *mp, int addr, char *tp)
{
  STACKENTRY *pentry = mp->machine_ret_stack + mp->machine_stk_ptr;
  pentry->stkent_type = ENTRY_BACKTRACK;
  pentry->stkent_backtrack_addr = addr;
  pentry->stkent_backtrack_tp = tp;
  mp->machine_stk_ptr += 1;
}

void stk_pop(MACHINE *mp, STACKENTRY *e)
{
  memcpy(e, &mp->machine_ret_stack[--(mp->machine_stk_ptr)], sizeof(STACKENTRY));
}

#define STK_TOPENTRY(mp) ((mp)->machine_ret_stack + ((mp)->machine_stk_ptr - 1))

#define STK_TOPENTRY_TYPE(mp) (STK_TOPENTRY(mp)->stkent_type)

void stk_change_backtrack_tp(MACHINE *mp, char *new_tp)
{
  if (mp->machine_stk_ptr <= 0) {
    exec_flag_halt(mp, H_FATAL, "Stack is empty. (stk_change_backtrack_tp()).");
  } else if (ENTRY_BACKTRACK != STK_TOPENTRY_TYPE(mp)) {
    exec_flag_halt(mp, H_FATAL, "Topmost stack entry is not type ENTRY_BACKTRACK (stk_change_backtrack_tp()).");
  } else {
    mp->machine_ret_stack[mp->machine_stk_ptr - 1].stkent_backtrack_tp = new_tp;
  }
}

/// Printing

void print_instruction(int ip, INSTRUCTION *pinstr, int is_current_instruction)
{
  printf("%c%4d : %-22s ", is_current_instruction ? '*' : ' ', ip, instruction_names[pinstr->instr_opcode]);
  switch (pinstr->instr_opcode) {
    case OP_END_OF_LIST:
    case OP_FAIL:
    case OP_ANY:
    case OP_RETURN:
    case OP_HALT_SUCCESSFULLY:
      break;
    case OP_CHAR:
      printf("'%c'", (char) pinstr->instr_char);
      break;
    case OP_CHOICE:
    case OP_COMMIT:
    case OP_JUMP:
    case OP_PARTIAL_COMMIT:
    case OP_CALL:
      printf("%4d", pinstr->instr_addr);
      break;
    case OP_BEGIN_CAPTURE:
    case OP_END_CAPTURE:
    case OP_REENTER_INVALIDATION_SCOPE:
    case OP_LEAVE_INVALIDATION_SCOPE:
      printf("%d", pinstr->instr_capture_slot_idx);
      break;
    case OP_RUN_SEMANTIC_ACTION:
      printf("%d", pinstr->instr_semantic_action_num);
      break;
    case OP_CREATE_CAPTURE_SLOTS:
      printf("%d", pinstr->instr_n_capture_slots);
      break;
  }
  printf("\n");
}

void print_stackentry(int stkptr, STACKENTRY *e)
{
  printf("%03d : ", stkptr);
  switch (e->stkent_type) {
    case ENTRY_BACKTRACK:
      printf("ENTRY_BACKTRACK\n");
      printf("  backtrack_addr = %04d\n", e->stkent_backtrack_addr);
      printf("  backtrack_tp   =\"");
      print_summary_string(e->stkent_backtrack_tp, 4);
      printf("\"\n");
      break;
    case ENTRY_CALL_FRAME: {
      int i;
      printf("ENTRY_CALL_FRAME, memory address %p:\n", e);
      printf("  return_addr     = %04d\n", e->stkent_return_addr);
      printf("  n_captures      = %d\n", e->stkent_n_captures);
      printf("  prev_call_frame = %p\n", e->stkent_prev_call_frame);
      printf("  capture_list:\n");
      for (i = 0; i < e->stkent_n_captures; ++i) {
        //print_capture_slot(4, e->stkent_capture_list + i);
      }
      break;
    }
    default:
      break;
  }
}

void print_machine_state(MACHINE *mp, int print_flags)
{
  int sp;
  int n;
  if (P_INSTRUCTION & print_flags) {
    n = GET_PRINT_FLAGS_NUM(flags);
    for (k = MAX(ip - n, 0); k <= MIN(ip + n, mp->machine_prog_size - 1); ++k) {
      print_instruction(mp->machine_ip, &mp->machine_prog[mp->machine_ip], ip == k);
    }
  }
  if (P_RET_STACK & print_flags) {
    n = GET_PRINT_FLAGS_NUM(flags);
    for (sp = mp->machine_stk_ptr; n && sp; --sp) {
      print_stackentry(sp, &(mp->machine_ret_stack[sp - 1]));
      if (sp) {
        printf("----------------\n");
      }
    }
    if (sp) {
      printf("And %d more below ... \n", sp);
    }
  }
}

void print_all_instructions(INSTRUCTION *pinst)
{
  int ip;
  for (ip = 0; OP_END_OF_LIST != pinst->instr_opcode; ++ip) {
    print_instruction(ip, pinst++, 0);
  }
}

/// Execution and struct MACHINE related.

MACHINE *m_new_machine(INSTRUCTION *prog)
{
  MACHINE *mp = (MACHINE *) malloc(sizeof(MACHINE));
  zero_mem(mp, sizeof(MACHINE));
  mp->machine_prog = prog;
  mp->machine_prog_size = prog_size(prog);
  mp->machine_ip = 0;
  mp->machine_stk_ptr = 0;
  mp->machine_tp = NULL;
  mp->machine_target_text = NULL;
  mp->machine_halt = 0;
  mp->machine_top_call_frame = NULL;
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
  STACKENTRY *pstkentry;
  STACKENTRY stkentry;
  while (mp->machine_stk_ptr && ENTRY_CALL_FRAME == STK_TOPENTRY_TYPE(mp)) {
    pstkentry = STK_TOPENTRY(mp);
    mp->machine_top_call_frame = pstkentry->stkent_prev_call_frame;
    free(pstkentry->stkent_capture_list);
    mp->machine_stk_ptr -= 1;
  }
  if (mp->machine_stk_ptr) {
    stk_pop(mp, &stkentry);
    mp->machine_tp = stkentry.stkent_backtrack_tp;
    mp->machine_ip = stkentry.stkent_backtrack_addr;
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

int exec_test_range(MACHINE *mp, char begin_char, char end_char)
{
  if (!*mp->machine_tp) {
    return 0;
  } if (begin_char > end_char) {
    // Empty set --> always fail.
    return 0;
  } else {
    return *mp->machine_tp >= begin_char, *mp->machine_tp <= end_char;
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
  INSTRUCTION *pinstr = mp->machine_prog + mp->machine_ip;
  mp->machine_halt = 0;
  while (!mp->machine_halt) {
    switch (pinstr->instr_opcode) {
      // LEFT OFF HERE
      // if debugging, then issue a debugger prompt, read and parse command
      case OP_END_OF_LIST:
        exec_flag_halt(mp, H_FATAL, "End of instruction list (?).");
        return;
        break;
      case OP_FAIL:
        exec_fail_backtrack(mp);
        break;
      case OP_CHAR:
        if (!exec_test_char(mp, pinstr->instr_char)) {
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
      case OP_RANGE:
        if (!exec_test_range(mp, pinstr->instr_begin_char, pinstr->instr_end_char)) {
          exec_fail_backtrack(mp);
        } else {
          mp->machine_ip += 1;
          mp->machine_tp += 1;
        }
        break;
      case OP_CHOICE:
        stk_push_backtrack_entry(mp, pinstr->instr_addr, mp->machine_tp);
        mp->machine_ip += 1;
        break;
      case OP_JUMP:
        mp->machine_ip = pinstr->instr_addr;
        break;
      case OP_CALL:
        mp->machine_top_call_frame = stk_push_call_frame(mp, mp->machine_ip + 1);
        mp->machine_ip = pinstr->instr_addr;
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
          mp->machine_ip = pinstr->instr_addr;
        }
        break;
      }
      case OP_PARTIAL_COMMIT:
        stk_change_backtrack_tp(mp, mp->machine_tp);
        mp->machine_ip = pinstr->instr_addr;
        break;
      case OP_CREATE_CAPTURE_SLOTS:
      {
        STACKENTRY *pentry = STK_TOPENTRY(mp);
        if (NULL == (pentry->stkent_capture_list =
                     (CAPTURE_SLOT *) malloc(sizeof(CAPTURE_SLOT)*pentry->stkent_n_captures))) {
          exec_flag_halt(mp, H_FATAL, "Memory overflow on OP_CREATE_CAPTURE_SLOTS.\n");
        } else {
          int i;
          int n_capture_slots = pinstr->instr_n_capture_slots;
          CAPTURE_SLOT *pslot = pentry->stkent_capture_list;
          for (pslot = pentry->stkent_capture_list; n_capture_slots; --n_capture_slots) {
            pslot->capture_is_valid = 0;
            // capture_name isn't used just yet.
            pslot->capture_name = NULL;
            pslot->capture_beginning_of_text = NULL;
            pslot->capture_end_of_text = NULL;
            pslot->capture_n_semantic_values = 0;
          }
          break;
        }
      }
      case OP_REENTER_INVALIDATION_SCOPE:
      case OP_LEAVE_INVALIDATION_SCOPE:
      {
        STACKENTRY *pentry = STK_TOPENTRY(mp);
        int cs_idx = pinstr->instr_capture_slot_idx;
        pentry->stkent_capture_list[cs_idx].capture_is_valid = (OP_REENTER_INVALIDATION_SCOPE == pinstr->instr_opcode);
        break;
      }
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
