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

/// Save/load

// BUFFER_SIZE is in bytes - must be a multiple of sizeof(INSTRUCTION);
#define BUFFER_SIZE (sizeof(INSTRUCTION)*1024)
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

void io_copy_u16_as_LE(uint16_t *dest, uint16_t *src, size_t count)
{
  int i;
  while(count) {
    *dest++ = htole16(*src++);
    count -= 1;
  }
}

void io_write_machine(MACHINE *mp, char *file_name)
{
  struct stat statbuf;
  off_t file_size_bytes;
  uint8_t *raw_buffer;
  IO_HEADER *phdr;
  FILE *fout;
  if (NULL == (fout = fopen(file_name, "wb"))) {
    fprintf(stderr, "Unable to open file '%s' for reading.\n", file_name);
    exit(0);
  }
  io_buffer_size =
    sizeof(uint32_t) + // iobuff_size
    PATH_MAX +         // iobuff_semantic_action_lib_name
    sizeof(uint16_t) + // iobuff_n_instructions
    sizeof(uint16_t) + // iobuff_n_symbols
    mp->machine_symtab.stbl_n_symbols*sizeof(SYMBOL_BUFFER) +
    mp->machine_prog_size*sizeof(INSTRUCTION);
  if (NULL == (pbuff = (IO_BUFFER *) malloc(sizeof(uint8_t)*io_buffer_size))) {
    fprintf(stderr, "Memory overflow on IO_BUFFER allocation.\n");
    fclose(fout);
    exit(0);
  }
  phdr->iobuff_size = io_buffer_size;
  strcpy(phdr->iobuff_semantic_action_lib_name, mp->machine_semantic_action_lib_name);
  phdr->iobuff_n_instructions = mp->machine_prog_size;
  phdr->iobuff_n_symbols = mp->machine_symtab.stbl_n_symbols;
  // left off here
  // 1. copy machine symbol table (as little-endian)
  // 2. copy program insructtions (as little-endian)
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
