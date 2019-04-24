#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include "util.h"
#include "machine.h"
#include "prototypes.h"

INSTRUCTION prog[] = {
#include "prog.h"
  I_END_OF_LIST
};

INSTRUCTION prog_in[128];

int prog_size(INSTRUCTION *prog)
{
  int size;
  for (size = 0; OP_END_OF_LIST != prog->instr_opcode; ++prog) {
    size += 1;
  }
  return size;
}

int main(int argc, char **argv)
{
  MACHINE *mp = m_new_machine(prog);
  int n_instr_read;
  m_set_target_text(mp, "(1+2)");
  //exec_run(mp, RMODE_UNINTERRUPTED, P_RET_STACK | P_INSTRUCTION);
  printf("prog_size(prog) == %d\n", prog_size(prog));
  printf("sizeof(INSTRUCTION) == %ld\n", sizeof(INSTRUCTION));
  io_write_instructions_to_file("prog.dump", prog, prog_size(prog));
  n_instr_read = io_read_instructions_from_file("prog.dump", prog_in, 128);
  printf("# instructions read == %d.\n", n_instr_read);
  print_all_instructions(prog_in, n_instr_read);
}
