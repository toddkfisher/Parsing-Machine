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

unsigned prog_size(INSTRUCTION *prog)
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
  //m_set_target_text(mp, "(1+2)");
  //exec_run(mp, RMODE_UNINTERRUPTED, P_RET_STACK | P_INSTRUCTION);
  printf("sizeof(MACHINE) == %ld\n", sizeof(MACHINE));
  print_all_instructions(prog);
}
