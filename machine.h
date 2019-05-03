#pragma once

enum OPCODE {
/* 00 : */ OP_END_OF_LIST = 0,
/* 01 : */ OP_FAIL,
/* 02 : */ OP_CHAR,
/* 03 : */ OP_ANY,
/* 04 : */ OP_CHOICE,
/* 05 : */ OP_JUMP,
/* 06 : */ OP_CALL,
/* 07 : */ OP_RETURN,
/* 08 : */ OP_COMMIT,
/* 09 : */ OP_PARTIAL_COMMIT,
/* 10 : */ OP_MAKE_CAPTURE_SLOTS,
/* 11 : */ OP_BEGIN_CAPTURE,
/* 12 : */ OP_END_CAPTURE,
/* 13 : */ OP_REENTER_INVALIDATION_SCOPE,
/* 14 : */ OP_LEAVE_INVALIDATION_SCOPE,
/* 15 : */ OP_HALT_SUCCESSFULLY,
};

typedef struct INSTRUCTION {
  // Opcode tag for union.
  uint16_t instr_opcode;
  union {
    // OP_FAIL,
    // OP_ANY,
    // OP_RETURN,
    // OP_HALT,
    //   - no data
    // OP_CHAR (not a character for ease of reading/writing)
    uint16_t instr_char;
    // OP_COMMIT, OP_CHOICE, OP_JUMP, OP_PARTIAL_COMMIT
    uint16_t instr_jump_addr;
    // OP_CALL
    uint16_t instr_call_addr;
    // OP_CREATE_CAPTURE_SLOTS
    uint16_t instr_n_capture_slots;
    // OP_BEGIN_CAPTURE,
    // OP_END_CAPTURE,
    // OP_REENTER_INVALIDATION_SCOPE,
    // OP_LEAVE_INVALIDATION_SCOPE
    uint16_t instr_capture_slot_idx;
    // placeholder/name for binary save/load
    uint16_t instr_union;
  };
} INSTRUCTION;

#define I_FAIL { .instr_opcode = OP_FAIL }
#define I_HALT_SUCCESSFULLY { .instr_opcode = OP_HALT_SUCCESSFULLY }
#define I_END_OF_LIST { .instr_opcode = OP_END_OF_LIST }
#define I_RETURN { .instr_opcode = OP_RETURN }
#define I_CHAR(c) { .instr_opcode = OP_CHAR, .instr_char = (c) }
#define I_ANY { .instr_opcode = OP_ANY }
#define I_CHOICE(addr) { .instr_opcode = OP_CHOICE, .instr_jump_addr = (addr) }
#define I_JUMP(addr) { .instr_opcode = OP_JUMP, .instr_jump_addr = (addr) }
#define I_CALL(addr) { .instr_opcode = OP_CALL, .instr_call_addr = (addr) }
#define I_COMMIT(addr) { .instr_opcode = OP_COMMIT, .instr_jump_addr = (addr) }
#define I_PARTIAL_COMMIT(addr) { .instr_opcode = OP_PARTIAL_COMMIT, .instr_jump_addr = (addr) }
#define I_MAKE_CAPTURE_SLOTS(n) { .instr_opcode = OP_MAKE_CAPTURE_SLOTS, .instr_n_capture_slots = (n) }
#define I_BEGIN_CAPTURE(n) { .instr_opcode = OP_BEGIN_CAPTURE, .instr_capture_slot_idx = (n) }
#define I_END_CAPTURE(n) { .instr_opcode = OP_END_CAPTURE, .instr_capture_slot_idx = (n) }
#define I_REENTER_INVALIDATION_SCOPE(n) { .instr_opcode = OP_REENTER_INVALIDATION_SCOPE, .instr_capture_slot_idx = (n) }
#define I_LEAVE_INVALIDATION_SCOPE(n) { .instr_opcode = OP_LEAVE_INVALIDATION_SCOPE, .instr_capture_slot_idx = (n) }


enum STACKENTRY_TYPE {
  ENTRY_CALL_FRAME,
  ENTRY_BACKTRACK
};

typedef struct CAPTURE {
  int capture_valid;
  char *capture_name;
  char *capture_beginning_of_text;
  char *capture_end_of_text;
} CAPTURE;

typedef struct STACKENTRY {
  int stkent_type;
  union {
    // ENTRY_CALL_FRAME
    struct {
      int stkent_return_addr;
      int stkent_n_captures;
      CAPTURE *stkent_capture_list;
    };
    // ENTRY_BACKTRACK
    struct {
      int stkent_backtrack_addr;
      char *stkent_backtrack_tp;
    };
  };
} STACKENTRY;

#define STACK_SIZE 1024

enum HALT_CODE {
  H_SUCCESS,
  H_PARSE_FAILED,
  H_FATAL
};

#define HALT_MESSAGE_MAX 1024

// Parsing machine state.  Bundled into a struct so "programs" (arrays of INSTRUCTIONS) are
// re-entrant.
typedef struct MACHINE {
  // Program being executed.
  INSTRUCTION *machine_prog;
  // Offset of current instruction being executed.
  int machine_ip;
  // Call return/backtrack stack.
  int machine_stk_ptr;
  STACKENTRY machine_ret_stack[STACK_SIZE];
  // Current position of text being scanned.
  char *machine_tp;
  // Target text being scanned.
  char *machine_target_text;
  // Stop execution.
  int machine_halt;
  // Reason for halting.
  int machine_halt_code;
  // Message to print just before stopping.
  char machine_halt_message[HALT_MESSAGE_MAX];
} MACHINE;

enum PRINT_MACHINE_STATE_FLAGS {
  P_RET_STACK = 0x01,
  P_INSTRUCTION = 0x02
};

enum RUN_MODE {
  RMODE_UNINTERRUPTED,
  RMODE_STEP
};
