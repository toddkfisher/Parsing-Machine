#pragma once

#define OP_END_OF_LIST 0
#define OP_FAIL 1
#define OP_CHAR 2
#define OP_ANY 3
#define OP_CHOICE 4
#define OP_JUMP 5
#define OP_CALL 6
#define OP_RETURN 7
#define OP_COMMIT 8
#define OP_PARTIAL_COMMIT 9
#define OP_BEGIN_CAPTURE 10
#define OP_END_CAPTURE 11
#define OP_REENTER_INVALIDATION_SCOPE 12
#define OP_LEAVE_INVALIDATION_SCOPE 13
#define OP_RUN_SEMANTIC_ACTION 14
#define OP_CREATE_CAPTURE_SLOTS 15
#define OP_RANGE 16
#define OP_HALT_SUCCESSFULLY 17

#define OPCODE_NAME_AT_IDX(opcode) [(opcode)] = #opcode

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
    // OP_COMMIT, OP_CHOICE, OP_JUMP, OP_PARTIAL_COMMIT, OP-CALL
    uint16_t instr_addr;
    // OP_CREATE_CAPTURE_SLOTS
    uint16_t instr_n_capture_slots;
    // OP_BEGIN_CAPTURE,
    // OP_END_CAPTURE,
    // OP_REENTER_INVALIDATION_SCOPE,
    // OP_LEAVE_INVALIDATION_SCOPE
    uint16_t instr_capture_slot_idx;
    // OP_RUN_SEMANTIC_ACTION
    uint16_t instr_semantic_action_num;
    // OP_RANGE
    struct {
      char instr_begin_char;
      char instr_end_char;
    };
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
#define I_CHOICE(addr) { .instr_opcode = OP_CHOICE, .instr_addr = (addr) }
#define I_JUMP(addr) { .instr_opcode = OP_JUMP, .instr_addr = (addr) }
#define I_CALL(addr) { .instr_opcode = OP_CALL, .instr_addr = (addr) }
#define I_COMMIT(addr) { .instr_opcode = OP_COMMIT, .instr_addr = (addr) }
#define I_PARTIAL_COMMIT(addr) { .instr_opcode = OP_PARTIAL_COMMIT, .instr_addr = (addr) }
#define I_BEGIN_CAPTURE(n) { .instr_opcode = OP_BEGIN_CAPTURE, .instr_capture_slot_idx = (n) }
#define I_END_CAPTURE(n) { .instr_opcode = OP_END_CAPTURE, .instr_capture_slot_idx = (n) }
#define I_REENTER_INVALIDATION_SCOPE(n) { .instr_opcode = OP_REENTER_INVALIDATION_SCOPE, .instr_capture_slot_idx = (n) }
#define I_LEAVE_INVALIDATION_SCOPE(n) { .instr_opcode = OP_LEAVE_INVALIDATION_SCOPE, .instr_capture_slot_idx = (n) }
#define I_CREATE_CAPTURE_SLOTS(n) { .instr_n_capture_slots = (n) }

enum STACKENTRY_TYPE {
  ENTRY_CALL_FRAME,
  ENTRY_BACKTRACK
};

typedef struct SEMANTIC_VALUE {
  int sval_rule_id;
  struct SEMANTIC_VALUE *sval_next;
  //%%USER-DEFINED-FIELDS%%
} SEMANTIC_VALUE;

typedef struct CAPTURE_SLOT {
  int capture_is_valid;
  char *capture_name;
  char *capture_beginning_of_text;
  char *capture_end_of_text;
  int capture_n_semantic_values;
  SEMANTIC_VALUE *capture_semantic_value;
} CAPTURE_SLOT;

typedef struct STACKENTRY {
  int stkent_type;
  union {
    // ENTRY_CALL_FRAME
    struct {
      int stkent_return_addr;
      int stkent_n_captures;
      CAPTURE_SLOT *stkent_capture_list;
      // This exists because there might be backtrack stack entries between
      // call frames.
      struct STACKENTRY *stkent_prev_call_frame;
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

#define SYMBOL_MAX 32

typedef struct SYMBOL {
  char sym_name[SYMBOL_MAX];
  uint16_t sym_addr;
} SYMBOL;

#define SYMTAB_MAX 1024

typedef struct SYMBOL_TABLE {
  uint16_t stbl_n_symbols;
  // Entries are sorted lexicographically, in ASCII order.
  SYMBOL stbl_entries[];
} SYMBOL_TABLE;

typedef struct WATCHLIST_ENTRY {
  uint16_t swl_address;
  uint16_t swl_symtab_idx;
} WATCHLIST_ENTRY;

typedef struct SYMBOL_WATCHLIST {
  uint16_t swl_n_on_watchlist;
  // Entries are sorted numerically.
  WATCHLIST_ENTRY *swl_entries;
}

#define HALT_MESSAGE_MAX 1024

// Parsing machine state.  Bundled into a struct so "programs" (arrays of INSTRUCTIONS) are
// re-entrant.
typedef struct MACHINE {
  // Program being executed.
  unsigned machine_prog_size;
  INSTRUCTION *machine_prog;
  // Topmost call frame.
  STACKENTRY *machine_top_call_frame;
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
  // Name of dll/shared lib containing code for semantic actions.
  char machine_semantic_action_lib_name[PATH_MAX];
  // Handle to dll/shared lib named above.
  void *machine_semantic_action_dlhandle;
  // Entry point to semantic action shared lib.
  SEMANIC_ACTION_EXEC_FN machine_SA_exec_fn;
  // Symbol table and watchlist (for debugging a parser).
  SYMBOL_TABLE *machine_symtab;
  SYMBOL_WATCHLIST machine_watchlist;
} MACHINE;

//
//   | bit | use                 |
//   |-----+---------------------|
//   |   0 | printing flag       |
//   |   1 | printing flag       |
//   |   2 | printing flag       |
//   |   3 | printing flag       |
//   |   4 | printing flag       |
//   |   5 | 0th order bit for # |
//   |   6 | 1st order bit for # |
//   |   7 | 2nd order bit for # |
//
enum PRINT_MACHINE_STATE_FLAGS {
  P_RET_STACK = 0x01,
  P_INSTRUCTION = 0x04,
  P_MAX = 0x10
};

// Example use: SET_PRINT_FLAGS_NUM(P_RET_STACK_TOP_N, 3)
// to indicate that only the top three entries of the return stack should be printed.
// Note: 0 <= n <= 7.
// n == 0 is an implied "all" or "none" depending on the context.
#define SET_PRINT_FLAGS_NUM(flags, n) ((flags) | (((n) & 0x07) << 5)
#define GET_PRINT_FLAGS_NUM(flags) (((flags) & (0x07 << 5)) >> 5)

enum RUN_MODE {
  RMODE_UNINTERRUPTED,
  RMODE_STEP
};
