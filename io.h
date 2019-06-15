#pragma once

typedef struct SYMBOL_BUFFER {
  char symbuff_name[SYMBOL_MAX];
  uint16_t symbuff_addr;
};

typedef struct IO_BUFFER {
  uint32_t iobuff_size;
  char iobuff_semantic_action_lib_name[PATH_MAX];
  uint16_t iobuff_n_instructions;
  uint16_t iobuff_n_symbols;
  uint8_t iobuff_contents[];
} IO_BUFFER;
