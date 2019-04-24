#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "util.h"

void zero_mem(void *p, size_t n)
{
  explicit_bzero(p, n);
}

void print_summary_string(char *s, size_t length)
{
  char buf[MAX_SUMMARY_LENGTH];
  size_t n = MIN(MAX_SUMMARY_LENGTH, length + 1);
  long slen = strlen(s);
  zero_mem(buf, n);
  if (n) {
    snprintf(buf, n, s);
    printf("%s", buf);
  }
  if (n - 1 < slen) {
    int i;
    for (i = n - 1; i - (n - 1) < 3 && i < slen; ++i) {
      printf(".");
    }
  }
}
