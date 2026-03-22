#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void printint(int32_t i) {
  printf("%d\n", i);
}

void printbool(uint8_t b) {
  printf("%s\n", b ? "true" : "false");
}
