#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void printint(int64_t i) {
  printf("%lld\n", (long long)i);
}

void printbool(int b) {
  printf("%s\n", b ? "true" : "false");
}
