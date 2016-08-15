#include <stdio.h>

#define S_PI ""
#define t_PI ""

struct S {
  int a;
  char b[1024];
  char *c;
};

typedef struct {
  int a;
  char b[1024];
  char *c;
}  t_t;

int main() {
  printf("Hello World!");
}
