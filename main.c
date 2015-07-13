#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

typedef struct
{
  double x;
  double y;
  double z;
  double vx;
  double vy;
  double vz;
  double mass;
} body_t;

int foo(int, unsigned int);

int main(int argc, char ** argv)
{
#define FASTPOW
#ifdef FASTPOW
  if(argc < 2)
    {
      printf("need 2 arguments\n");
      return 0;
    }
  int n = atoi(argv[1]);
  unsigned int m = atoi(argv[2]);
  printf("%d\n", foo(n, m));
  return 0;
#else
  int n = atoi(argv[1]);

#include "gen.c"
  return 0;
#endif
}
