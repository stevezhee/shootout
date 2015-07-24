#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <inttypes.h>

void print_v16w4(uint64_t x)
{
  uint64_t d;

  printf("[");
  while(x != 0)
    {
      d = x & 0xf;
      printf("%lu;",d);
      x >>= 4;
    }
  printf("]\n");
}

//#define NBODY
//#define FANNKUCHREDUX
#define SPECTRAL

#if defined(NBODY)
double foo(int);
#endif
#if defined(FANNKUCHREDUX)
int foo(int);
#endif
#if defined(SPECTRAL)
// double foo(int);
int foo(unsigned int);
#endif

void arg_check(int argc, int n)
{
  if(argc - 1 < n)
    {
      printf("need %d argument(s)\n", n);
      exit(-1);
    }
}

int main(int argc, char ** argv)
{
#if defined(NBODY)
  printf("N-Body:");
  arg_check(argc, 1);
  int n = atoi(argv[1]);
  double m = foo(n);
  printf ("%.9f\n", m);
#endif
#if defined(FANNKUCHREDUX)
  printf("Pfannkuchen:");
  arg_check(argc, 1);
  int n = atoi(argv[1]);
  if((n < 3) || (n > 15))
    {
      printf("need an argument between 1 and 15\n");
      return 0;
    }

  printf("(%d) = %d\n", n, foo(n));
#endif
#if defined(SPECTRAL)
  printf("spectral:");
  arg_check(argc, 1);
  int n = atoi(argv[1]);
  // BAL: printf("%0.9f\n",foo(n));
  printf("%d\n",foo(n));
#endif
  return 0;
}
