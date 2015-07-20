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

  // #define FASTPOW
#define NBODY
/* #if defined(FASTPOW) */
/* int foo(int, unsigned int); */
/* #elseif defined(NBODY) */
double foo(int);
/* #else */
/* double foo(double); */
/* #endif */

int main(int argc, char ** argv)
{
/* #ifdef FASTPOW */
/*   if(argc < 2) */
/*     { */
/*       printf("need 2 arguments\n"); */
/*       return 0; */
/*     } */
/*   int n = atoi(argv[1]); */
/*   unsigned int m = atoi(argv[2]); */
/*   printf("%d\n", foo(n, m)); */
/*   return 0; */
/* #elseif defined(NBODY) */
  if(argc < 1)
    {
      printf("need an argument\n");
      return 0;
    }
  int n = atoi(argv[1]);
  double m = foo(n);
  printf ("%.9f\n", m);
  return 0;
/* #else */
/*   if(argc < 1) */
/*     { */
/*       printf("need an argument\n"); */
/*       return 0; */
/*     } */
/*   double n = atof(argv[1]); */
/*   double m = foo(n); */
/*   printf("%f\n", m); */
/*   return 0; */

/*   // #include "gen.c" */
/*   //  return 0; */
/* #endif */
}
