#include <stdio.h>
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

int main(int argc, char ** argv)
{
  int n = atoi(argv[1]);

#include "gen.c"

  return 0;
}
