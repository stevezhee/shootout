/* C gcc #1 */
/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Sebastien Loisel
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double eval_A(int i, int j) { return 1.0/((i+j)*(i+j+1)/2+i+1); }

void eval_A_times_u(int N, const double u[], double Au[])
{
  int i,j;
  for(i=0;i<N;i++)
    {
      Au[i]=0;
      for(j=0;j<N;j++)
	{
	  Au[i]+=eval_A(i,j)*u[j];
	}
    }
}

void eval_At_times_u(int N, const double u[], double Au[])
{
  int i,j;
  for(i=0;i<N;i++)
    {
      Au[i]=0;
      for(j=0;j<N;j++)
	{
	  Au[i]+=eval_A(j,i)*u[j];
	}
    }
}

void eval_AtA_times_u(int N, const double u[], double AtAu[])
{
  double v[N];
  eval_A_times_u(N,u,v);
  eval_At_times_u(N,v,AtAu);
}

double foo(int N)
{
  int i;
  double u[N],v[N],vBv,vv;
  for(i=0;i<N;i++)
    {
      // BAL: u[i]=1;
      u[i]=3; // BAL:
      v[i]=2; // BAL:
    }

  /* for(i=0;i<1;i++) */
  /*   //  for(i=0;i<10;i++) */
  /*   { */
  /*     eval_AtA_times_u(N,u,v); */
  /*     eval_AtA_times_u(N,v,u); */
  /*   } */
  vBv=vv=0;
  for(i=0;i<N;i++)
    {
      vBv+=u[i]*v[i];
      /* vv+=v[i]*v[i]; */
    }
  return(vBv);
  // BAL: return(sqrt(vBv/vv));
}
