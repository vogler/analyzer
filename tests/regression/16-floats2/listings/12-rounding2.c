#include <assert.h>

#define N 0.5000000298023224154508881156289135105907917022705078125

int main (void)
{
  float f; 

  f =  N;

  // is fine
  assert(f == (float)N);
  // fails on excecution
  assert(f == N);
}
