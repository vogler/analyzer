#include <assert.h>

#define N 16777217.0

int main()
{
  float f =  N;
  // float == float
  assert(f == (float)N);
  // float != double
  assert(f != N);
} 
