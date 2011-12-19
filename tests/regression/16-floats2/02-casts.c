#include <assert.h>

#define N 16777217.0

int main()
{
  float f =  N;
  // float == (float)double
  assert(f == (float)N);
  // float != double
  assert(f != N);
  
  assert(2.8 != 2);
  assert((int)2.8 == 2); // truncated
} 
