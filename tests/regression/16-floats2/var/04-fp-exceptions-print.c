#include <assert.h>
#include <stdio.h>
#include <math.h>

int main(){
  printf("sqrt(-1): %f\n", sqrt(-1)); // -nan, exception ‘Invalid Operation’ -> quiet

  // float:	 1.5e-45   .. 3.4e38
  // double:	 5.0e-324  .. 1.7e308
  // long doube: 1.9e-4951 .. 1.1e4932

  // float
  printf("3.4e38: %e\n", 3.4e38f); // 3.4e38
  printf("3.5e38: %e\n", 3.5e38f); // inf
  // printf("isfinite(3.5e38f): %i\n", isfinite(3.5e38f)); // 0
  printf("3.4e38f==3.4e38: %i\n", 3.4e38f==3.4e38); // 0
  // warning: floating constant exceeds range of ‘float’
  
  float f1, f2;
  f1 = 1.0f;
  f2 = 2.0f;
  printf("hex 3.4e38: %x\n", *(unsigned int*)&f1);
  printf("hex 3.5e38: %x\n", *(unsigned int*)&f2);
// 3.4e38f (max): 0 11111110 11111111100100110011110
// 3.5e38f (inf): 0 11111111 00000000000000000000000
// -1.0f        : 1 01111111 00000000000000000000000
// 1.0f         : 0 01111111 00000000000000000000000
// 2.0f         : 0 10000000 00000000000000000000000


  // double
  printf("1.7e308: %e\n", 1.7e308); // 1.7e308
  printf("1.8e308L: %e\n", 1.8e308); // inf
  // warning: floating constant exceeds range of ‘double’

  // long double
  printf("1.1e4932L: %Le\n", 1.1e4932L); // 1.1e4932L
  printf("1.2e4932L: %Le\n", 1.2e4932L); // inf
  // warning: floating constant exceeds range of ‘long double’

  printf("1/0: %i\n", 1/0); // exception ‘Division by Zero’ -> trap
  // warning: division by zero
  printf("0/0: %i\n", 0/0); // exception ‘Division by Zero’ -> trap
  // warning: division by zero
}
