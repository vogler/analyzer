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
  printf("3.4e38f==3.4e38: %i\n", 3.4e38f==3.4e38); // 0
  // warning: floating constant exceeds range of ‘float’

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
