#include <stdio.h>
 
int main()
{
  int i   = 16777217;
  float f = 16777217.0;
  printf("integer: %i\n", i); // 16777217
  printf("float:   %f\n", f); // 16777216.000000
  printf("integer==float: %i\n", i == f); // results in 0 if int and float are 32 bit

  printf("16777217.0==float: %i\n", 16777217.0 == f); // results in 0
}
