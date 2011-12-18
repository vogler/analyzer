#include <stdio.h>
#include <assert.h>
#include <fenv.h>
 
int main()
{
  float f;
  f = 1;
  f++;
  printf("%f", f);

  float g;
  g = 8;
  g = g % 3; // error: invalid operands to binary % (have ‘float’ and ‘int’)
  printf("%f", g);

  float k;
  k = 1;
  k = k << 1; // error: invalid operands to binary << (have ‘float’ and ‘int’)
  printf("%f", k);

  float l;
  l = 3;
  l = l & 5;
  l = l | 4;
  l = l ^ 3;
  l = l << 1;
  l = l >> 1;
  l = l && 1;
  l = l || 1;
}
