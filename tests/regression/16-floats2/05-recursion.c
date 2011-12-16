// PARAM: --result pretty
#include<stdio.h>
#include<assert.h>

float fact(float x) {
  if (x < 2) 
    return 1;
  else 
    return x * fact (x-1);
}

int main () {
  float a = 1;
  
  a = fact(20);
  assert(a == 2432902008176640000.0);

  return 0;
}
