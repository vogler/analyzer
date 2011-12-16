#include <assert.h>
#include <stdio.h>
#include <math.h>

int check(double x){
  int b = cos(x) != cos(x);
  if(b)
    printf("Surprise\n");
  else
    printf("Expected\n");
  return b;
}

int main(){
    assert(!check(0));
    assert(check(1));
}
