#include <stdio.h>

int main(){
  if(.362*100. != 36.2)
    printf("unequal 1\n");
 
  if(.362*100./100. != .362)
    printf("unequal 2\n");
}