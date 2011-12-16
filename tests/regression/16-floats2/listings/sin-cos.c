#include <stdio.h>

void check(double x, double y){
  if(cos (x) != cos (y))
    printf("Surprise\n");
  else
    printf("Expected\n");
}

int main(){
    check(1, 1);
}