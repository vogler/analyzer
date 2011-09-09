// PARAM: --result pretty
#include<assert.h>
#include<stdlib.h>

int main() {
  float f = 1.0+2.0;
  assert(f == 3.0);

  float f2 = 1.0;
  srand(time(NULL));
  int r = rand();
  if(r){
	f2 = 3.0;
  }else{
  	f2 = -3.0;
  }
  assert(f2 == -3.0 || f2 == 3.0); // UNKNOWN

  return 0;
}
