// PARAM: --result pretty
// just a few sanity checks on the asserts
#include<assert.h>
#include<stdlib.h>

int getRandom(){
  srand(time(NULL));
  return rand();
}

int main() {
  //int i = 1+2;
  float f = 1.0+2.0;
  float f2 = 1.0;
  int r = getRandom();
  if(r){
	f2 = 3.0;
  }else{
  	f2 = -3.0;
  }
  assert(f == 3.0f);
  return 0;
}
