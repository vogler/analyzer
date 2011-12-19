// PARAM: --result pretty --propset float_domain interval
#include<assert.h>
#include<stdlib.h>

#define NUM 5.5

float getNumber(){
	return NUM;
}

int getRandom(){
	srand(time(NULL));
	return rand();
}

int main() {
	float f1 = 1.0+2.0;
	assert(f1 == 3.0);

	float f2 = 1.0;
	int r = getRandom();
	if(r){
		f2 = 3.0;
	}else{
		f2 = -3.0;
	}
	assert(f2 == -3.0 || f2 == 3.0); // UNKNOWN

	float f3 = getNumber();
	assert(f3 == NUM);

	return 0;
}
