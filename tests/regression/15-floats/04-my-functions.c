// PARAM: --result pretty --propset float_domain interval
#include<assert.h>
#include<stdlib.h>

#define NUM 5.5

float getNumber(float f){
	return NUM + f;
}

int getRandom(){ // TODO test params
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
		r=r;
	}else{
		f2 = -3.0;
		r=r;
	}
	// strided intervals
	assert(f2 >= (-3.0f));
	assert(f2 <= 3.0f);

	float f3 = getNumber(1.5);
	assert(f3 == 7.0);

	return 0;
}
