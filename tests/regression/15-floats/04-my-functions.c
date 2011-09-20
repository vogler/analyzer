// PARAM: --result pretty
#include<assert.h>
#include<stdlib.h>

#define NUM 5.5

int getNumber(){
	return NUM;
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
		f2 = 3.0; // f2 immernoch 1.0, funktioniert bei branch
	}else{
		f2 = -3.0; // same
	}
	// strided intervals
	assert(f2 >= (-3.0f));
	assert(f2 <= 3.0f);

	float f3 = getNumber();
	assert(f3 == NUM);

	return 0;
}
