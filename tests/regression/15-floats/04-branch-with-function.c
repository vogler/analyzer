// PARAM: --result pretty --propset float_domain interval
#include<assert.h>
#include<stdlib.h>

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
		r=r;
	}else{
		f2 = -3.0;
		r=r;
	}

	assert(f2 >= (-3.0f));
	assert(f2 <= 3.0f);

	return 0;
}
