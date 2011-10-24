// PARAM: --result pretty
#include<assert.h>
#include<stdlib.h>


int getRandom(){
	srand(time(NULL));
	return rand();
}

int main() {
	int f2 = 1;
	int r = getRandom();

	if(r){
		f2 = 3;
	}else{
		f2 = -3;
	}
	// strided intervals
	assert(f2 >= (-3));
	assert(f2 <= 3);

	return 0;
}
