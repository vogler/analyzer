// PARAM: --result pretty
#include<assert.h>
#include<stdlib.h>


int getRandom(){ // TODO test params
	srand(time(NULL));
	return rand();
}

int main() {
	int f2 = 1;
	int r = getRandom();

	if(r){
		f2 = 3; // f2 immernoch 1.0, funktioniert bei branch
	}else{
		f2 = -3; // same
	}
	// strided intervals
	assert(f2 >= (-3));
	assert(f2 <= 3);

	return 0;
}
