// PARAM: --result pretty
#include<stdio.h>
#include<assert.h>

float glob1;
float glob2 = 9;

void vergleiche (double x, double y){
 if(cos (x) != cos (y))
  printf("paradox\n");
 else
  printf("wuerde jeder so erwarten\n");
}

int main() {
  float i,j,k;

  // double == float?
  assert(1.1 == 1.1f);

	// http://de.wikipedia.org/wiki/Gleitkommazahl#Pr.C3.BCfung_auf_Gleichheit
	assert(.362*100. != 36.2);
	assert(.362*100./100. != .362);

  return 0;
}
