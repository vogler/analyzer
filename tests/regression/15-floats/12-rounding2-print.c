#include <assert.h>
#include <stdio.h>

#define N 0.5000000298023224154508881156289135105907917022705078125

int main (void)
{
 float f; 
 
 f = N; 

 assert(f == (float)N);
 printf("assert ok: %f == (float)%f\n", f, (float)N);
 assert(f != N);
 printf("assert ok: %f != %f\n", f, (float)N);
} 


