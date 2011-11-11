#include <assert.h>

#define N 0.5000000298023224154508881156289135105907917022705078125

void 
set_fpu (unsigned int mode) {
  asm ("fldcw %0" : : "m" (*&mode));
}

int main (void)
{
 set_fpu (0x27F); 

 float f; 
 
 f = N;

 assert(f == (float)N);
 assert(f != N);

} 

