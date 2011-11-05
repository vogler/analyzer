// PARAM: --result pretty
#include <assert.h>

void 
set_fpu (unsigned int mode) {
  asm ("fldcw %0" : : "m" (*&mode));
}

int main (void)
{
 set_fpu (0x27F); 

 float f; 
 
 f =  0.5000000298023224154508881156289135105907917022705078125;

 assert(f == (float)0.5000000298023224154508881156289135105907917022705078125); //is fine.
 assert(f == 0.5000000298023224154508881156289135105907917022705078125); //fails on excecution but NOT in the analysis!
 
} 


