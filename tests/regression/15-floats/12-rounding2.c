// PARAM: --result pretty
#include <assert.h>

int main (void)
{
 float f; 
 
 f =  0.5000000298023224154508881156289135105907917022705078125;

 assert(f == (float)0.5000000298023224154508881156289135105907917022705078125); //is fine.
 assert(f == 0.5000000298023224154508881156289135105907917022705078125); //fails on excecution but NOT in the analysis!
 
} 





