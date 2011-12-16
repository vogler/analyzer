#include <assert.h>
 
int main()
{
  // inexact float != exact double
  assert(16777217.0f != 16777217.0);
  assert(0.5000000298023224154508881156289135105907917022705078125f != 
	   0.5000000298023224154508881156289135105907917022705078125);

  // max range of float
  assert(3.5e38f != 3.5e38);

  // max range of double
  assert(1.8e308 != 1.8e308L);
}
