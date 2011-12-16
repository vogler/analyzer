// PARAM: --result pretty
#include <assert.h>
#include <stdio.h>

int main (void)
{
  double d;
  float f, fd; 

  // long double -> double
  d =  0.5000000894069671353303618843710864894092082977294921875;
  // long double -> float
  f =  0.5000000894069671353303618843710864894092082977294921875f;
  // long double -> double -> float
  fd = 0.5000000894069671353303618843710864894092082977294921875;

  assert(d != f);
  assert(d != fd);
  assert(f != fd);
  
  // assert(isfinite(f));
  
  assert(d  == 0x1.000003p-1);
  assert(f  == 0x1.000002p-1);
  assert(fd == 0x1.000004p-1);
  
  printf ("d =  %a\n",d);
  printf ("f =  %a\n",f);
  printf ("fd = %a\n",fd);
}
