#include <stdio.h>
#include <assert.h>
#include <fenv.h>
 
int getMode()
{
  int mode = fegetround();
  if(mode == FE_TONEAREST) // 0
    printf("FE_TONEAREST %i\n", mode);
  if(mode == FE_UPWARD) // 2048
    printf("FE_UPWARD %i\n", mode);
  if(mode == FE_DOWNWARD) // 1024
    printf("FE_DOWNWARD %i\n", mode);
  if(mode == FE_TOWARDZERO) // 3072
    printf("FE_TOWARDZERO %i\n", mode);
  return mode;
}

void setMode(int mode)
{
  int ok = fesetround(mode);
  assert(ok == 0);
}

int main()
{
  getMode();
  setMode(FE_TOWARDZERO);
  getMode();
  setMode(FE_UPWARD);
  getMode();
  setMode(FE_DOWNWARD);
  getMode();
  assert(getMode()==FE_DOWNWARD);
}
