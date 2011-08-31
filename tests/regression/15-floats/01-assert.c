// PARAM: --result compact -o /home/ralf/workspace/test2/goblint.xml
// just a few sanity checks on the asserts
#include<assert.h>

int main() {
  float f = 1.0+2.0;
  assert(f == 3.0f);
  return 0;
}
