#include <assert.h>
 
int main()
{
    int i   = 16777217; // exact
    float f = 16777217.0; // inexact for 32 bit
    assert(i != f); // if int and float are both 32 bit
    assert(16777217.0 != f); // double!=float
    assert(16777217.0f == f); // float==float
    
    float g = i; // int -> float
    assert(g == f);
}
