// PARAM: --oil 02-example.oil --tramp 06-suffix-tramp.h --osektasksuffix _func --osekisrsuffix _func

// #include "02-example-tramp.h"


int x;
int y;
int z;
int t;

ISR( ii ) {
   GetResource(ri);
    z = 20;
   ReleaseResource(ri);
   return;
}


ISR( i) {
   GetResource(r);
   y++;
   x--;
   ReleaseResource(r);
   return;
}

TASK(t) {
   GetResource(ri);
   x=10;
   y=0;
   ReleaseResource(ri);
   GetResource(r);
   t=x+y;
   x=t-x;
   ReleaseResource(r);
   z= 2*t; //RACE
   GetResource(r);
   y=t-y;
   ReleaseResource(r);
   TerminateTask();
return;}