/*
  File : Clock.c
  Description : Clock Function

  Author : Ryosuke Itoh, TOPAZ, KEK
  Date : 17 - OCT - 1990
*/
#include <time.h>
#include <sys/types.h>

void clock_ ( itime, dum1, dum2 )

/* arg */
int *itime;
int *dum1,*dum2;

{
/* var */
  time_t tb;
/* func */
  time_t time();

/* main */
/*
  tb = time(NULL);
  *itime = tb;
*/
  *itime = 0;
}
  
