/*	Example to get time.		*/

#include <sys/types.h>
#include <time.h>
#include <sys/times.h>
#ifndef CLK_TCK
#define CLK_TCK 60  /* For Sun Only */
#endif
clockm_(icpu)
int *icpu;
{
	struct tms	now;
	float  factor=1000./CLK_TCK;

 	times(&now);
	*icpu = (float)now.tms_utime*factor;
  
}

