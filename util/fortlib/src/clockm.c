/*	Example to get time.		*/

#include <sys/types.h>
#include <time.h>
#include <sys/times.h>
#define CLK_TCK 60  /* For Sun Only */
clockm_(icpu)
int *icpu;
{
	struct tms	now;
	float  factor=1000./CLK_TCK;

 	times(&now);
	*icpu = (float)now.tms_utime*factor;
  
}

