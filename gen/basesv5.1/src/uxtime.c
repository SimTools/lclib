#include <sys/times.h>
#include <unistd.h>
/*main()*/
float	uxtime() 
{
	struct	tms q;
	long 	t,s;
	long    sysconf();
	long    ticks;
	float   uxtime;


        ticks = sysconf(_SC_CLK_TCK);
	times(&q);
        t = q.tms_utime;
	return (float) t/ (float) ticks;
}
