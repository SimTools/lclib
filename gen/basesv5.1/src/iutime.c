#include <sys/times.h>
long	iutime()
{
	struct	tms q;
	long 	t,s;

	times(&q);
        t = q.tms_utime;
	return t;
}
