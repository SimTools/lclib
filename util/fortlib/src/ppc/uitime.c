#include <time.h>
void uitime_(int *sec)
{
	struct tm *timeptr;
        time_t timer;
        int hr, mn, sc;
        
        timer = time((void *)0);
        timeptr = localtime(&timer);
        hr = (timeptr->tm_hour);
        mn = (timeptr->tm_min);
        sc = (timeptr->tm_sec);
        *sec = 3600*hr + 60*mn + sc;
}
