#include <time.h>
void utime_(char *timc)
{
        struct tm *timeptr;
        time_t timer;
        int hr, mn, sc;
        
        timer = time((void *)0);
        timeptr = localtime(&timer);
        hr = (timeptr->tm_hour);
        mn = (timeptr->tm_min);
        sc = (timeptr->tm_sec);
        sprintf(timc,"%2.2d:%2.2d:%2.2d",hr,mn,sc);
}
