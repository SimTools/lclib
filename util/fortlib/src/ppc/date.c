#include <time.h>
time_t time();
void date_(char *datec)
{
	const struct tm *timeptr;
	time_t timer;
	int yy, mm, dd;

	timer = time((void *)0);
	timeptr = localtime(&timer);
	yy = (timeptr->tm_year);
	mm = (timeptr->tm_mon) + 1;
	dd = (timeptr->tm_mday);
	sprintf(datec,"%2.2d-%2.2d-%2.2d",yy,mm,dd);
}
