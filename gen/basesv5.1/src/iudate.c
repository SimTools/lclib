#include <time.h>
/* long	iutime() */
main()
{
	struct	tm q;
	int	year;

	time(&q);
        year = q.tm_year;
	printf(" %d \n",year);
	return;
}
