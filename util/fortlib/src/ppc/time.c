extern int uitime_(int *);
int time_()
{
        int sec;
        uitime_(&sec);
	return sec;
}

