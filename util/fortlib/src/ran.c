float ran(iseed)
int *iseed;
{
	float x;
	static long FLOAT_MAX = 2147483647;
	x = (float)random()/FLOAT_MAX;
	return x;
}
