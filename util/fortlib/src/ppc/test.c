void main()
{
	char *hizuke[8], *jikan[8];
	int itim;
	
	date_(hizuke);
	utime_(jikan);
	uitime_(&itim);
	printf("date  : %s\n",hizuke);
	printf("utime : %s\n",jikan);
	printf("time  : %d\n",time_());
	printf("uitime: %d\n",itim);
}
