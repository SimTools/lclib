#include <stdio.h>
#include <f2c.h>
#include "fio.h"
void test2_(int *lu)
{
	int  fn;
	FILE *fd;
	char buf[1024];
/*
	fn  = ftell_(lu);
	printf("lu = %d fn = %d\n",*lu,fn);
	read(fn,buf,4);
	printf("buf = %c%c%c%c\n",buf[0],buf[1],buf[2],buf[3]);
*/
	printf("use f__units\n");
/*
	fd = f__units[*lu].ufd;
	fn = fileno(fd);
*/
	lun_to_fileno_(lu,&fn);
	printf("lu = %d fn = %d\n",*lu,fn);
	read(fn,buf,4);
	printf("buf = %c%c%c%c\n",buf[0],buf[1],buf[2],buf[3]);
}
