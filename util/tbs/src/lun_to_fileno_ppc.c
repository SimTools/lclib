#include <stdio.h>
#include <f2c.h>
#include "fio.h"
void lun_to_fileno_ (int *lun, int *fn)
{
        *fn = fileno(f__units[*lun].ufd);
/*
	*fn = ftell_(lun);
*/
}

