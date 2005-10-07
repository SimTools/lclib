#include <stdio.h>
#if !defined(__GFORTRAN__)
#if defined(__APPLE__)
#include <g2c.h>
#else
#include <f2c.h>
#endif
#else
#include <gfortran.h>
#endif
#include "fio.h"
void lun_to_fileno_ (int *lun, int *fn)
{
        *fn = fileno(f__units[*lun].ufd);
/*
	*fn = ftell_(lun);
*/
}

