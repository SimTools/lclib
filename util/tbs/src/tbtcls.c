
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

#include "tbtctl.h"

struct mt_info mtst;


/********************************************************************
**  Subroutine TBTCLS_( NRET )
**(Function)
**  Close MT file.
**(Input)
**  none
**(Output)
**  NRET   ; = 0 when OK, otherwise <0
**(Author)
**  A. Miyamoto  6-June-1994
*********************************************************************/
void tbtcls_( nret )
int *nret;
{
  extern struct mt_info mtst;

  int iret;
/*
  printf("tbtcls ... mtst.device=%s\n",mtst.device);
*/
  iret=close(mtst.fd);
  mtst.fd=-10;
  bzero(fhdr.dummy,sizeof(fhdr.dummy));
  mtst.recfm=NULL;
  mtst.lrecl=0;
  mtst.nxt=-1;
  mtst.lst=0;

  if(mtst.nread<0){     /* mtst.nread=-1 when read enf-of-file */
    mtst.lpos=mtst.lpos+1;
    mtst.nread=0;
  }

}
