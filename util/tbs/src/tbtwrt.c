
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
extern struct mt_info mtst ;

/*******************************
** 
**  Subroutine TBTWRT_( WRTBUF, NRET )
**(Function)
**  Write data 
**(Input)
**  WRTBUF  : Data to be written.
**     WRTBUF(0) is a number of words.
**     WRTBUF(1) to WRTBUF(WRTBUF(0)) is data.
**(Output)
**  NRET ; = 0 when OK, otherwise error.
**
**(Author)
**   A. Miyamoto  6-June-1994
**  
********************************/
  
void tbtwrt_( wrtbuf, nret )
long nret, *wrtbuf;
{
  extern struct mt_info mtst;
  int nbyte, lw;


  nbyte = wrtbuf[0]+1;
  lw = write(mtst.fd, wrtbuf, nbyte);
  
  mtst.nread=mtst.nread+1;
  
  nret=0;
  return;
}
