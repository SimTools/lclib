
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
**  Subroutine TBTDSM_( VOL, NRET )
**(Function)
**  Dismount tape, just rewind for a moment. 
**(Input)
**  VOL    ; Tape volume to be dismounted.
**(Output)
**  NRET   ; = 0 when OK, otherwise <0
**(Author)
**  A. Miyamoto  6-June-1994
*********************************************************************/
void tbtdsm_( vol, nret )
char *vol;
int *nret;
{

  extern struct mt_info mtst;
  
  int errcode=16;
  int iret,nbyte;
  struct mtop mtop;
  struct mtget mt_status;

  if(strcmp(mtst.vol,vol)!=0){
    printf("TBTDSM .. Request %s to be dismounted while",vol);
    printf(" %s is mounted.\n",mtst.vol);
    exit(errcode);
  }
  
  if( mtst.fd >=0 ){
    printf("TBTDSM .. required dismount before closing file.\n");
    printf("File %s is still opened.\n",fhdr.dbuf.name);
    exit(errcode);
  }

  mtst.fd=open(mtst.device, O_RDONLY);
  if(mtst.fd <0 ){
    printf("TBTDSM .. Unable to open %s for rewind.\n",mtst.device);
    exit(errcode);
  }

  printf("TBTDSM .. rewind %s on %s in progress.\n",vol,mtst.device);

  mtop.mt_op=MTREW;
  mtop.mt_count=1;
  iret=ioctl(mtst.fd, MTIOCTOP, &mtop);
  if(iret!=0){
    printf("Unable to rewind tape %s, iret=%d.\n",vol,iret);
    exit(errcode);
  }

  close(mtst.fd);

  *nret=0;
  mtst.fd=-10;
  mtst.vol=NULL;
  mtst.lpos=0;
  mtst.nread=0;
  mtst.action=NULL;
  mtst.device=NULL;
  mtst.recfm=NULL;
  mtst.type=1;
}  

