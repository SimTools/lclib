
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

#include <errno.h>

#include "tbtctl.h"

struct mt_info mtst 
  ={-10, NULL, 0, 0, NULL, NULL, NULL, 0, 1,-100,0, 0 } ;

/********************************************************************
**  Subroutine TBTMNT_(VOL, DEVICE, ACTION, NRET )
**(Function)
**  Mount tape, VOL, to the DEVICE.
**  Read VOLUME label, and check consistency.
**(Input)
**  VOL    ; Tape volume
**  DEVICE ; tape device name.
**  ACTION ; tape device name.
**(Output)
**  NRET   ; = 0 when OK, otherwise <0
**(Author)
**  A. Miyamoto  6-June-1994
*********************************************************************/
void tbtmnt_( vol, devnow, action, nret )
char *vol, *devnow, *action;
int *nret;
{

  extern struct mt_info mtst; 
  static device[100];
  int errcode=16;
  int iret,nbyte;
  struct mtop mtop;
  struct mtget mt_status;
/*
  printf(" In tbtmnt .. vol=%s, device is %s",vol,device);
  printf(" action=%s \n",action);

  printf(" mtst.vol=%s.\n",mtst.vol);

*/
  if(mtst.vol!=NULL){
    printf("In TBTMNT .. Tape %s is alread mounted to %s.\n",
	   mtst.vol,mtst.device);
    return;
/*    exit(16); */
  }
/*
  perror("At the begining of tbtmnt.");
  printf(" strcmp complete.\n");
*/
  strcpy(device, devnow);

  if(strcmp(action,"WRITE")==0){
    mtst.fd=open(device,O_RDWR);
    if(mtst.fd==-1){
      printf("Error in TBTMNT .. unable to open %s.\n",device);
      exit(errcode);
    }
    mtst.action="WRITE";
  }
  else{
    printf(" Will call open .. ");
    mtst.fd=open(device,O_RDONLY);

    if(mtst.fd==-1){
      printf("Error in TBTMNT .. unable to open %s.\n",device);
      exit(errcode);
    }
    mtst.action="READ";
  }

  printf("In TBTMNT .. Tape device is opened..  fd=%d.\n",mtst.fd);

/* First get tape status */
  iret=ioctl(mtst.fd, MTIOCGET, (char *)&mt_status);
  printf("TBTMNT .. Current tape file#=%d, blk#=%d",
         mt_status.mt_fileno, mt_status.mt_blkno);
  printf("  Tape is rewinding.\n");

/*  Rewind */
  mtop.mt_op=MTREW;
  mtop.mt_count=1;
  iret=ioctl(mtst.fd, MTIOCTOP, &mtop);
  if( iret != 0 ) {
    printf("Failed to rewind the tape .\n");
    exit(errcode);
  }

/* Get Volume header. */
    
  nbyte=read(mtst.fd, vhdr.dummy, HEADSIZE);
  if(nbyte<512){
    printf("Error in TBTMNT .. Invalid tape volume header, length(%d)<512.\n",nbyte);
    exit(errcode);
  }

  if(strncmp(vhdr.dbuf.prefix,"VOLL",4)!=0){
    printf("Error in TBTMNT .. This is not a TOPAZ standard tape label.");
    printf(" Assume tar format.\n");
    mtst.type=0;
  }
  if(strcmp(vhdr.dbuf.name,vol)!=0){
    printf("Error in TBTMNT .. Tape %s is mounted, though %s is requested.\n",
	   vhdr.dbuf.name, vol);
    exit(errcode);
  }

/* Skip to the begining of next file */

  mtop.mt_op=MTFSF;
  mtop.mt_count=1;
  iret=ioctl(mtst.fd, MTIOCTOP, &mtop);
  if( iret!=0 ) {
    printf("Error in TBTMNT .. Unable to possition tape at the first file.\n");
    exit(errcode);
  }

  close(mtst.fd);
  mtst.fd=-2;

  mtst.device=device;
  mtst.lpos=1;
  mtst.nread=0;
  mtst.recfm=NULL;
  mtst.lrecl=0;
  mtst.vol=vhdr.dbuf.name;

  bzero(fhdr.dummy,sizeof(fhdr.dummy));
  nret = 0 ;
/*  perror("At the end of tbtmnt ... "); */

  return;
}

