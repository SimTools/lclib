

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
struct mt_info mtst ;

/*******************************
** 
**  Subroutine TBTOPN_(DSN, LPOS, ACTION )
**(Function)
**  Open tape file for subsequent read/write.
**  If opend with WRITE mode, file hdr is written.
**(Input)
**  VOL ; Tape Volume
**  DSN : Tape file name
**  LPOS ; file position in the tape.
**  ACTION ; READ or WRITE
**(Output)
**  none;
**  program abends in case of error.
**
**(Author)
**   A. Miyamoto  6-June-1994
**  
********************************/
  
void tbtopn_( dsn, lpos, action )
char *dsn,*action;
int *lpos;
{
  extern struct mt_info mtst ;

  int errcode=16;
  int iret;
  int myfd,nbyte;
  struct mtop mtop;
  struct mtget mt_status;

  struct tm *now;
  time_t itime;

  char cmd[160];
  int lbuf,iread,sumbyte,i;
  int irun,ifseq, lfseq, lrecl;
  char filename[100];

/*
**  Tape device name, open it then rewind.
**
*/

  printf("In TBTOPN .. dsn=%s, *lpos=%d, action=%s, ",
	 dsn, *lpos, action);
  printf("on device =%s \n",mtst.device);

/*  perror("Perror in tbtopn at the begining .."); */


  if(mtst.vol==NULL){
    printf("Error in TBTOPN .. Tape is not mounted yet.\n");
    exit(errcode);
  }

  if(strcmp(mtst.action,action)!=0){
    printf("Error in TBTOPN .. requested to open as %s, but tape is mounted as %s",
	   action, mtst.action);
    exit(errcode);
  }

/*
**  Open file.
*/
  if( mtst.fd >= 0 ){
    printf("Error in TBTOPN .. Tape file is laready opened.\n");
    exit(16);
  }

  if(strcmp(action,"WRITE")!=0){
    mtst.fd=open(mtst.device,O_RDONLY);
  } else {
    if( (mtst.type!=1) ||
        ((strcmp(mtst.recfm,"V")!=0) && 
	 (strcmp(mtst.recfm,"F")!=0) ) ){
	  printf("MTTOPN .. parameters does not allow write mode open.");
	  printf(" type=%d, recfm=%s.\n",
		 mtst.type, mtst.recfm);
	}
    mtst.fd=open(mtst.device,O_RDWR);
  }
  if(mtst.fd==-1){
    printf("Error in TBTOPN .. Error to open %s with %s.\n",
     mtst.device, mtst.action);
     exit(errcode);
  }

  printf("In TBTOPN .. Open file complete. Positioning in progress. \n");

/*
** move file to the proper space. 
**  (1) If current tape position is larget than requested,
**      rewind, and fsf to the requried position.
**  (2) Current position is smaller than requested,
**      make a Forward space.
*/
/*  printf("mtst.lpos=%d  ",mtst.lpos);
  printf("mtst.nread=%d  ",mtst.nread);
*/

  if(mtst.lpos>*lpos ||
     (mtst.lpos==*lpos && mtst.nread !=0 )){
      mtop.mt_op=MTREW;
      mtop.mt_count=1;
      iret=ioctl(mtst.fd, MTIOCTOP, &mtop);

      mtop.mt_op=MTFSF;
      mtop.mt_count=*lpos;
      if(ioctl(mtst.fd, MTIOCTOP, &mtop)!=0){
	printf("Error in TBTOPN .. error to move tape position, to %d.\n",*lpos);
	close(mtst.fd);
	exit(errcode);
      }
      mtst.lpos=*lpos;
      mtst.nread=0;
  }
  else if(mtst.lpos < *lpos ){
      mtop.mt_op=MTFSF;
      mtop.mt_count=*lpos - mtst.lpos ;
      if(ioctl(mtst.fd, MTIOCTOP, &mtop)!=0){
	printf("Error in TBTOPN .. error to move tape position to %d.\n",
	       *lpos);
	close(mtst.fd);
	exit(errcode);
      }
      mtst.lpos=*lpos;
      mtst.nread=0;
  }

/*  printf(" Moving file properlly.\n"); */

/*
**  Allocate data buffer for subsequent data read/write.
**
*/
/*
  if(mtst.nxt==-100){
    mtst.buf.addr=malloc(TEMPSIZE);
    if(mtst.buf.addr==NULL){
      printf("TBTRED .. Unable to allocate temporary data buffer.\n");
      exit(16);
    }
    mtst.nxt=-1;
    printf(" Allocat temp buf .. mtst.buf.addr=%d\n",mtst.buf.addr); 
  }
*/
/*
**  Read file header for consistency check 
**  or write file header for new file, according to action.
**
*/
  if(strcmp(action,"WRITE")!=0){
    nbyte=read(mtst.fd,fhdr.dummy,HEADSIZE);
    if(nbyte<512){ 
      printf("Error in TBTOPN .. Invalid header file,");
      printf("%d-th file, #byte=%d.\n",*lpos,nbyte);
      exit(errcode);
    }
    if( nbyte>512){
      printf("Error in TBTOPN .. Header file size greater than 512.");
      printf("This file is not supported yet.\n");
      exit(errcode);
    }
    else {
      mtst.nxt=-1;
      mtst.lst=0;
    }
    if(strcmp(fhdr.dbuf.name,dsn)!=0){
      printf("Error in TBTOPN .. Tape file name is %s",fhdr.dbuf.name);
      printf(", but requested to read %s.\n",dsn);
      close(mtst.fd);
      exit(errcode);
    }
    printf("In TBTOPN .. File name check OK.");
    if(mtst.type==1){
      if(strncmp(fhdr.dbuf.prefix,"HDRL",4)!=0){
	printf("\n Error in TBTOPN .. Invalid file header type.");
	printf(" %d-th file, name %s.\n",*lpos,dsn);
        exit(errcode);
      }
      sscanf(fhdr.dbuf.fseq,"%6d ",&i);
      if(i!=*lpos){
        printf("\n Error in TBTOPN .. File position writen on tape(%d)",i);
        printf(" is not what it should be(%d).\n",*lpos);
        exit(errcode);
      }
    } 
    else {
/*
      sscanf(fhdr.dbuf.size,"%o",&mtst.bdw);
      mtst.bdw=mtst.bdw/VBSBSIZE;
      printf("TBTOPN .. Going to read %d VBS blocks.\n",mtst.bdw);
*/
    }
    printf("Read position is set to the file, %s.\n",fhdr.dbuf.name);
  }
  else{
    iret=tbtwfh_(mtst.fd, dsn, mtst.recfm,
		mtst.lrecl, mtst.lpos);
  }

/*
  perror("Perror in tbtopn ..");
  printf("tbtopn .. end of tbtopn.\n");
*/
  mtst.lpos=*lpos;
  mtst.nread=1;

/*  printf(" at the end of tbtopn .. mtst.device =%s\n",mtst.device); */
  
  return;
}

