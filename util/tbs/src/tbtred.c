
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

#include <errno.h>

struct mt_info mtst;

/*******************************
** 
**  Subroutine TBTRED_(MAXBUF, NW, READBUF )
**(Function)
**  Read data from MT and copy one record to READBUF
**(Input)
**  MAXBUF  ; Maxmum size of READBUF
**(Output)
**  NW ; Number of words being read.
**  READBUF ; read data.
**    NW= 0 for read end-of-file
**      > MAXBUF ; when data exceeds buffer size, data is trancated. 
**
**(Author)
**   A. Miyamoto  6-June-1994
**  
********************************/
  
void tbtred_( maxbuf, nw, readbuf )
long *maxbuf, *nw, *readbuf;
{
#define VBSWSIZE VBSBSIZE/4

  extern struct mt_info mtst;

  int errcode=16;

  int nbyte, lw ;
  int lcpy, ipnt, icpy, iret;
  int sdw, seg;
  int nread;
  int idebug=0;

  int vbsbsize=VBSBSIZE;
  int vbsrsize=VBSRSIZE;
  int vbsbcnt=VBSBCNT;
 
  int bdw;
  static int nbgn,nend,kbgn,klst,knxt,k0;
  static int ncall=0;

  static union tempbuf {
    char *addr;
    int *tmpbuf;
  } buf;

/* Allocate temporary buffer */
/*
  printf("\n\n\n\n");
  printf(" Start tbtred .. maxbuf=%d\n",*maxbuf);
  printf(" mtst.nxt=%d",mtst.nxt);
  printf("kbgn =%d klst=%d knxt=%d k0=%d \n",kbgn,klst,knxt,k0);
*/

  if(ncall==0){
    buf.addr=malloc(TEMPSIZE);
    if(buf.addr==NULL){
      printf("TBTRED .. Unable to allocate temporary data buffer.\n");
      exit(16);
    }
    ncall++;
/*    printf(" Allocat temp buf .. mtst.buf.addr=%d",buf.addr);  */
  }

/* Copy data to readbuf */
/*
  printf(" At the begining of tbtred ..device =%s",mtst.device);
  printf(" type is %d\n",mtst.type);
*/
  if(mtst.type==0) goto vbsfmt;

  lcpy=0;
  ipnt=0;
  for(;;){
    if(mtst.nxt<0){
      mtst.lst=read(mtst.fd, buf.addr, TEMPSIZE )/4;  
      mtst.nread=mtst.nread+1;
/*
      printf(" read one block .. mtst.lst=%d, mtst.read=%d ",
	     mtst.lst, mtst.nread);
      printf(" tempbuf[0-3]=%d %d %d %d \n",
	     buf.tmpbuf[0], buf.tmpbuf[1],
	     buf.tmpbuf[2], buf.tmpbuf[3]); 
*/    
      if(mtst.lst==0){
	     *nw=0;
	     mtst.nread=-1;
	     break;
      }
      mtst.nxt=0;
    }
    if(ipnt==0){
      *nw=buf.tmpbuf[mtst.nxt];
/*      printf(" nw =%d \n ",*nw); */
      mtst.nxt=mtst.nxt+1;
      lcpy=*nw;
      if(*nw > *maxbuf){
        printf("TBTRED .. Data size(%d) exceeds ",*nw);
        printf(" buffer size(%d).\n",*maxbuf);
        lcpy=*maxbuf;
      }
    }
    icpy=lcpy;
/*    printf("before copy .. device=%s\n",mtst.device); */
    if( (mtst.nxt+lcpy) > mtst.lst ) icpy=mtst.lst-mtst.nxt;
    if(ipnt<*maxbuf) memcpy(&readbuf[ipnt],&buf.tmpbuf[mtst.nxt], 4*icpy); 
/*    printf("after copy device=%s\n",mtst.device); */
    mtst.nxt=mtst.nxt+icpy;
    if(mtst.nxt>=mtst.lst) mtst.nxt=-1;
    ipnt=ipnt+icpy;
    lcpy=lcpy-icpy;
    if(lcpy<=0) break;
  }
  return;

/*
**  Special treatment for vbs format.
*/

vbsfmt:  
  if(mtst.nread==1){
    kbgn=0;
    klst=0;
    nbgn=0;
    sscanf(fhdr.dbuf.size,"%o",&nend);
    nend=nend/VBSBSIZE;
/*    printf(" Read from nbgn=%d to nend=%d \n",nbgn,nend); */
/*    printf("TBTMNT .. Going to read %d VBS blocks.\n",nend); */
  }

  lcpy=0;
  ipnt=-100;
  *nw=0;
/*
  nread=VBSRSIZE;
  printf("in TBTRED .. Read size is %d.\n",nread);
  printf(" with mtst.nxt=%d",mtst.nxt);
  printf(" mtst.lst=%d\n",mtst.lst);
  nread=VBSWSIZE;
  printf(" VBSWSIZE=%d ",nread);
*/
  for(;;){
/*
    printf("kbgn =%d klst=%d knxt=%d k0=%d \n",kbgn,klst,knxt,k0);
*/
    if( kbgn>=klst ){
/*      printf(" nbgn=%d nend=%d .\n",nbgn,nend); */
      klst=read(mtst.fd, buf.addr, VBSRSIZE )/4;
      kbgn=0;
      mtst.nread=mtst.nread+1;
/*      printf("klst=%d mst.nread=%d \n",klst,mtst.nread); */

      if(klst==0){
	printf(" klst was 0\n");
	*nw=0;
	break;
      }
/*
      printf(" read one block .. klst=%d, mtst.read=%d ",
	     klst, mtst.nread);
      printf(" tempbuf[0-3]=%x %x %x %x \n",
	     buf.tmpbuf[0], buf.tmpbuf[1],
	     buf.tmpbuf[2], buf.tmpbuf[3]);
*/
    }
/*
    printf("kbgn=%d",kbgn);
    printf(" Buff=%x %x %x %x \n",buf.tmpbuf[kbgn],
	   buf.tmpbuf[kbgn+1], buf.tmpbuf[kbgn+2],
	   buf.tmpbuf[kbgn+3]);
*/
    lw=kbgn%5869;
/*
    printf(" lw =%d \n",lw);
*/
    if(lw==0) {
      nbgn++;
      if(nbgn>nend){
	printf("In TBTRED .. Detect end of file.\n");
/*	printf(" nbgn(%d) exceeds nend(%d) \n",nbgn, nend); /*
/*	idebug=1; */
	*nw=0;
	break;
      }
      bdw=buf.tmpbuf[kbgn]>>16;
      knxt=kbgn+bdw/4;
      k0=kbgn+(vbsbsize/4);
      kbgn++;
/*
      if(bdw==23476) idebug=0;
      else idebug=1;
*/
/*      printf(" kbgn=%d  bdw=%d \n",kbgn,bdw); */
    }

    sdw=buf.tmpbuf[kbgn]>>16;
/*    printf("sdw=%d ",sdw); */
    seg=buf.tmpbuf[kbgn]<<16 >>16; 
/*    printf("seg=%x ",seg); */
    kbgn++;
/*    printf("kbgn=%d \n",kbgn); */
    
    lcpy=sdw/4-1;

    if(ipnt<0){
        *nw=buf.tmpbuf[kbgn];
	kbgn++;
	lcpy=lcpy-1;
	ipnt=0;
      }
/*    printf(" nw =%d \n ",*nw);  */

    icpy=lcpy;
/*
    printf("kbgn=%d ",kbgn);
    printf("Going to copy %d words.\n",icpy);
    printf("Before copy deivce=%s",mtst.device);
*/
    if( (ipnt+icpy) <= *maxbuf ){
      memcpy(&readbuf[ipnt],&buf.tmpbuf[kbgn], 4*icpy); 
      ipnt=ipnt+icpy;
    }
    else {
      lw=*maxbuf-ipnt;
      if(lw>0)  memcpy(&readbuf[ipnt],&buf.tmpbuf[kbgn], 4*lw); 
      ipnt=ipnt+lw;
    }
/*    printf("After copy deivce=%s \n",mtst.device); */

    kbgn=kbgn+icpy;
    if(kbgn>=knxt) kbgn=k0;

    if(seg==0 || seg==0x0200) break;

  }

  if(idebug==1){
    printf("Read buf[0-4]=%d %d %d %d %d \n",
	   readbuf[0],readbuf[1],readbuf[2],
	   readbuf[3], readbuf[4]);


    printf(" nw=%d maxbuf=%d\n",*nw, *maxbuf);

    printf("kbgn =%d klst=%d knxt=%d k0=%d \n",kbgn,klst,knxt,k0);
    printf("nbgn=%d nend=%d\n",nbgn,nend);
  }
/*  printf(" at the end of tbtred : mtst.device =%s\n",mtst.device); */
  return;

}

