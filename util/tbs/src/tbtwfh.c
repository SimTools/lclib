/*******************************
** 
**   int tbtwfh(fd,filename,recfm,lrecl,fseq);  
**(function) 
**   Write Volume header
**(Input)
**   device : device name
**   volname : volume name 
**(Author)
**   A. Miyamoto  3-June-1994
**  
********************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

int tbtwfh_(fd, filename, recfm, lrecl, fseq)
int *fd;
char *filename;
char *recfm;
int lrecl;
int fseq;
{
#include "tbthed.h"

  union hblock hdr;

  struct tm *now;
  time_t itime;

  int nbyte;
  int iclos;
  int i,j,lsum;
  char tmp[20];

  printf("Start program.\n");

  printf("myfd =%d\n",fd);

/*
**  Write File HDR label.
*/

  bzero(hdr.dummy,sizeof(hdr.dummy));
  sprintf(hdr.dbuf.name,"%s",filename);
  printf("hdr.dbuf.name=%s\n",hdr.dbuf.name);
  strncpy(hdr.dbuf.mode,"   644 ",7);
  sprintf(hdr.dbuf.uid,"%6d ",getuid());
  sprintf(hdr.dbuf.uid,"%6d ",getgid());
  memcpy(hdr.dbuf.size,"          0  ",13);
  time( &itime );
  sprintf(tmp,"%10o  ",itime);
  memcpy(hdr.dbuf.mtime,tmp,11);
  printf("hdr.dbuf.mtime=%s\n",hdr.dbuf.mtime);
  memcpy(hdr.dbuf.chksum,"          ",8); 
  memcpy(hdr.dbuf.linkflag,"0",1);
  strncpy(hdr.dbuf.labelid,"TBS  ",5);
  sprintf(hdr.dbuf.recfm,"%5s ",recfm);
  printf("hdr.dbuf.labelid=%s\n",hdr.dbuf.labelid);
  sprintf(hdr.dbuf.lrec,"%10d ",lrecl);
  printf("hdr.dbuf.lrec=%s\n",hdr.dbuf.lrec);
  sprintf(hdr.dbuf.fseq,"%6d ",fseq);
  sprintf(hdr.dbuf.uname,"%s",getlogin());
  strncpy(hdr.dbuf.prefix,"HDRL",8);
  printf("hdr.dbuf.prefix=%s\n",hdr.dbuf.prefix);

/*
**  Calculate check sum.
*/

  lsum=0;
  for(i=0;i<512;i++){
   lsum=lsum+hdr.dummy[i];
  }    
  printf("lsum=%d\n",lsum);
  sprintf(hdr.dbuf.chksum,"%6o",lsum);


/*  devnam="tape.dump3"; */
  nbyte=write(fd, hdr.dummy, 512);
  printf("Write hdrl, %d bytes \n",nbyte);
  if( nbyte == 512 ) {
    return (0);
  }
  else{
    return(-1);
  }

}

