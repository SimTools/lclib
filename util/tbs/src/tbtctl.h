/**********
**  Tape control blocks. 
**********/
#include "tbthed.h"

union hblock vhdr,fhdr;

struct mt_info {
  int  fd;
  char *vol;
  int  lpos;   /*  file position */
  int  nread;  /*  Number of read */
  char *action;
  char *device;
  char *recfm;
  int   lrecl;
  int   type;  /*(0=Created by tar+dd, 1=Created by RMTL+TBS) */
  int   nxt;
  int   lst;
  int   bdw;
} ;

/* extern struct mt_info 
**mtst ={-10, NULL, 0, 0, NULL, NULL, NULL, 0, 1, NULL,0,0} ;
*/

#define TEMPSIZE 1048576
#define VBSBSIZE 23476
#define VBSBCNT  TEMPSIZE/VBSBSIZE
#define VBSRSIZE VBSBCNT*VBSBSIZE



