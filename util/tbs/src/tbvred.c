/*****************************************************
**  To read VBS data created on FACOM MSP
**
**  Coded by A.Miyamoto  15-Feb-1994
**
******************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>


struct comtype {
  FILE *instream[2];
  int fd[2];
  long doread;
};
struct comtype vread_cntl_;

/*************************************************
**  Function prototypes 
*************************************************/
void tbvred_(); 
/********************************************
**
**  Read 1 VBS record 
**  nw and readbuf corresponds to the following fortran read.
**  READ(n) NW, (READBUF(K),K=1,NW)
**
**  meaning of NW
**    NW = 0  ; Read end-of-file.
**    NW = -1 ; Data block size is not 23476.
**    1<= NW <= MAXBUF  ; Read sucessfull
**    NW > MAXBUF  ; Data exceeds buffer size, data is trancated.
**
*********************************************/
void tbvred_(maxbuf, nw, readbuf)
long *maxbuf;
long *nw;
long *readbuf;
{

   static long tempbuf[32760];
   static long lread, lw;
   
   short int bdw, sdw, seg, sdw2;
   short int iend;
   short int itran;
/*   printf("\n\nstart tbvred...doread =%d \n",vread_cntl_.doread);*/
   iend = 0;
   *nw  = 0;
   itran = 0;
   
   while(iend==0){
/*
**  If no data in the buffer, get next block.
*/    
     if( vread_cntl_.doread <= 0 ){
/*       printf("going to call fread...\n"); */
       lread = fread(&(tempbuf[0]), 1, 23476, 
		     vread_cntl_.instream[1]); 
/*       lread = read(vread_cntl_.fd[1], &(tempbuf[0]),23476 ), */
/*       printf(" vread...lread=%d \n",lread); */

       if( lread == 0 ) {
         *nw = 0; 
         break;
       }

/*     printf(" Fread ... lread =%d \n",lread); */
/*     printf(" tempbuf[0]=%x \n",tempbuf[0]);  */
       bdw = tempbuf[0] >> 16 ;
     
       if( bdw > 23476){
         printf("Error:Block size is greater than 23476..BDW=%d \n",bdw);
         *nw = -1;
         return;
       }
       vread_cntl_.doread=1;
     }
     sdw = tempbuf[vread_cntl_.doread] >> 16 ;
     seg = tempbuf[vread_cntl_.doread] << 16 >> 16 ;

     sdw  = sdw - 4;
/* Do not copy first word of a record.  */
     if( seg == 0x0100 || seg == 0 ){ 
       sdw = sdw - 4;
       vread_cntl_.doread = vread_cntl_.doread + 1;
     }
     sdw2 = sdw >> 2;
     if( *nw + sdw2 > *maxbuf ) {
       itran = 1;
       if( *maxbuf - *nw > 0 ) 
	 memcpy(&readbuf[*nw], &tempbuf[vread_cntl_.doread+1], 4*(*maxbuf-*nw) );
     }
     if( itran == 0 ) memcpy(&readbuf[*nw], &tempbuf[vread_cntl_.doread+1], sdw);
     *nw = *nw + sdw2 ;
     vread_cntl_.doread = vread_cntl_.doread + sdw2  + 1;
/*     printf(" doread =%d ...\n",doread); */
     if( vread_cntl_.doread >= lread/4 ) vread_cntl_.doread = 0;
     
     if( seg == 0 ) break;
     if( seg == 0x0200 ) break ;

   }


/*   printf(" End tbvred..nw =%d ... doread=%d \n",*nw,vread_cntl_.doread); */
/*   printf(" End of vbsread ...\n");   */
   return;
}

