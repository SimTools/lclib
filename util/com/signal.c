/*DECK ID>, SIGNALF. */
/*>    ROUTINE SIGNALF
  CERN PROGLIB#         SIGNALF         .VERSION KERNFOR  4.26  910313
  ORIG. 12/03/91, JZ
  FORTRAN interface routine to signal
 
      INTEGER FUNCTION SIGNALF (NUMSIG,PROC,IFLAG)
 
C-        NUMSIG :  signal number
C-          PROC :  external of the handler, if IFLAG = -1
C-         IFLAG :  < 0  instal PROC
C-                  = 0  default action
C-                  = 1  ignore signal
C-                  > 1  adr of handler as returned earlier
C-        function value = adr of previous handler
*/
#include <stdio.h>
#include <signal.h>
#include <errno.h>
int signal_(signum,funct,flag)
      int  *signum, *flag;
      int  *funct;
{
      int  signo, istat;
      int  handler;
      void *oldhand;
 
 
      signo = *signum;
/*    printf (" entry signalf fu=%x *fu=%x fl=%x *fl=%x\n",
                              funct, *funct, flag, *flag);  */
 
      if (*flag < 0)          handler = (int)funct;
        else if (*flag == 0)  handler = (int)SIG_DFL;
        else if (*flag == 1)  handler = (int)SIG_IGN;
        else                  handler = *flag;
 
/*    printf (" handler = %x\n", handler);   */
 
      oldhand = signal(signo,handler);
      istat   = (int)oldhand;
      if (istat >= 0)        return istat;
      return -errno;
}
/*> END <----------------------------------------------------------*/
