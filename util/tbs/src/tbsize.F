C *********************************************************************C
C*                                                                     *
C*======================================                               *
C* Subroutine TBSIZE( /NSIZE/, /NWORK/ )                               *
C*======================================                               *
C*(Purpose)                                                            *
C*   Initialize the TRISTAN_BANK_SYSTEM for Program Start_up.          *
C*(Input)                                                              *
C*   NSIZE    : Size of the Data_Buffer (DB)                           *
C*(Used COMMON)                                                        *
C*  /TBSBNK/    For detail information, see TBALOC.                    *
C*  /TBSPAS/    Table of allocated unit numbers for passed_Records.    *
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85   Version 3.0                      *
C*       S.Kawabata     Jan. 13 '86   Version 4.0                      *
C*                                                                     *
C **********************************************************************
 
#ifdef  MSP
      SUBROUTINE TBSIZE( NSIZE, /NWORK/ )
#endif
#ifdef  UNIX
      SUBROUTINE TBSIZE( NSIZE )
#endif
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbspas.inc"
#include "sswork.inc"
#include "tbssum.inc"
 
#ifdef  UNIX
      NUMARG = 1
#endif
#ifdef  UNIX
      GO TO 9900
#endif
#ifdef  UNIX
      ENTRY TBSIZEx(NSIZE,  NWORK)
#endif
#ifdef  UNIX
      NUMARG = 2
#endif
#ifdef  UNIX
9900  CONTINUE
#endif
#ifdef  MSP
        NUMARG = NARG(0)
#endif
C  -----------------------------------------------------------------
C     Initialization of Contents of COMMON /TBSBNK/
C  -----------------------------------------------------------------
C  ------------------ Set Data_Buffer Size -------------------
 
         HSIZE    = NSIZE*2
         MAXSIZ   = HSIZE
         NWFREE   = HSIZE
 
         MXWORK   = MWORK
        IF( ( NUMARG  .GE. 2 ) .AND.
     .       ( NWORK   .GT. MXWORK) ) MXWORK = NWORK
 
C  ---------------- Set the default message printing level.  -
C
         CALL TBWARN(0)
         CALL TBLUNI(6)
C
C  ---------------- Clear the number of DSBs' ----------------
C
         NOSUB    = 0
         NOSHR    = 0
         NOPAS    = 0
C
C  ---------------- Clear statistical parameters -------------
C
         NTREAD   = 0
         NREDER   = 0
         NOPASS   = 0
         NOSKIP   = 0
         NTWRIT   = 0
C
         RETURN
C
         END
