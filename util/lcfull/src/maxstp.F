C**********************************************************************
C*
C* -----------------
C* Subroutine MAxSTP
C* -----------------
C*
C*(Function)
C*   Write Time stamp data of the data modification on the data base.
C*
C*(Author)
C*
C*  A. Miyamoto.  18-Mar-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MAxSTP
C
#include "macntl.inc"
#include "mames.inc"
C
      INTEGER*4    IBUF(6)
C
C
C============< Entry Point >============================================
C
C-----------------------------------------------------------------------
C (1) Write time stamp.
C-----------------------------------------------------------------------
C
#ifdef  UNIX
      IR = 1
#endif
#ifdef  UNIX
      IF( IR.EQ.1 ) THEN
#endif
#ifdef  UNIX
       PRINT *,'MAxSTP .. THis routine is not supported on UNIX.'
#endif
#ifdef  UNIX
        STOP
#endif
#ifdef  UNIX
      ENDIF
#endif
      CALL UIDATE(IBUF(1))
#ifdef  MSP
      CALL   TIME(IBUF(2))
#endif
      READ(CMxJBN,'(4A4)') (IBUF(K1),K1=3,6)
      NW      = 6
#ifdef  MSP
      REWRITE(MCxDBS,ERR=910)CMxDLM(:MCxKEY),NW,(IBUF(K1),K1=1,NW)
#endif
      RETURN
C
910   CONTINUE
      MESAGE(1) = 'Failed to write time stamp data.'
      CALL MAxERR(800, 'MAxSTP', 1)
      STOP
      END
