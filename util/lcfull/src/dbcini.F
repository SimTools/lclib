C***********************************************************************
C*
C* --------------------------------
C*  Subroutine DBCINI(NOCMD, NOOPR)
C* -------------------------------
C*
C*(Function)
C*    Declare total # of command and operand.
C*
C*(Author)
C*    A. Miyamoto    Jun-13-1986  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DBCINI(NOCMD, NOOPR)
C
#include "dbcmdb.inc"
C
C
C------< Entry Point >--------------------------------------------------
C
      NUMCMD = 0
      NUMOPR = 0
      IF( NOCMD+NOOPR .GT. NCMDxB ) THEN
        PRINT *,'%Fatal error in DBCINI ... too many buffer are ',
     >          ' requested.'
        STOP
      ENDIF
C
      MXxCMD = NOCMD
      MXxOPR = NOOPR
C
      RETURN
      END
