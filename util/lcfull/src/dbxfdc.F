C***********************************************************************
C*
C* --------------------------=====
C*  Subroutine DBxFDC(CMDNAM, NCMD)
C* --------------------------=====
C*
C*(Function)
C*    Return pointer to the command matched with the geven name
C*
C*(Input)
C*    CMDNAM ; command name.
C*
C*(Output)
C*    NCMD   : Pointer to the command.
C*            negative if not found.
C*
C*(Author)
C*    A. Miyamoto    Jun-13-1986  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DBxFDC(CMDNAM, NCMD)
C
      CHARACTER*(*)   CMDNAM
C
#include "dbcmdb.inc"
C
C------< Entry Point >--------------------------------------------------
C
      LCMD = LEN(CMDNAM)
      NCMD = -1
      DO 100 ICMD = 1, NUMCMD
        IF(DBCMDN(ICMD)(:LCMD).EQ.CMDNAM(:LCMD)) GO TO 200
100   CONTINUE
      RETURN
200   CONTINUE
      NCMD = ICMD
      RETURN
      END
