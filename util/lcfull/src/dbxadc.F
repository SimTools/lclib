C***********************************************************************
C*
C* --------------------------=====
C*  Subroutine DBxADC(CMDNAM, NCMD)
C* --------------------------=====
C*
C*(Function)
C*    Add new command.
C*
C*(Input)
C*    CMDNAM ; command name, character*16 .
C*
C*(Output)
C*    NCMD   : Pointer to new command.
C*            negative in case of error.
C*
C*(Author)
C*    A. Miyamoto    Jun-13-1986  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DBxADC(CMDNAM, NCMD)
C
      CHARACTER*16    CMDNAM
C
#include "dbcmdb.inc"
C
C------< Entry Point >--------------------------------------------------
C
      IF(NUMCMD.GE.MXxCMD) GO TO 990
      NUMCMD = NUMCMD + 1
      DBCMDN(NUMCMD) = CMDNAM
      NDBCTL(1,NUMCMD) = 0
      NDBCTL(2,NUMCMD) = 0
      NDBCTL(3,NUMCMD) = 0
      NDBCTL(4,NUMCMD) = 0
      NCMD = NUMCMD
      RETURN
C
C
C
990   CONTINUE
      PRINT *,'%Error DBxADC .. Failed to define command ',CMDNAM
      PRINT *,' since the size of command name buffer exceeds ',MXxCMD
      NCMD = -1
      RETURN
      END
