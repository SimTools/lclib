COPTIONS @OPTIONS  NODEBUG
C***********************************************************************
C*
C* -------------------------------------------------=====
C*  Subroutine DBCOPR(CMDNAM, OPRNAM, TYPE, /NVAR/, NRET)
C* -------------------------------------------------=====
C*
C*(Function)
C*    Define command and operand.
C*
C*(Input)
C*    CMDNAM ; command name to be defined.
C*    OPRNAM ; Operand name. Character*16, Blank to define only command
C*    TYPE   ; Type of operand.
C*    /NVAR/ ; Address where value is set.
C*
C*(Output)
C*    NNRET  : Return code.
C*            negative in case of error.
C*
C*(Author)
C*    A. Miyamoto    Jun-13-1986  Original version.
C*
C***********************************************************************
C
#ifdef  MSP
      SUBROUTINE DBCOPR(CMDNAM, OPRNAM, TYPE, /NVAR/, NRET)
#endif
#ifdef  UNIX
      SUBROUTINE DBCOPR(CMDNAM, OPRNAM, TYPE, NVAR, NRET)
#endif
C
      CHARACTER*16    WCMDNM, WOPRNM
      CHARACTER*(*)   TYPE, OPRNAM, CMDNAM
C
#include "dbcmdb.inc"
C
C------< Entry Point >--------------------------------------------------
C
C (1) Check whether same command is exist or not.
C
      LCMD   = LEN(CMDNAM)
      WCMDNM = CMDNAM
      CALL UCUPER(LCMD,WCMDNM)
      CALL DBxFDC(WCMDNM, ICMD)
      IF(ICMD.LE.0) THEN
        CALL DBxADC(WCMDNM, ICMD)
        IF(ICMD.LE.0) GO TO 990
      ENDIF
C
C (2) Define operand.
C
      LOPR   = LEN(OPRNAM)
      WOPRNM = OPRNAM
      CALL UCUPER(LOPR, WOPRNM)
      IADR   = NUADDR(NVAR)
      CALL DBxADO(ICMD, WOPRNM, TYPE, IADR, IRET)
      IF(IRET.LT.0) GO TO 990
C
C (3) Normal return.
C
900   CONTINUE
      NRET = 0
      RETURN
C
C
C
990   CONTINUE
      NRET = -1
      RETURN
      END
