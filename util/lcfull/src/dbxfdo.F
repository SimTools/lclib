C***********************************************************************
C*
C* ---------------------------------====
C*  Subroutine DBxFDO(NCMD, OPRNAM, NOPR)
C* ---------------------------------====
C*
C*(Function)
C*    Find operand matched with the specified operand name.
C*
C*(Input)
C*    NCMD   ; Pointer to the command name.
C*    OPERND ; OPernamd name.
C*
C*(Output)
C*    NOPR   : Pointer to the operand.
C*            negative if not found.
C*
C*(Author)
C*    A. Miyamoto    Jun-13-1986  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DBxFDO(NCMD, OPRNAM, NOPR)
C
      CHARACTER*(*)   OPRNAM
C
#include "dbcmdb.inc"
C
C------< Entry Point >--------------------------------------------------
C
      LOPR = LEN(OPRNAM)
      NOPR = -1
      IPNT = NDBCTL(2,NCMD)
100   CONTINUE
      IF(IPNT.LE.0) GO TO 900
      IF(DBCMDN(IPNT)(:LOPR).EQ.OPRNAM(:LOPR)) THEN
        NOPR = IPNT
        GO TO 900
      ENDIF
      IPNT = NDBCTL(2,IPNT)
      GO TO 100
C
C
C
900   CONTINUE
      RETURN
      END
