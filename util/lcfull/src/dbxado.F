C***********************************************************************
C*
C* -----------------------------------------------=====
C*  Subroutine DBxADO(NCMD, OPRNAM, TYPE, NADR  , NRET)
C* -----------------------------------------------=====
C*
C*(Function)
C*    Add new opernad.
C*
C*(Input)
C*    NCMD   ; Pointer to the assigned command.
C*    OPRNAM ; Operand name. Character*16
C*    TYPE   ; Type of operand.
C*    NADR   ; Address where value is set.
C*
C*(Output)
C*    NNRET  : Return code.
C*            negative in case of error.
C*
C*(Author)
C*    A. Miyamoto    Jun-13-1986  Original version.
C*    A. Miyamoto    Mar-25-1995  Add S* TYPE for lower case option.
C*
C***********************************************************************
C
      SUBROUTINE DBxADO(NCMD, OPRNAM, TYPE,  NADR , NRET)
C
      CHARACTER*16    OPRNAM
      CHARACTER*(*)   TYPE
C
#include "dbcmdb.inc"
C
C------< Entry Point >--------------------------------------------------
C
C (1) Search last command defined for the command.
C
      IOPR = NDBCTL(2,NCMD)
      KOPR = NCMD
100   CONTINUE
      IF(IOPR.LE.0) THEN
        IF(NUMOPR.GE.MXxOPR) THEN
          PRINT *,'%Error DBxADO  Going to define too many operand.'
          PRINT *,' Buffer size for operand is defined as ',MXxOPR
          GO TO 990
        ENDIF
        NUMOPR = NUMOPR + 1
        IOPR   = MXxCMD + NUMOPR
        GO TO 200
      ENDIF
      IF(DBCMDN(IOPR).EQ.OPRNAM) THEN
        PRINT *,'%Error DBxADO .. failed to define new operand for the',
     .          ' command ',DBCMDN(NCMD)
        PRINT *,'                 Duplicate operand ',OPRNAM
        GO TO 990
      ENDIF
      KOPR = IOPR
      IOPR = NDBCTL(2,IOPR)
      GO TO 100
C
C (2) Add information of the operand.
C
200   CONTINUE
      NDBCTL(2,KOPR) = IOPR
      DBCMDN(IOPR)   = OPRNAM
      NDBCTL(1,IOPR) = 0
      NDBCTL(2,IOPR) = 0
      LTYP = LEN(TYPE)
      IF(TYPE(1:1).EQ.'I') THEN
        NDBCTL(3,IOPR) = -1
      ELSEIF(TYPE(1:1).EQ.'R') THEN
        NDBCTL(3,IOPR) = -2
      ELSEIF(TYPE(1:1).EQ.'F') THEN
        NDBCTL(3,IOPR) = -3
      ELSEIF(TYPE(1:1).EQ.' ') THEN
        NDBCTL(3,IOPR) = -4
      ELSEIF(TYPE(1:1).EQ.'C') THEN
        READ(TYPE(3:LTYP),*) NDBCTL(3,IOPR)
      ELSEIF(TYPE(1:1).EQ.'S') THEN
        READ(TYPE(3:LTYP),*) ITEMP
        NDBCTL(3,IOPR) = -1*MAX(10, ITEMP)
      ELSE
        PRINT *,'%Error DBxADO .. Invalid operand type ',TYPE(:LTYP),
     .          ' specified.'
        GO TO 990
      ENDIF
C
      NDBCTL(4,IOPR) = NADR
      NRET = 0
      RETURN
C
C (9) Error return.
C
990   CONTINUE
      NRET = -1
      RETURN
      END
