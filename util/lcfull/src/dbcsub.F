COPTIONS @OPTIONS  NODEBUG
C***********************************************************************
C*
C* ----------------------------------=====
C*  Subroutine DBCSUB(CMDNAM, /MADR/, NRET)
C* ----------------------------------=====
C*
C*(Function)
C*    Define subroutine to be called when command is entered.
C*
C*(Input)
C*    CMDNAM   ; command name to be defined.
C*    /MADR/   ; Subroutine address .
C*
C*(Output)
C*    NRET  : Return code.
C*            negative in case of error.
C*
C*(Author)
C*    A. Miyamoto    Jun-16-1986  Original version.
C*
C***********************************************************************
C
#ifdef  MSP
      SUBROUTINE DBCSUB(CMDNAM, /MADR/, NRET)
#endif
#ifdef  UNIX
      SUBROUTINE DBCSUB(CMDNAM, MADR, NRET)
#endif
C
      CHARACTER*16    WCMDNM
      CHARACTER*(*)   CMDNAM
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
C (2) Store subroutine address.
C
      IF(NDBCTL(4,ICMD).NE.0) THEN
        PRINT *,'%Error DBCSUB .. Subroutine is already assigned to ',
     .          ' the command ',CMDNAM(:LCMD)
        PRINT *,'                 This call will be neglected.'
        GO TO 990
      ENDIF
#ifdef  MSP
      NDBCTL(4,ICMD) = MADR
      CALL DBCSEX(']'//WCMDNM(:LCMD),NDBCTL(4,ICMD))
#endif
C
#ifdef  SUN
       NDBCTL(4,ICMD) = LOC(MADR)
       CALL DBCSEX(']'//WCMDNM(:LCMD),%VAL(NDBCTL(4,ICMD)))
#endif
#if defined(HP) || defined(HIUXF77) || defined(MACHTEN_PPC)
        NDBCTL(4,ICMD) = %LOC(MADR)
        CALL DBCSEX(']'//WCMDNM(:LCMD),%VAL(NDBCTL(4,ICMD)))
#endif
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
