CC*********************************************************************C
C*
C*====================
C* Subroutine DBCRED(INPUT, NRET)
C*====================
C*
C*(Purpose)
C*    Interprete input command.
C*
C*(Input)
C*    INPUT  ; Input string.
C*
C*(Output)
C*    NRET   ; Return code.
C*         > 0 ; when expecting continuation card.
C*         = 0 ; for normal return.
C*         < 0 ; in case of error.
C*
C*(Author)
C*    A. Miyamoto   13-Jun-1986   Original version.
C*
CC**********************************************************************
C
      SUBROUTINE DBCRED(INPUT, NRET)
C
      CHARACTER*(*) INPUT
      CHARACTER     WRK*80, CMDNAM*16, BLANK*72
      CHARACTER     INPSTK*400
C
      DATA    LSTWRD/1/
      DATA    BLANK/' '/
C
#include "dbcmdb.inc"
C
C
C=======< Entry Point >=================================================
C
C (1) Remove comment line and inline comment, and get valid command name
C
      NRET = 0
      LINP = LEN(INPUT)
C
C (1.1) If first character is *, it is a comment line.
C       ] for inline comments.
C
      IF(INPUT(1:1).EQ.'*')          GO TO 900
      IF(INPUT(1:72).EQ.BLANK(1:72)) GO TO 900
      LAST   = INDEX(INPUT(:LINP),']') - 1
      IF(LAST.EQ.0)       GO TO 900
      IF(LAST.EQ.-1) LAST = LINP
C
C (1.2) Convert to the upper case, and fill into command stack
C       till found not-continuation card.
C
      CALL UCSTRP(INPUT(:LAST),' ',WRK,LWRK)
 
      ISTWRD = LSTWRD + LWRK
      IF(ISTWRD.GT.400) THEN
        PRINT *,'%Error in DBCRED .. Too many continuation, overflow',
     .          ' command stack.'
        GO TO 990
      ENDIF
      IF(WRK(LWRK:LWRK).NE.'+') THEN
        INPSTK(LSTWRD:ISTWRD) = WRK(:LWRK)
        LSTWRD = ISTWRD
      ELSE
        IF(LWRK.GT.1) THEN
          INPSTK(LSTWRD:ISTWRD) = WRK(:LWRK-1)
        ENDIF
        LSTWRD = ISTWRD - 1
        NRET = 1
        GO TO 900
      ENDIF
C
C ----------------------------------------------------------------------
C (2) Obtain command name.
C ----------------------------------------------------------------------
C
      LCMD   = INDEX(INPSTK(:LSTWRD),' ') - 1
      IF(LCMD.GT.16.OR.LCMD.EQ.-1) THEN
        PRINT *,'%Error .. DBCRED Invalid command format.'
        GO TO 990
      ENDIF
      CMDNAM = INPSTK(:LCMD)
      CALL UCUPER( 16, CMDNAM )
CTAYM950325        IF( INDEX(CMDNAM,'INPUT').EQ.0) CALL UCUPER(LCMD, INPSTK)
      CALL DBxFDC(CMDNAM,ICMD)
      IF(ICMD.LE.0) THEN
        PRINT *,'%Error DBCRED .. Command ',CMDNAM(:LCMD),' not found.'
        GO TO 990
      ENDIF
C
C ----------------------------------------------------------------------
C (3) Interprete input command.
C ----------------------------------------------------------------------
C
      IF(LCMD+2.GT.LSTWRD) THEN
        PRINT *,'%Error DBCRED .. no operand specified to the ',
     .          'command ',CMDNAM(:LCMD)
        GO TO 990
      ENDIF
      CALL DBCANL(ICMD,INPSTK(LCMD+2:LSTWRD),IRET)
      IF(IRET.LT.0) THEN
        PRINT *,'%Error DBCRED .. Failed to interprete operand for ',
     .          'command ',CMDNAM(:LCMD)
        GO TO 990
      ENDIF
C
C ----------------------------------------------------------------------
C (4) Call subroutine assigned to the command.
C ----------------------------------------------------------------------
C
      IF(NDBCTL(4,ICMD).NE.0) THEN
#ifdef  MSP
       CALL DBCSEX(CMDNAM, NDBCTL(4,ICMD))
#endif
#ifdef  UNIX
#ifndef __GFORTRAN__
       CALL DBCSEX(CMDNAM, %VAL(NDBCTL(4,ICMD)))
#else
       CALL cdbcsex(cmdnam, ndbctl(4,icmd))
#endif
#endif
      ENDIF
C
C ----------------------------------------------------------------------
C (9) Return.
C ----------------------------------------------------------------------
C
C (9.1) Normal return
C
      LSTWRD = 1
      NRET   = 0
900   CONTINUE
      RETURN
C
C (9.2) Error return.
C
990   CONTINUE
      PRINT *,' Error input was'
      PRINT *,INPUT(:LINP)
      NRET = -1
      RETURN
      END
