C**********************************************************************
C*
C* --------------------------------------
C* Subroutine MAMSG(LEVEL, NLINE, MESAGE)
C* --------------------------------------
C*
C*(Function)
C*   Printout message to sysout.
C*
C*(Input)
C*   LEVEL  ; Message level.
C*   NLINE  ; # of output lines.
C*   MESAGE ; Message string.
C*
C*(Author)
C*
C*  A. MIYAMOTO.   9-JAN-1987   ORIGINAL VERSION.
C*
C**********************************************************************
C*
      SUBROUTINE MAMSG(LEVEL, NLINE, MESAGE)
C
#include "macntl.inc"
C
      CHARACTER*(*) MESAGE(NLINE)
C
C============< ENTRY POINT >============================================
C
C ----------------------------------------------------------------------
C (1) Switch accroding to the message level.
C ----------------------------------------------------------------------
C
      IF(LEVEL.GT.MCxMSL) RETURN
 
C
C ----------------------------------------------------------------------
C (2) Printout message.
C ----------------------------------------------------------------------
C
      LMSG = LEN(MESAGE(1))
      DO 200 IL = 1, NLINE
        DO 210 IWRT = LMSG, 1, -1
          IF(MESAGE(IL)(IWRT:IWRT).NE.' ') GO TO 220
210     CONTINUE
        IWRT = 1
220     CONTINUE
        WRITE(MCxMPT,'(A)') MESAGE(IL)(1:IWRT)
200   CONTINUE
C
      IF(LEVEL.EQ.0) THEN
        CALL ERRTRA
        WRITE(MCxMPT,'(A)') ' Job abnormal end at Sub. MAMSG '
        STOP
      ENDIF
C
      RETURN
      END
