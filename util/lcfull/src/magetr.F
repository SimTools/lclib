C**********************************************************************
C*
C* --------------------------------------------============
C* Subroutine MAGETR(MODE, NEXP, NRUN, MXxBUF, NWORDS, NBUF)
C* --------------------------------------------============
C*
C*(Function)
C*   Get run record.
C*
C*(Input)
C*   MODE   ; Data base access mode.
C*        = 0 ; Direct access.
C*        = 1 ; Sequential access.
C*
C*   NEXP   ; Experiment #, if lt 0, read from the first experiment.
C*   NRUN   ; Run #, if eq 0, read from the first run.
C*   MXxBUF ; Size of NBUF.
C*
C*(Output)
C*   NWORDS ; # of data in NBUF.
C*        = -1 when Run not found.
C*        = -2 when read end-of-file.
C*   NBUF   ; Run information array.
C*
C*(Author)
C*
C*  A. Miyamoto.  19-Jan-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MAGETR(MODE, NEXP, NRUN, MXxBUF, NWORDS, NBUF)
C
#include "macntl.inc"
C
      PARAMETER      (MXxLEN=1000)
      INTEGER*4       NBUF(MXxBUF)
      COMMON /MAWORK/ IBUF(MXxLEN)
      CHARACTER       MESAGE*137, KEY*20
      DATA            IORUN/0/, IOEXP/0/
C
C============< Entry Point >=========================================
C
C -------------------------------------------------------------------
C (1) Read data.
C -------------------------------------------------------------------
C
      IVERS = 0
      IW    = 0
      KEY   = ' '
      IF(MODE.EQ.1) GO TO 200
C
C ... Direct access read.
      IF(NEXP.LE.0) THEN
        CALL MAxLOC('                ',0,IRET)
        CALL MAxGET(0, 1, MXxLEN, KEY(:MCxKYL), IVERS,IW, IBUF, IRET)
      ELSE
        WRITE(KEY,'(''E'',I5.5,''.R'')') NEXP
        IF(NRUN.LE.0) THEN
          CALL MAxLOC(KEY(1:8),0, IRET)
          IF(IRET.LT.0) THEN
            WRITE(MESAGE,110) NEXP
110         FORMAT(' %Warning_MAGETR..Experiment #',I5,' not exists.')
            CALL MAMSG(10,1, MESAGE)
            GO TO 800
          ENDIF
          CALL MAxGET(3, 1, MXxLEN, KEY(:MCxKYL), IVERS,IW, IBUF, IRET)
        ELSE
C
          WRITE(KEY(9:16),'(I6.6)') NRUN
          CALL MAxGET(0, 1, MXxLEN, KEY(:MCxKYL), IVERS,IW, IBUF, IRET)
          IF(IW.LT.0) THEN
            WRITE(MESAGE,125) NRUN
125         FORMAT(' %Warning_MAGETR..Run#',I6,' not exists.')
            CALL MAMSG(10,1, MESAGE)
            GO TO 800
          ENDIF
        ENDIF
      ENDIF
      GO TO 300
C
C -------------------------------------------------------------------
C (2) sequential access.
C -------------------------------------------------------------------
C
200   CONTINUE
      CALL MAxGET(3, 1, MXxLEN, KEY(:MCxKYL), IVERS, IW, IBUF, IRET)
      IF(IRET.NE.0) GO TO 810
      IEXP = IBUF(3)
      IRUN = IBUF(4)
      IF(NEXP.NE.0) THEN
        IF(IEXP.NE.NEXP) GO TO 200
        IF(NRUN.NE.0.AND.NRUN.EQ.IRUN) GO TO 300
        IF(NRUN.NE.0.AND.NRUN.LT.IRUN) GO TO 800
        IF(NRUN.EQ.0.AND.IRUN.NE.IORUN) GO TO 300
      ELSE
        IF(IRUN.NE.IORUN) GO TO 300
      ENDIF
      GO TO 200
C
300   CONTINUE
      NWORDS = MIN(IW, MXxBUF)
      CALL UVCOPY(NWORDS, IBUF, NBUF)
      IORUN  = IBUF(4)
      IOEXP  = IBUF(3)
      RETURN
C
800   CONTINUE
      NWORDS = -1
      RETURN
810   CONTINUE
      NWORDS = -2
      RETURN
      END
