C**********************************************************************
C*
C* ------------------------------==============
C* Subroutine MAGRNL(NEXP, MXxBUF, NWORDS, NBUF)
C* ------------------------------==============
C*
C*(Function)
C*   Obtain RUNLIST record.
C*
C*(Input)
C*   NEXP   ; Experiment # whose information is returned.
C*   MXxBUF ; Size of array NBUF.
C*
C*(Output)
C*   NWORDS ; # of data in NBUF.
C*        = -1 when RUNLIST record is not obtained.
C*   NBUF   ; Run information array.
C*
C*(Author)
C*
C*  A. Miyamoto.  19-Jan-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MAGRNL(NEXP, MXxBUF, NWORDS, NBUF)
C
#include "macntl.inc"
C
      PARAMETER      (MXxLEN=1000)
      INTEGER*4       NBUF(MXxBUF)
      COMMON /MAWORK/ IBUF(MXxLEN)
      CHARACTER       KEY*20
C
C============< Entry Point >=========================================
C
C -------------------------------------------------------------------
C (1) Read data.
C -------------------------------------------------------------------
C
      IVERS = 0
      IW    = 0
      WRITE(KEY,'(''E'',I5.5,''.RUNLIST'')') NEXP
      CALL MAxGET(0, 1, MXxLEN, KEY(:MCxKYL), IVERS, IW, IBUF, IRET)
      IF(IRET.NE.0) GO TO 800
C
      NWORDS = MIN(IW, MXxBUF)
      CALL UVCOPY(NWORDS, IBUF, NBUF)
      RETURN
C
C
800   CONTINUE
      NWORDS = -1
      RETURN
      END
