CC*********************************************************************C
C*                                                                     *
C*--------------------------------------                               *
C* SUBROUTINE MAxRST(NEXP, NRUN, FRMVOL)                               *
C*--------------------------------------                               *
C*                                                                     *
C*(Purpose)                                                            *
C*  update   first and last run # information.
C*                                                                     *
C*(Input)                                                              *
C*   NEXP  ; Experiment # of the last one.                             *
C*   NRUN  ; Run # of the last one.                                    *
C*   FRMVOL ; Vol=Ser of the last one.                                 *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  12-Jan-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAxRST(NEXP, NRUN, FRMVOL)
C
#include "macntl.inc"
#include "maxwrk.inc"
C
      CHARACTER*(*) FRMVOL
      CHARACTER*68 KEY, KEYWRK
      CHARACTER*8  FVOL
      CHARACTER*137 MESAGE
C
C     IBUF(1) = Date when the largest run # info. is saved.
C         (2) = Time
C         (3) = Experiment # of largest run # run.
C         (4) = Run # of largest run.
C         (5) = Format tape vol=ser.
C         (6) =      "
C         (7) = Date when the smallest run # info. is saved.
C         (8) = Time
C         (9) = Experiment # of smallest run # run.
C        (10) = Run # of smallest run.
C        (11) = Format tape vol=ser.
C        (12) =      "
C
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Get old information.
C ----------------------------------------------------------------------
C
      IWRIT = 0
      IWRIT = 1
#ifdef  UNIX
      IF( IWRIT.EQ.1 ) THEN
        PRINT *,'MAxRST .. This routine should not be called ',
     >          ' on unix system.'
        STOP
      ENDIF
#else
      WRITE(KEY,110) NEXP
110   FORMAT('E',I5.5,'.RUNLIST')
      READ(MCxDBS, KEY=KEY(:MCxKYL), ERR=300)
     >     KEYWRK(:MCxKEY), IW, (IBUF(K1),K1=1,IW)
C
      IF(IW.LE.0) THEN
        IWRIT = 1
        IW = 12
        CALL UVZERO(IW, IBUF)
        CALL UIDATE(IBUF(1))
        CALL   TIME(IBUF(2))
        IBUF(3) = NEXP
        IBUF(4) = 0
        CALL UIDATE(IBUF(7))
        CALL   TIME(IBUF(8))
        IBUF(9) = NEXP
        IBUF(10) = 9999999
      ENDIF
C
      IF(NRUN.LT.IBUF(10)) THEN
        IWRIT = 1
        CALL UIDATE(IBUF(7))
        CALL   TIME(IBUF(8))
        IBUF( 9) = NEXP
        IBUF(10) = NRUN
        FVOL     = FRMVOL
        READ(FVOL,'(2A4)') IBUF(11), IBUF(12)
        IW       = MAX(IW, 12)
      ELSEIF(NRUN.GT.IBUF(4)) THEN
        IWRIT = 1
        CALL UIDATE(IBUF(1))
        CALL   TIME(IBUF(2))
        IBUF( 3) = NEXP
        IBUF( 4) = NRUN
        FVOL     = FRMVOL
        READ(FVOL,'(2A4)') IBUF( 5), IBUF( 6)
        IW       = MAX(IW, 12)
      ENDIF
C
      IF(IWRIT.EQ.1) REWRITE(MCxDBS)
     >               KEYWRK(:MCxKEY),IW,(IBUF(K1),K1=1,IW)
C
      RETURN
C
C ----------------------------------------------------------------------
C (3) If this is first run in a experiment, update EXPLIST key.
C ----------------------------------------------------------------------
C
300   CONTINUE
      CALL MAxEST( NEXP )
C
C (3.1) Write RUNLIST record of new run.
C
      IW = 12
      CALL UVZERO(IW, IBUF)
      CALL UIDATE(IBUF(1))
      CALL   TIME(IBUF(2))
      IBUF(3)  = NEXP
      IBUF(4)  = NRUN
      FVOL     = FRMVOL
      READ(FVOL,'(2A4)') IBUF( 5), IBUF( 6)
      CALL UIDATE(IBUF(7))
      CALL   TIME(IBUF(8))
      IBUF(9)  = NEXP
      IBUF(10) = NRUN
      FVOL     = FRMVOL
      READ(FVOL,'(2A4)') IBUF(11), IBUF(12)
C
      WRITE(KEY,310) NEXP
310   FORMAT('E',I5.5,'.RUNLIST')
      KEY(MCxKYL+1:MCxKEY) = '9999'
      WRITE(MCxDBS,ERR=800) KEY(:MCxKEY), IW, (IBUF(I),I=1,IW)
      RETURN
C
C
C (8) Error.
C
800   CONTINUE
      MESAGE = ' %Warning_MAxRST..failed to create '//KEY(:MCxKEY)
      CALL MAMSG(1,1,MESAGE)
#endif
      RETURN
      END
