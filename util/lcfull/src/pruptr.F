C***********************************************************************
C*                                                                     *
C* --------------------------------                                    *
C*  Subroutine PRUPTR                                                  *
C* --------------------------------                                    *
C*                                                                     *
C*(Function)                                                           *
C*    Unpack trigger list, results are saved in /PRTRIG/               *
C*                                                                     *
C*(Input)                                                              *
C*    none.                                                            *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto    18-Feb-1987  Original version.                    *
C*    K. Fujii       03-Jun-1987  Switch XCTIME according to run type. *
C*    A. Miyamoto    25-May-1990  Delete -4.9 timming correction.      *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE PRUPTR
C
#include "prtrig.inc"
#include "prhead.inc"
C
      CHARACTER*30 BANK /'REDUCTION:BTOF;SORTED_RAW_DATA'/
      INTEGER*2  HTRIG(16)
#ifdef  UNIX
      INTEGER*2  IIBITS, IB, INUM
#endif
      COMMON /SSWORK/ ITRIG(8), RBUF(260)
      EQUIVALENCE (HTRIG(1), ITRIG(1))
      DATA   IFIRST /1/
C
C===============< Entry Point >=================================
C
C ----------------------------------------------------------------------
C (1) Get Trigger bank.
C ----------------------------------------------------------------------
C
      CALL TBGET(1,'Raw_Data:Trigger', 1, NW, ITRIG, IRET)
C
C ----------------------------------------------------------------------
C (2) Unpack IDC trigger info.
C ----------------------------------------------------------------------
C
#ifdef  UNIX
      INUM = 2
#endif
      CALL UVZERO(64, NTIDRP)
      DO 200 IW = 1, 8
        DO 210 IP = 1, 8
          IS   = 8*(IW-1) + IP
          IB   = 16 - 2*IP
#ifdef  MSP
          IBIT = IIBITS(HTRIG(IW), IB, 2)
#endif
#ifdef  UNIX
          IBIT = IIBITS(HTRIG(IW), IB, INUM)
#endif
          NTIDRP(IS) = IBIT
210     CONTINUE
200   CONTINUE
C
C ----------------------------------------------------------------------
C (3) Unpack TOF hit pattern.
C ----------------------------------------------------------------------
C
      CALL UVZERO(32, NTTOFH)
      DO 300 IB = 0, 31
#if !defined(HIUXF77) && !defined(JavaVM)
        IF(.NOT.BTEST(ITRIG(5),IB)) GO TO 300
#else
	KIB = IB
        IF(.NOT.BTEST(ITRIG(5),KIB)) GO TO 300
#endif
        NTTOFH(IB+1) = 1
300   CONTINUE
C
C
C ----------------------------------------------------------------------
C (4) Unpack C-Trigger.
C ----------------------------------------------------------------------
C
      XTIME  = HTRIG(11)
      IF ( xRUNTP.EQ.1 ) THEN
         XTCTIM = 150.
C
C (5) Get Trigger timing from TOF counter info, for COSMIC ray run.
C
      ELSE
CCC         XTCTIM = 0.1562*XTIME
         XTCTIM = 150.0
         CALL TBGET( 1, BANK, 1, NW, RBUF, IRET )
         IF ( IRET .GE. 0 )  THEN
CTAYM900525        XTCTIM = RBUF(130) - 4.9 + 150.
           XTCTIM = RBUF(130) + 150.
         ELSEIF( IFIRST .EQ. 1 ) THEN
           PRINT *,'%Warning..PRUPTR  Unable to obtain Cosmic Ray ',
     .             'trigger timing.  Use XTCTIM = 150.0 '
           PRINT *,'  TBGET ',BANK,' return code was ', IRET
           IFIRST = 0
         END IF
      ENDIF
C
      RETURN
      END
