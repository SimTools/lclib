CC********************************************************************CC
C*                                                                    *C
C*------------------======                                            *C
C* Subroutine PREVIN(IDREC, LEVEL, IDEBUG, NRET)
C*------------------======                                            *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Event initialization of the analysis routine.                    *C
C*   Do (1) Fill Production:Header bank.                              *C
C*      (2) Reset Filter ID and Event ID flag.                        *C
C*      (3) Write Detail Event information, if IDEBUG > 4.            *C
C*                                                                    *C
C*(Input)                                                             *C
C*   Level  : Level flag.
C*   IDEBUG : Debug flag.
C*                                                                    *C
C*(Output)                                                            *C
C*   NRET  : Return flag.                                             *C
C*      = 0 for normal return                                         *C
C*      < 0 for some error.                                           *C
C*                                                                    *C
C*(Relation)                                                          *C
C*   Called by PRANAL.                                                *C
C*                                                                    *C
C*(Author)                                                            *C
C*   A. Miyamoto  06-May-1987  Original version.                      *C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----===
      SUBROUTINE PREVIN(IDREC, LEVEL, IDEBUG, NRET)
C ======================----===
C
#include "pretyp.inc"
#include "prdtyp.inc"
#include "prjctl.inc"
#include "prjenv.inc"
#include "uskalr.inc"
#include "preerr.inc"
C
      PARAMETER        (IWKSIZ = 200)
      COMMON /SSWORK/   NEARY(200), IBUFF(IWKSIZ)
      CHARACTER*8       VDATE
C
C =======< Entry Point >===============================================
C
C --------------------------------------------------------------------
C     Reset return flag.
C --------------------------------------------------------------------
C
      NRET = 0
      LEVEL = LEVEL
C     PRINT *,' PREVIN called..IDREC =',IDREC
C     PRINT *,' NDTYPE=',NDTYPE
      IF( IDREC .NE. 1 ) RETURN
C
C --------------------------------------------------------------------
C (1) If this is first time to process, create bank PRODUCTION.
C --------------------------------------------------------------------
C
C     IF( NDTYPE(8) .EQ. 1 .OR. NDTYPE(7) .EQ. 1 ) THEN
        IF( NDTYPE(9) .LE. 1 ) THEN
          CALL TBCRTE(1, 'Production:Header', 0, 0, IRET)
        ENDIF
C     ENDIF
C
C --------------------------------------------------------------------
C (2) Determine element # from # of times Processed Full_Analysis.
C --------------------------------------------------------------------
C
      IELM = NDTYPE(9)
C
C --------------------------------------------------------------------
C (3) Put Event header Dummy event header information.
C --------------------------------------------------------------------
C    IBUFF(1) : Event sequence #.
C         (2) : Processed Date.
C         (3) : Processed Time.
C         (4) : Error flag.
C         (5) : Filter ID.
C         (6) : Event ID.
C         (7) : Version # of Steering.
C         (8) : Version # of FLANLI
C
      IBUFF(1) = IEVSEQ
      CALL UIDATE( IBUFF(2) )
cc      CALL   TIME( IBUFF(3) )
      call uitime( ibuff(3) )
      IBUFF(4) = -1
      IBUFF(5) = 0
      IBUFF(6) = 0
      CALL PRGVRS( IVERS, IVLVL, VDATE )
      IBUFF(7) = 100*IVERS + IVLVL
      READ(DSCNAM,'(4A4)') (IBUFF(K),K=8,11)
      IBUFF(12)= NDSCVR
      CALL TBPUT(1, 'Production:Header', IELM,12, IBUFF, IRET)
C
C --------------------------------------------------------------------
C (4) Reset Event ID and Filted ID common.
C --------------------------------------------------------------------
C
      IDFILT   = 0
      IDEVNT   = 0
C
C --------------------------------------------------------------------
C (5) Copy # of errors at the begining of event.
C --------------------------------------------------------------------
C
CTOPAZ       CALL UVCOPY(2*(MXxSKL+1), ISKLR, ISKLRE)
C
C --------------------------------------------------------------------
C (6) Write Detail event header, if IDEBUG > 4.
C --------------------------------------------------------------------
C
      IF( IDEBUG .GE. 100 .AND. NDTYPE(1) .EQ. 2 ) THEN
        CALL TBGET(1, 'Generator:Header', 1, NW, IBUFF, IRET)
        WRITE(6,610) IBUFF(1), IBUFF(4), IBUFF(8)
610     FORMAT(10X,'Generator  ID =', I5,'  Evt#=',I6,
     >             ' Event Type =',I6)
      ENDIF
C
C --------------------------------------------------------------------
C (7) Unpack Trigger Bank.
C --------------------------------------------------------------------
C
CTOPAZ       CALL PRUPTR
C
900   CONTINUE
      RETURN
      END
