CC********************************************************************CC
C*                                                                    *C
C*===================----====                                         *C
C* Subroutine PRRUNI(LEVEL, IDEBUG)                                   *C
C*===================----====                                         *C
C*  1) Purpose                                                        *C
C*       Initializes processing of this run.                          *C
C*  2) Output                                                         *C
C*       IRET   : return flag.                                        *C
C*  3) Relation                                                       *C
C*       Called by;                                                   *C
C*           PRRUN  : steers run loop.                                *C
C*       Calls the following subroutines;                             *C
C*           PRRUNC : reads in run-dep. calibration constants.        *C
C*           PRRPIN : initializes run parameters.                     *C
C*           PRRINI : initializes each component for this run.        *C
C*  4) Update record                                                  *C
C*       10/14/85  A.Miyamoto  Original Version.                      *C
C*       01/28/86  A.Miyamoto  Call PRBDSB.                           *C
C*       04/25/87  A.Miyamoto  For FULL system                        *C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----=====
      SUBROUTINE PRRUNI(LEVEL, IDEBUG)
C ======================----=====
C
#include "prhead.inc"
#include "prrsta.inc"
#include "presta.inc"
#include "uskalr.inc"
#include "preerr.inc"
C
C=========< Entry Point >===============================================
C
      IRET  = 0
      LEVEL = LEVEL
C
C-----------------------------------------------------------------------
C (1) COPY Record and Analysis routine statistics
C-----------------------------------------------------------------------
C
      CALL UVCOPY(62, NRDREC, MRDREC)
      MRDREC = MRDREC - 1
      MSTREC(1, 2) = MSTREC(1, 2) - 1
      NSTREC(2, 2) = NSTREC(2, 2) + 1
      NSTREC(3, 2) = NSTREC(3, 2) + 1
 
      CALL UVCOPY( 64*5 + 2, NCTBGN, MCTBGN)
      CALL UVCOPY( 2*(MXxSKL+1), ISKLR, ISKLRR)
C
C-----------------------------------------------------------------------
C (2) Printout Run header.
C-----------------------------------------------------------------------
C
      IF(IDEBUG .GE. 10) WRITE(6,'(1H1)')
      IDBG = MOD( IDEBUG, 10 )
 
#ifdef  MSP
       CALL CLOCK(XIMUSD, 0, 1)
#endif
#ifdef  UNIX
       CALL CLOCKM(IMUSD)
#endif
#ifdef  UNIX
       XIMUSD = IMUSD
#endif
      IF( IDBG .GE. 6 ) WRITE(6,'(1H1)')
      IF( IDBG .GE. 4 ) WRITE(6,200) xEXPNO, xRUNNO, XIMUSD
200   FORMAT(1H ,//,
     >    ' *** Exp#',I5,'  Run# ',I4,'  Started ******* at CPU ',
     >    'time ',F8.3,' sec.  *********',//)
C
C
C -------------------------------------------------------------------- C
C (3) Fill Begin RUn record.
C -------------------------------------------------------------------- C
C
      CALL PRWDTP
C
C
C -------------------------------------------------------------------- C
C (4) Reset array for save constant version.
C -------------------------------------------------------------------- C
C
      CALL PRWCNS(-1, 0 )
C
 9000 RETURN
      END
