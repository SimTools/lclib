CC********************************************************************CC
C*                                                                    *C
C*------------------======                                            *C
C* Subroutine PRWERR( IDREC, NERFLG)
C*------------------======                                            *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Create Error information bank.
C*                                                                    *C
C*(Input)                                                             *C
C*   IDREC     : Record ID, should be 1 or 8.                         *C
C*                                                                    *C
C*(Output)                                                            *C
C*   NERFLG    : Error flag of this event.
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
      SUBROUTINE PRWERR( IDREC, NERFLG )
C ======================----===
C
      INTEGER*4  NERFLG
C
#include "uskalr.inc"
#include "preerr.inc"
#include "prdtyp.inc"
C
      PARAMETER        (IWKSIZ = 2000)
      COMMON /SSWORK/   NEARY(200), IERRBF(0:MXxSKL,2),
     >                  IBUFF(IWKSIZ)
      CHARACTER         BHED(2)*12
      INTEGER*4         LBHED(2)
      DATA (LBHED(I), BHED(I), I=1,2)
     .  /10, 'Reduction:',  11,  'Production:'/
 
      CHARACTER         ERRNAM(13)*8, BNKNAM*32
      INTEGER*4         LENERR(13)
      DATA   ( LENERR(I), ERRNAM(I), I = 1, 13)
     . /4, 'STER',
     .  3, 'IDC',
     .  3, 'TPC',
     .  4, 'BTOF',
     .  3, 'BDC',
     .  3, 'BCL',
     .  3, 'BMU',
     .  4, 'ETOF',
     .  3, 'EDC',
     .  3, 'ECL',
     .  3, 'LUM',
     .  4, 'Hybrid',
     .  3, 'XXX'/
C
C =======< Entry Point >===============================================
C
C --------------------------------------------------------------------
C     Reset error flag.
C --------------------------------------------------------------------
C
      NERFLG = 0
C
C --------------------------------------------------------------------
C (2) Obtain error count of this event, for reduction job.
C --------------------------------------------------------------------
C
      CALL UVZERO(2*(MXxSKL+1), IERRBF)
      IF( IDREC .EQ. 1 ) THEN
        DO 200 J = 1, 2
        DO 200 I = 0, MXxSKL
          IERRBF(I, J) = ISKLR(I, J) - ISKLRE(I, J)
200     CONTINUE
      ELSE
        DO 210 J = 1, 2
        DO 210 I = 0, MXxSKL
          IERRBF(I, J) = ISKLR(I, J) - ISKLRR(I, J)
210     CONTINUE
      ENDIF
C
C --------------------------------------------------------------------
C (3) Search error in this event or Run.
C --------------------------------------------------------------------
C
      DO 300 ITYP = 1, 2
        DO 310 IDETCT = 1, 13
          IPED     = 10*(IDETCT-1)
          IERR     = 0
          DO 320 I = IPED, IPED+9
            IF( IERRBF(I, ITYP) .LE. 0 ) GO TO 320
            IERR   = IERR + 1
320       CONTINUE
          IF( IERR .LE. 0 ) GO TO 310
C
C --------------------------------------------------------------------
C (3.1) Set bit for error detector.
C --------------------------------------------------------------------
C
          IPOS   = 16*(ITYP-1) + IDETCT - 1
          NERFLG =  IBSET( NERFLG, IPOS)
C
C
C --------------------------------------------------------------------
C (3.2) Put detail error information into Error bank.
C --------------------------------------------------------------------
C
C
          BNKNAM = BHED(ITYP)(:LBHED(ITYP))//
     .             ERRNAM(IDETCT)(:LENERR(IDETCT))//';Error'
          CALL TBNOEL(IDREC, BNKNAM, NELM, NEARY)
          IF( NELM .EQ. -2 ) THEN
            CALL UERROR(3, DUMMY, 9, 'PRWERR',
     >       'Event record does not exists at event termination.')
          ELSEIF( NELM .EQ. -1 ) THEN
            CALL TBCRTE(IDREC, BNKNAM, 0, 0, IRET)
          ENDIF
C
          IELM = NDTYPE(9)
          CALL TBPUT(IDREC, BNKNAM, IELM, 10, IERRBF(IPED,ITYP), IRET)
C
310     CONTINUE
300   CONTINUE
C
900   CONTINUE
      RETURN
      END
