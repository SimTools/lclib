CC*********************************************************************C
C*                                                                     *
C*--------------------------                                           *
C* Subroutine PRSDTP(NDTYPE)                                           *
C*--------------------------                                           *
C*                                                                     *
C*(Purpose)                                                            *
C*    Scan Execution entry list and prepare data type flag, which will *
C*  are filled at the run initialization.                              *
C*                                                                     *
C*(Input)                                                              *
C*    none.                                                            *
C*                                                                     *
C*(Output)                                                             *
C*    NDTYPE   : Data type flag of current run.                        *
C*          (1) : Generator flag.                                      *
C*          (2) : Simulator flag.                                      *
C*          (3) : Trigger_Simulator.                                   *
C*          (4) : Formatting                                           *
C*          (5) : Reduction.                                           *
C*          (6) : Production.                                          *
C*          (7) : Full_analysis.                                       *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto  18-Dec-1985  Original version.                      *
C*                  8-Sep-1989  To FULLnize Simulator. add GEN,SIM,TRI *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRSDTP( NDTYPE )
C
#include "fldscr.inc"
#include "fllevl.inc"
C
      INTEGER*4  NDTYPE(7)
 
      PARAMETER  (NO=14)
      INTEGER*4  IDTYPE(NO), LENNAM(NO)
      CHARACTER*12  DEFNAM(NO)
      DATA  (IDTYPE(I), LENNAM(I), DEFNAM(I), I=1,NO)/
     . 5, 1, 'R',
     . 6, 3, 'IDC',
     . 6, 3, 'TPC',
     . 6, 4, 'BTOF',
     . 6, 3, 'BDC',
     . 6, 3, 'BCL',
     . 6, 3, 'BMU',
     . 6, 3, 'EDC',
     . 6, 3, 'ECL',
     . 6, 3, 'LUM',
     . 6, 5, 'STEER',
     . 1, 3, 'GEN',
     . 2, 3, 'SIM',
     . 3, 3, 'TRI'/
 
C==================<<  Entry Point  >>================================
 
C ---------------------------------------------------------------------
C (1) Reset array.
C ---------------------------------------------------------------------
C
      DO 100 I = 1, 6
        NDTYPE(I) = 0
100   CONTINUE
      NDTYPE(7) = 1
C
C ---------------------------------------------------------------------
C (2) Search execution list and set flag.
C ---------------------------------------------------------------------
C
      DO 200 I = 1, LISTSP(0)
        LI     = LISTSP(I)
        IF( LI .LE. 0 ) THEN
            PRINT *,'%Fatal Error in PRSDTP '
            PRINT *,'       Please define default execution order in ',
     >              ' DESCRIBE routine.'
            STOP
        ENDIF
        IF( EXELVL(LI) .LE. 0 ) GO TO 200
 
        DO 300 J = 1, NO
          IF( ENTNAM(LI)(:LENNAM(J)).NE.DEFNAM(J)(:LENNAM(J)))GO TO 300
          NDTYPE(IDTYPE(J)) = 1
300     CONTINUE
200   CONTINUE
C
      RETURN
      END
