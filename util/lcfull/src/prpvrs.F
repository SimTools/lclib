CC*********************************************************************C
C*                                                                     *
C*-----------------------------------------                            *
C* Subroutine PRPVRS( MODNAM, NVERS, NDATE )                           *
C*-----------------------------------------                            *
C*                                                                     *
C*(Purpose)                                                            *
C*    Stack version date of each analysis module.
C*                                                                     *
C*(Input)                                                              *
C*    MODNAM : name of the module.  ( Character)
C*    NVERS  : Version number of the module.                           *
C*    VDATE  : Version date.  (yymmdd)
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto  02-May-1987  Original version.                      *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRPVRS( MODNAM, NVERS, NDATE )
C
#include "prjenv.inc"
#include "fldscr.inc"
C
      CHARACTER*(*) MODNAM
      CHARACTER*12  TNAME, TENT
 
C==================<<  Entry Point  >>================================
 
C
C ---------------------------------------------------------------------
C (1) Search pre-defined program ID.
C ---------------------------------------------------------------------
C
      TNAME = MODNAM
      CALL UCUPER(12, TNAME )
      LTNAM = INDEX( TNAME, ':')
      DO 100 I = 1, NPROG
        TENT   = ENTNAM(I)
        CALL UCUPER(12, TENT)
        IF( TENT(:LTNAM) .NE. TNAME(:LTNAM) ) GO TO 100
          MODVRS(I) = NVERS
          MODDAT(I) = NDATE
          RETURN
100   CONTINUE
C
C ---------------------------------------------------------------------
C (2) Write error message.
C ---------------------------------------------------------------------
C
      PRINT *,'%Fatal Error in PRPVRS..'
      PRINT *,'Module name ',TNAME,' not defined.'
      CALL ERRTRA
      STOP
C
      END
