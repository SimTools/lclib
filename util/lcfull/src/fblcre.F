C
C----------
C FBLCRE
C----------
C
        LOGICAL FUNCTION FBLCRE ( PNTR )
C
C ARGUMENTS
C
        INTEGER * 4     PNTR
C
C COMMON
C
#include "fldscr.inc"
C
C VARIABLES
C
        INTEGER * 4     I, IRET
C
C MAIN
C
        IF ( OUTBLK(PNTR) .EQ. 0 ) THEN
          FBLCRE = .TRUE.
          RETURN
        END IF
C
        DO 10 I = 1, OUTBLK(PNTR)
          CALL TBCRTE ( 1, OUBNAM(PNTR,I), OUBSIZ(PNTR,I), 1, IRET )
          IF ( IRET .EQ. -3 ) THEN
            PRINT *, '%ANL-F-BNKEXC, Number of bank exceeded LIMIT'
          ELSE IF ( IRET .EQ. -2 ) THEN
            FBLCRE = .FALSE.
            RETURN
          END IF
10      CONTINUE
C
        FBLCRE = .TRUE.
        RETURN
        END
