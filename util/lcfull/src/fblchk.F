C
C----------
C FBLCHK
C----------
C
        LOGICAL FUNCTION FBLCHK ( PNTR )
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
        CHARACTER * 64  BLKNAM
        INTEGER * 4     ISRT, IPBNK
C
C MAIN
C
        IF ( INPBLK(PNTR) .EQ. 0 ) THEN
          FBLCHK = .TRUE.
          RETURN
        END IF
C
        DO 10 I = 1, INPBLK(PNTR)
          CALL TBxFRD( 1, IPSUB )
          BLKNAM = INBNAM(PNTR,I)
          CALL UCUPER( 64, BLKNAM )
          CALL TBxFBK( IPSUB, BLKNAM, ISRT, IPBNK, IRET )
          IF( IRET .LE. 0 ) THEN
            FBLCHK = .FALSE.
            RETURN
          END IF
10      CONTINUE
C
        FBLCHK = .TRUE.
        RETURN
        END
