C
C----------
C FLCLSS
C----------
C
      SUBROUTINE FLCLSS
C
C Common
C
#include "fldscr.inc"
C
C VARIABLES
C
      INTEGER * 4      I
C
C MAIN
C
C      DO 10 I = 1, NPROG
       DO 10 I = 1, LISTSP(0)
         LI = LISTSP(I)
        IF ( ENTTER(LI) .NE. 0 ) THEN
#ifdef  MSP
          CALL FEXSUB ( ENTTER(LI) )
#endif
#ifdef  UNIX
          CALL FEXSUB ( %VAL(ENTTER(LI)) )
#endif
        END IF
10    CONTINUE
      RETURN
      END
