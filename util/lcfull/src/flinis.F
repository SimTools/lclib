C   18/11/86 704142103  MEMBER NAME  FLSUBPRG (FORT)     M  FORTRAN
C+
C File : FLSUBPRG
C Description : Initialization/Temination routines for supplied subprgs
C
C Author : Ryosuke Itoh, Univ. of Tokyo ( at KEK )
C Date : 18 - NOV - 1986
C-
C----------
C FLINIS
C----------
C
      SUBROUTINE FLINIS
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
        IF ( ENTINI(LI) .NE. 0 ) THEN
#ifdef  MSP
          CALL FEXSUB ( ENTINI(LI) )
#endif
#ifdef  UNIX
          CALL FEXSUB (%VAL(ENTINI(LI)) )
#endif
        END IF
10    CONTINUE
      RETURN
      END
