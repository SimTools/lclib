C
C----------
C FENDRN
C----------
C
        SUBROUTINE FENDRN
C
C COMMON
C
#include "fldscr.inc"
C       INCLUDE         'FLDSCR.INC'
#include "fllevl.inc"
#include "prjflg.inc"
C
C VARIABLES
C
        INTEGER * 4     I
C
C MAIN
C
C Calls Run termination of Steering routine.
C
        CALL PRRUNT( NJBLVL(2,1), NJBDBG(2,1) )
C
C
C       DO 10 I = 1, NPROG
	DO 10 I = 1, LISTSP(0)
	  LI = LISTSP(I)
	  
          IF ( ENTERU(LI) .NE. 0 .AND. EXELVL(LI) .GT. 0 ) THEN
#ifdef  MSP
            CALL FEXPRN ( ENTERU(LI), EXELVL(LI), DBGLVL(LI) )
#endif
#ifdef  UNIX
#if !defined(__GFORTRAN__)
           CALL FEXPRN ( %VAL(ENTERU(LI)), EXELVL(LI), DBGLVL(LI) )
#else
           call cfexprn ( ENTERU(LI), EXELVL(LI), DBGLVL(LI) )
#endif
#endif
          END IF
10      CONTINUE
        RETURN
        END
