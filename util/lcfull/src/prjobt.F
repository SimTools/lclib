CC********************************************************************CC
C*                                                                    *C
C*===================----====                                         *C
C* Subroutine PRJOBT
C*===================----====                                         *C
C*  1) Purpose                                                        *C
C*       Terminates the production Job.                               *C
C*  2) Output                                                         *C
C*       IRET   : return flag.                                        *C
C*  3) Relation                                                       *C
C*       Called by;                                                   *C
C*           PRMAIN : Main routine.                                   *C
C*       Calls the following subroutines;                             *C
C*           PRJTRM : terminates reduction routines.                  *C
C*           PRJSUM : summarizes this job.                            *C
C*  4) Update record                                                  *C
C*       10/21/85  A. Miyamoto  Original version.                     *C
C*       25-Apr-1987 A.Miyamoto  Change Arguments for FULL system     *C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----=====
      SUBROUTINE PRJOBT
C ======================----=====
C
#include "prjflg.inc"
C
C -------------------------------------------------------------------- C
C     Execute Job end module.                                          C
C -------------------------------------------------------------------- C
C
      CALL FLCLSS
C
C -------------------------------------------------------------------- C
C     Summarize this job.                                              C
C -------------------------------------------------------------------- C
C
      CALL PRJSUM(IRET)
C
CTOPAZ       IF( NJBLVL(2, 1) .GE. 2 ) CALL TBLSUM
#ifdef  JLC
       IF( NJBLVL(2, 1) .GE. 8 ) CALL TBLSUM
#endif
C
      PRINT *,' '
      PRINT *,' '

 9000 RETURN
      END
