CC********************************************************************CC
C*                                                                    *C
C*===================----====                                         *C
C* Subroutine PRRUNT(LEVEL, IDEBUG)                                   *C
C*===================----====                                         *C
C*  1) Purpose                                                        *C
C*       Terminates the prodution of this run.                        *C
C*  2) Output                                                         *C
C*       IRET   : return flag.                                        *C
C*  3) Relation                                                       *C
C*       Called by;                                                   *C
C*           PRRUN  : steers run loop.                                *C
C*       Calls the following subroutines;                             *C
C*           PRRTRM : terminates reduction routines.                  *C
C*           PRRSUM : summarizes this run.                            *C
C*  4) Update record                                                  *C
C*       10/21/85  A. Miyamoto  Original version.                     *C
C*       04/25/87  A. Miyamoto  Change arguments for FULL system      *C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----=====
      SUBROUTINE PRRUNT(LEVEL, IDEBUG)
C ======================----=====
C
#include "prrsta.inc"
C
 
C -------------------------------------------------------------------- C
C     Increment record statistics.
C -------------------------------------------------------------------- C
C
      IDEBUG = IDEBUG
      LEVEL  = LEVEL
      NSTREC(2, 8) = NSTREC(2, 8) + 1
      NSTREC(3, 8) = NSTREC(3, 8) + 1
 
C -------------------------------------------------------------------- C
C     Update End_Run banks.                                            C
C -------------------------------------------------------------------- C
C
      CALL PRWERN
C
C -------------------------------------------------------------------- C
C     Print Run end summary.                                           C
C -------------------------------------------------------------------- C
C
      CALL PRRSUM( IDEBUG, IRET )
C
 9000 CONTINUE
      RETURN
      END
