CC********************************************************************CC
C*                                                                    *C
C*=============================                                       *C
C* Subroutine UERINI(JOBTYP)                                          *C
C*=============================                                       *C
C*  1) Purpose                                                        *C
C*       Initializes TOPAZ error handling facility.                   *C
C*  2) Input                                                          *C
C*       JOBTYP  : Type of job.                                       *C
C*              = 1 for simulator.                                    *C
C*              = 2 for reduction job.                                *C
C*              = 3 for production job.                               *C
C*  3) Relation                                                       *C
C*       Called by;                                                   *C
C*       Calls the following subroutines;                             *C
C*           None.                                                    *C
C*  4) Update record                                                  *C
C*       09/14/85  K.Fujii  Original version.                         *C
C*       05/27/86  K.Fujii  Scaler # 0 is added.                      *C
C*       05/01/87  K.Fujii  Use UVZERO for zapping.                   *C
C*                          Added zapping of JSKLR.                   *C
C*       05/06/87  K.Fujii  JSKLR removed. ISKLR is 2-dim. instead.   *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UERINI(JOBTYP)
   
#include "uskalr.inc"
#include "ujbtyp.inc"
   
C--
C  Set job type.   For a while set JBTYP by hand.
C--
      JBTYP = JOBTYP
C--
C  Reset error counters.
C--
      CALL UVZERO(2*MXxSKL+2,ISKLR)
   
      RETURN
      END
