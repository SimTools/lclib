CC********************************************************************CC
C*                                                                    *C
C*-------------------=======
C* Subroutine PRSFID(NDFILT)
C*-------------------=======
C*                                                                    *C
C*(Purpose)                                                           *C
C*     Store Filter ID to the steering common.                        *C
C*                                                                    *C
C*(Input)                                                             *C
C*     NDFILT  : Filter ID value.
C*                                                                    *C
C*(Output)                                                            *C
C*   None.
C*                                                                    *C
C*(Relation)                                                          *C
C*   Called by  user routines.                                        *C
C*                                                                    *C
C*(Author)                                                            *C
C*   06/05/87  A. Miyamoto  Original version.                         *C
C*                                                                    *C
CC********************************************************************CC
C
C ----------------------==========
      SUBROUTINE PRSFID( NDFILT)
C ---------------------=====-=====
C
#include "pretyp.inc"
C
C ----------------------------------------------------------------------
C (1) Chack ID range.
C ----------------------------------------------------------------------
C
      IDFILT = NDFILT
      RETURN
      END
