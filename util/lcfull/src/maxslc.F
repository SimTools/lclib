CC*********************************************************************C
C*                                                                     *
C*-----------------------------=====                                   *
C* SUBROUTINE MAxSLC(NW, NBUF, NRET)                                   *
C*-----------------------------=====                                   *
C*                                                                     *
C*(Purpose)                                                            *
C*   Check the input record satisfies the condition set in /MASLCT/    *
C*                                                                     *
C*(Input)                                                              *
C*   NW     ; # of words of the record.                                *
C*   NBUF   ; Data record.                                             *
C*                                                                     *
C*(Output)                                                             *
C*   NRET   ; Return code.                                             *
C*        = 0  Satisfies the conditions.                               *
C*        =-1  Does not satisfies.                                     *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  20-Jan-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAxSLC(NW, NBUF, NRET)
C
#include "maslct.inc"
C
      INTEGER*4  NBUF(NW)
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Loop over max record.
C ----------------------------------------------------------------------
C
      IW   = MIN(NW, MXxSLC)
      NRET = 0
      DO 100 I = 1, IW
        IF(NBUF(I).LT.MSxSLC(1,I).OR.NBUF(I).GE.MSxSLC(2,I)) GO TO 120
100   CONTINUE
      RETURN
120   CONTINUE
      NRET = -1
      RETURN
      END
