C***********************************************************************
C*
C*   -------------------======
C*    Subroutine FULENV( NENV )
C*   -------------------======
C*(Function)
C*    Tell FULL environment, Generator mode or not.
C*(Input)
C*    NENV  : = 0  : non-Generator mode. ( Do TBREAD )
C*            = 1  : Generator mode. ( Do TBINIT )
C*(Author)
C*    A. Miyamoto  18-Sep-1989  Modifiyed for JCL version
C*
C***********************************************************************
C
      SUBROUTINE FULENV( NENV )
C
#include "prgenf.inc"
C
C =====< Entry Point >==================================================
C
      NENV = 0
      IF( NUMGEN .NE. 0 ) NENV = 1
C
      RETURN
      END
