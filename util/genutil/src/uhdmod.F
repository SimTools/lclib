C**********************************************************************
C*
C* ---------------------------------------=====
C*  Subroutine UHDMOD( X, NUMMOD, WIDTH,  NMODE )
C* ---------------------------------------=====
C*(Function)
C*  Decide decay mode according to the decay width.
C*(Input)
C*  X : REAL*8 variable of 0 to 1 to decide decay mode.
C*  NUMMOD : Size of array WIDTH
C*  WIDTH  : Decay width array (WIDTH(0:NUMMOD))
C*           WIDTH(0) is total width
C*(Output)
C*  NMODE  : Decay mode.
C**********************************************************************
C
      SUBROUTINE UHDMOD( X, NUMMOD, WIDTH, NMODE )
C
      REAL*8  X
      REAL*4  WIDTH(0:NUMMOD)
C
C =====< Entry Point >=================================================
C
      BRWS = WIDTH(0)*X
      BSUM = 0.
      DO 150 I = NUMMOD, 1, -1
        BSUM = BSUM + WIDTH(I)
        IF( BRWS .GT. BSUM ) GO TO 150
        NMODE = I
        GO TO 155
150   CONTINUE
      NMODE = 1
155   CONTINUE
      RETURN
      END
