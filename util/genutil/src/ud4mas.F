CC********************************************************************CC
C*                                                                    *C
C*===========-----=======                                             *C
C*  Function UD4MAS(A,B)                                              *C
C*===========-----=======                                             *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculate invariant mass of (A+B) system.
C*    UD4MAS < 0, when squared-mass is negative.                      *C
C* (Inputs)                                                           *C
C*      A(0:3), b(0:3); 2 4-vector.                                   *C
C* (Output)                                                           *C
C*      UD4MAS     :
C* (Update Record)                                                    *C
C*   6/18/90  A. Miyamoto  Original version.
C*   3/17/93  A.Miyamoto Real*8 version
C*                                                                    *C
CC********************************************************************CC
C
C =============-----=======
      REAL FUNCTION UD4MAS*8(A,B)
C =============-----=======
C
      IMPLICIT REAL*8 (A-H,O-Z)

      DIMENSION A(0:3),B(0:3)
 
      XMSQ = (A(0)+B(0))**2 - (A(1)+B(1))**2
     >     - (A(2)+B(2))**2 - (A(3)+B(3))**2
 
      IF( XMSQ .GE. 0. ) THEN
        UD4MAS = SQRT(XMSQ)
      ELSE
        UD4MAS = -SQRT(-XMSQ)
      ENDIF
 
      RETURN
      END
