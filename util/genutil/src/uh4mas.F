CC********************************************************************CC
C*                                                                    *C
C*===========-----=======                                             *C
C*  Function UH4MAS(A,B)                                              *C
C*===========-----=======                                             *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculate invariant mass of (A+B) system.
C*    UP4MAS < 0, when squared-mass is negative.                      *C
C* (Inputs)                                                           *C
C*      A(0:3), b(0:3); 2 4-vector.                                   *C
C* (Output)                                                           *C
C*      UH4MAS     :
C* (Update Record)                                                    *C
C*   6/18/90  A. Miyamoto  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C =============-----=======
      FUNCTION UH4MAS(A,B)
C =============-----=======
C
      DIMENSION A(0:3),B(0:3)
 
      XMSQ = (A(0)+B(0))**2 - (A(1)+B(1))**2
     >     - (A(2)+B(2))**2 - (A(3)+B(3))**2
 
      IF( XMSQ .GE. 0. ) THEN
        UH4MAS = SQRT(XMSQ)
      ELSE
        UH4MAS = -SQRT(-XMSQ)
      ENDIF
 
      RETURN
      END
