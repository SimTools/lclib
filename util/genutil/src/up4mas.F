CC********************************************************************CC
C*                                                                    *C
C*===========-----=======                                             *C
C*  Function UP4MAS(A,B)                                              *C
C*===========-----=======                                             *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculate invariant mass of (A+B) system.
C*    UP4MAS < 0, when squared-mass is negative.                      *C
C* (Inputs)                                                           *C
C*      A(4), b(4); 2 4-vector.                                   *C
C* (Output)                                                           *C
C*      UH4MAS     :
C* (Update Record)                                                    *C
C*   6/18/90  A. Miyamoto  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C =============-----=======
      FUNCTION UP4MAS(A,B)
C =============-----=======
C
      DIMENSION A(4),B(4)
 
      XMSQ = (A(4)+B(4))**2 - (A(1)+B(1))**2
     >     - (A(2)+B(2))**2 - (A(3)+B(3))**2
      IF( XMSQ .GE. 0. ) THEN
        UP4MAS = SQRT(XMSQ)
      ELSE
        UP4MAS = -SQRT(-XMSQ)
      ENDIF
 
      RETURN
      END
