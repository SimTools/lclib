CC********************************************************************CC
C*                                                                    *C
C*===========-----=======                                             *C
C*  Function UDOT4(A,B)                                               *C
C*===========-----=======                                             *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to calculate invariant 4-scalar product.                *C
C* (Inputs)                                                           *C
C*      A(4), B(4) :  2 4-vectors.                                    *C
C* (Output)                                                           *C
C*      UDOT4      :  A(4)*B(4)-UDOT3(A(1),B(1))                      *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =============-----=======
      FUNCTION UDOT4(A,B)
C =============-----=======
C  
      DIMENSION A(4),B(4)
      UDOT4=A(4)*B(4)
      DO 10 I=1,3
      UDOT4=UDOT4-A(I)*B(I)
   10 CONTINUE
      RETURN
      END
