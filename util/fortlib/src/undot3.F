CC********************************************************************CC
C*                                                                    *C
C* ========================                                           *C
C*   Function UNDOT3(A,B)                                             *C
C* ========================                                           *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   calculates 3-vector normed-dot product. After normalizing A and  *C
C*   B to unity, makes dot-product of the two.                        *C
C* (Inputs)                                                           *C
C*   A,B  : 2 3-vectors to make dot product.                          *C
C* (Output)                                                           *C
C*   UDOTN3: results of the dot product.                              *C
C* (Relation)                                                         *C
C*   calls                                                            *C
C*      UNRMV3, UDOT3                                                 *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================
      FUNCTION UNDOT3(A,B)
C =========================
C  
      DIMENSION A(3),B(3),C(3),D(3)
      CALL UNRMV3(A,C)
      CALL UNRMV3(B,D)
      UNDOT3 = UDOT3(C,D)
      RETURN
      END
