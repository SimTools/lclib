CC********************************************************************CC
C*                                                                    *C
C*=======================--===                                        *C
C*  Subroutine UCOPV4(P1,P2)                                          *C
C*=======================--===                                        *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   copies a 4-vector to another 4-vector.                           *C
C* (Input)                                                            *C
C*       P1(4) = an input 4-vector to be copied.                      *C
C* (Output)                                                           *C
C*       P2(4) = a copy of P1.                                        *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================--===
      SUBROUTINE UCOPV4(P1,P2)
C =========================--===
C  
      DIMENSION P1(4),P2(4)
      DO 10 I=1,4
      P2(I)=P1(I)
   10 CONTINUE
      RETURN
      END
