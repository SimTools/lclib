CC********************************************************************CC
C*                                                                    *C
C*=======================--====                                       *C
C*  Subroutine UCONJ4(P1,P2)                                          *C
C*=======================--====                                       *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   Take Lorentz conjugate.                                          *C
C* (Input)                                                            *C
C*       P1(4) = an input 4-vector.                                   *C
C* (Output)                                                           *C
C*       P2(4) = its Lorentz conjugated 4-vector                      *C
C* (Relation)                                                         *C
C*   Calls                                                            *C
C*       USCLM3                                                       *C
C* (Update Record)                                                    *C
C*    7/29/85  K. Fujii  Original version.                            *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================--====
      SUBROUTINE UCONJ4(P1,P2)
C =========================--====
C  
      DIMENSION P1(4),P2(4)
      P2(4)=P1(4)
      CALL USCLM3(-1.,P1(1),P2(1))
      RETURN
      END
