CC********************************************************************CC
C*                                                                    *C
C*========================-----====                                   *C
C*  Subroutine UPTPL(P,PR,PT,PL)                                      *C
C*========================-----====                                   *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to calculate PT and PL of P with PR as a reference.     *C
C* (Inputs)                                                           *C
C*       P (3) = 3-vector in question.                                *C
C*       PR(3) = reference 3-vector.                                  *C
C* (Outputs)                                                          *C
C*       PT    = transverse component of P(3).                        *C
C*       PL    = longitudinal component of P(3).                      *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       UPTPLV                                                       *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii   Original version.                            *C
C*                                                                    *C
CC********************************************************************CC
C  
C ==========================-----====
      SUBROUTINE UPTPL(P,PR,PT,PL)
C ==========================-----====
C  
      DIMENSION P(3),PR(3),PLV(3),PTV(3)
      CALL UPTPLV(P,PR,PTV,PLV,PT,PL)
      RETURN
      END
