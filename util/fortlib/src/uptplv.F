CC********************************************************************CC
C*                                                                    *C
C*=========================-------------====                          *C
C*  Subroutine UPTPLV(P,PR,PTV,PLV,PT,PL)                             *C
C*=========================-------------====                          *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to calculate transverse vector PTV and longitudinal     *C
C*    vector PLV as PR as a reference. PT and PL are their absolute   *C
C*    values.                                                         *C
C* (Inputs)                                                           *C
C*       P (3)   = 3-vector in question.                              *C
C*       PR(3)   = reference 3-vector.                                *C
C* (Outputs)                                                          *C
C*       PTV(3)  = transverse vector of P(3).                         *C
C*       PLV(3)  = longitudinal vector of P(3).                       *C
C*       PT      = transverse component of P(3).                      *C
C*       PL      = longitudinal component of P(3).                    *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       UDOT3, USCLM3, USUB3.                                        *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii   Original version.                            *C
C*                                                                    *C
CC********************************************************************CC
C  
C ===========================-------------====
      SUBROUTINE UPTPLV(P,PR,PTV,PLV,PT,PL)
C ===========================-------------====
C  
      DIMENSION P(3),PR(3),PTV(3),PLV(3)
      A=UDOT3(P,PR)
      IF(A.EQ.0.) GO TO 10
      APR=UDOT3(PR,PR)
      PL=A/SQRT(APR)
      A=A/APR
      CALL USCLM3(A,PR,PLV)
      CALL USUB3(P,PLV,PTV)
      PT=UDOT3(PTV,PTV)
      PT=SQRT(PT)
      RETURN
C  
   10 DO 20 I=1,3
      PLV(I)=0.
      PTV(I)=P(I)
   20 CONTINUE
      PL=0.
      PT=SQRT(UDOT3(P,P))
      RETURN
      END
