CC********************************************************************CC
C*                                                                    *C
C*========================--===                                       *C
C*  Subroutine UXFYN(P,PR,XF)                                         *C
C*========================--===                                       *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to calculate Feynman X of P in PR-rest frame.           *C
C* (Inputs)                                                           *C
C*      P (4) = 4-momentum of a particle.                             *C
C*      PR(4) = reference 4-momentum.                                 *C
C* (Inputs)                                                           *C
C*      XF    = Feynman X.                                            *C
C* (Relation)                                                         *C
C*   Calls                                                            *C
C*      UBSTFD, UPTPL, UDOT4                                          *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii   Original version.                            *C
C*                                                                    *C
CC********************************************************************CC
C  
C ==========================--===
      SUBROUTINE UXFYN(P,PR,XF)
C ==========================--===
C  
      DIMENSION P(4),PR(4),PCM(4)
      CALL UBSTFD(P,PR,PCM)
      CALL UPTPL(PCM,PR,PT,PL)
      S=UDOT4(PR,PR)
      S=SQRT(S)
      XF=2*PL/S
      RETURN
      END
