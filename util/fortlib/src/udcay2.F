CC********************************************************************CC
C*                                                                    *C
C*==============================--===                                 *C
C*  Subroutine UDCAY2(P,AMD,RAN,PD)                                   *C
C*==============================--===                                 *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Simulates isotropic 2-body decay.                               *C
C* (Inputs)                                                           *C
C*       P  (4)  ; 4-momentum of parent particle.                     *C
C*       AMD(1)  ; mass value of particle 1.                          *C
C*       AMD(2)  ; mass value of particle 2.                          *C
C*       RAN(2)  ; 2 uniform random numbers.                          *C
C* (Output)                                                           *C
C*       PD(4,2) ; 4-momenta of daughter particles.                   *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       UDOT4, UPSTAR, USETRF, UBTRAN, USETV4, UBSTBK, USUB4         *C
C* (Update Record)                                                    *C
C*    7/29/85  K. Fujii  Original version.                            *C
C*                                                                    *C
CC********************************************************************CC
C  
C ================================--===
      SUBROUTINE UDCAY2(P,AMD,RAN,PD)
C ================================--===
C  
      DIMENSION P(4),AMD(2),RAN(2),PD(4,2),E(3,3)
      DATA TWOPI /6.28319/
C  
      AM    = UDOT4(P,P)
      AM    = SQRT(AM)
      APD   = UPSTAR(AM,AMD(1),AMD(2))
      COSTH = -1.+2.*RAN(1)
      SINTH =  1.-COSTH**2
      IF(SINTH.LT.0.) SINTH = 0.
      SINTH=SQRT(SINTH)
      PHI=TWOPI*RAN(2)
      PD(2,1)=APD*SINTH
      PD(1,1)=PD(2,1)*COS(PHI)
      PD(2,1)=PD(2,1)*SIN(PHI)
      PD(3,1)=APD*COSTH
      CALL USETRF(P,E)
      CALL UBTRAN(PD(1,1),E,PD(1,1))
      CALL USETV4(AMD(1),PD(1,1),PD(1,1))
      CALL UBSTBK(PD(1,1),P,PD(1,1))
      CALL USUB4(P,PD(1,1),PD(1,2))
      RETURN
      END
