CC********************************************************************CC
C*                                                                    *C
C*=========================---===                                     *C
C*  Subroutine UPHIAN(P,PR,PHI)                                       *C
C*=========================---===                                     *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates phi-angle of P measured from the axis normal to      *C
C*    the Z-PR plane with PR as a polar axis.                         *C
C* (Inputs)                                                           *C
C*       P (3)   :  a 3-vector.                                       *C
C*       PR(3)   :  a 3-vector defining a polar axis.                 *C
C* (Inputs)                                                           *C
C*       PHI     :  phi measured from the normal to the Z-PR plane    *C
C*                  with PR as a polar axis.                          *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       UNCROS, UDOT3                                                *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C ===========================---===
      SUBROUTINE UPHIAN(P,PR,PHI)
C ===========================---===
C  
      DIMENSION P(3),PR(3),E(3),E1(3),E2(3)
      DATA  E /0.,0.,1./
C  
      CALL UNCROS(PR,E,E1)
      CALL UNCROS(PR,E1,E2)
      PX  = UDOT3(E1,P)
      PY  = UDOT3(E2,P)
      PHI = ATAN2(PY,PX)
C  
      RETURN
      END
