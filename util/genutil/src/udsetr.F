CC********************************************************************CC
C*                                                                    *C
C* ====================---===                                         *C
C*  Subroutine UDSETR(P,E)                                            *C
C* ====================---===                                         *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Setups a reference frame with E(*,3) along P and E(*,1)     *C
C*        perpendicular to P-E3(*) plane.                             *C
C*        (i.e. E(*,1) in E1-E2 plane)                                *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          P(3)   ; direction vector of polar axis.                  *C
C*        Output                                                      *C
C*          E(*,I) ; I-th axis of the reference frame                 *C
C*    3) Relation                                                     *C
C*        Calls                                                       *C
C*          UNRMV3, UNCROS                                            *C
C*    4) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C
C* ======================---===
      SUBROUTINE UDSETR(P,E)
C* ======================---===
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION E(3,3),E3(3),P(3)
      DATA      XMIN/1.D-10/
C
      E3(1) = 0.0D0
      E3(2) = 0.0D0
      E3(3) = 1.0D0
C
      PXY2  = P(1)*P(1) + P(2)*P(2)
C
C      CALL UNRMV3(P,E(1,3))
      PXI = 1.D0/SQRT(PXY2+P(3)*P(3))
      E(1,3) = PXI*P(1)
      E(2,3) = PXI*P(2)
      E(3,3) = PXI*P(3)

C
      IF ( PXY2 .LT. XMIN ) THEN
        E(1,2) = 0.0
        E(2,2) = 1.0
        E(3,2) = 0.0
      ELSE
        PXYN = 1./SQRT(PXY2)
        E(1,2) = -PXYN*P(2)
        E(2,2) =  PXYN*P(1)
        E(3,2) = 0.0
      ENDIF
C
C      CALL UNCROS(E(1,2),E(1,3),E(1,1))
C      E(1,1) = E(2,2)*E(3,3) - E(3,2)*E(2,3)
C      E(2,1) = E(3,2)*E(1,3) - E(1,2)*E(3,3)
C      E(3,1) = E(1,2)*E(2,3) - E(2,2)*E(1,3)
      E(1,1) =   E(2,2)*E(3,3) 
      E(2,1) = - E(1,2)*E(3,3)
      E(3,1) =   E(1,2)*E(2,3) - E(2,2)*E(1,3)
      EAI = 1.D0/SQRT(E(1,1)**2 + E(2,1)**2+E(3,1)**2)
      E(1,1) = EAI*E(1,1)
      E(2,1) = EAI*E(2,1)
      E(3,1) = EAI*E(3,1)
C
      RETURN
      END
