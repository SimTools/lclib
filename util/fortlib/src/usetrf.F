CC********************************************************************CC
C*                                                                    *C
C* ====================---===                                         *C
C*  Subroutine USETRF(P,E)                                            *C
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
C*        12/02/93 TKSF Protection for -P along Z.                    *C
C*                                                                    *C
CC********************************************************************CC
C  
C* ======================---===
      SUBROUTINE USETRF(P,E)
C* ======================---===
C  
      DIMENSION E(3,3),E3(3),P(3)
C  
      E3(1) = 0.0
      E3(2) = 0.0
      E3(3) = 1.0
C  
      CALL UNRMV3(P,E(1,3))
C  
      IF ( ABS(E(3,3)-1.0) .LT. 1.0E-6 ) THEN
        E(1,1) = 1.0
        E(2,1) = 0.0
        E(3,1) = 0.0
      ELSE IF ( ABS(E(3,3)+1.0) .LT. 1.0E-6 ) THEN
        E(1,1) =-1.0
        E(2,1) = 0.0
        E(3,1) = 0.0
      ELSE
        CALL UNCROS(E3(1),E(1,3),E(1,1))
      ENDIF
C  
      CALL UNCROS(E(1,3),E(1,1),E(1,2))
C  
      RETURN
      END
