CC**********************************************************************
C*
C*========================--==
C* Subroutine UDSETF(P,EB,EA)
C*========================--==
C*
C* (Purpose)
C*    Setups a reference frame with EA(*,3) along P and EA(*,2)
C*    perpendicular to P-EB(*,3) plane.
C* (Input)
C*       P (3)   : (R*4) : direction vector of new polar axis.
C*       EB(*,i) : (R*4) : i-th axis of the old reference frame.
C* (Output)
C*       EA(*,i) : (R*4) : i-th axis of the new reference frame.
C* (Relation)
C*    Calls UNRMV3, UNCROS, UVCOPY, UVZERO, and UABSV3.
C* (Update Record)
C*    6/22/90  K.Fujii      Derived from USETRF in 'T#PB.FORTLIB.FORT'.
C*
CC**********************************************************************
 
      SUBROUTINE UDSETF(P,EB,EA)
 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8     P(3), EB(3,3), EA(3,3)
      REAL*8     E(3,3), E1(3)
      DATA XMIN  / 1.D-6 /
C
C========< Entry Point >================================================
C
C--
C  Set new Z-axis.
C--
      PA = 1.D0/SQRT( P(1)*P(1) + P(2)*P(2) + P(3)*P(3) )
      E(1,3) = PA*P(1)
      E(2,3) = PA*P(2)
      E(3,3) = PA*P(3)
C--
C  Set new Y-axis.
C--
#ifdef C
   CSTH  = UDOT3(E(1,3),EB(1,3))
#endif 
      CSTH  = E(1,3)*EB(1,3) + E(2,3)*EB(2,3) + E(3,3)*EB(3,3)
      E1(1) = E(1,3) - CSTH*EB(1,3)
      E1(2) = E(2,3) - CSTH*EB(2,3)
      E1(3) = E(3,3) - CSTH*EB(3,3)
#ifdef C
   AE1   = UABSV3(E1)
#endif 
      AE1   = SQRT( E1(1)*E1(1) + E1(2)*E1(2) + E1(3)*E1(3) )
C--
      IF ( AE1.LT.XMIN ) THEN
#ifdef C
      CALL UVCOPY(9,EB,EA)
#endif 
         DO 10 I = 1, 3
         DO 10 J = 1, 3
           EA(I,J) = EB(I,J)
10       CONTINUE
         RETURN
      ELSE
#ifdef C
      CALL UNCROS(EB(1,3),E1(1),E(1,2))
#endif 
C
         E(1,2) = EB(2,3)*E1(3) - EB(3,3)*E1(2)
         E(2,2) = EB(3,3)*E1(1) - EB(1,3)*E1(3)
         E(3,2) = EB(1,3)*E1(2) - EB(2,3)*E1(1)
         SCL    = 1./SQRT( E(1,2)**2 + E(2,2)**2 + E(3,2)**2 )
         E(1,2) = SCL*E(1,2)
         E(2,2) = SCL*E(2,2)
         E(3,2) = SCL*E(3,2)
C
      ENDIF
C--
C  Set new X-axis.
C--
#ifdef C
   CALL UNCROS(E(1,2),E(1,3),E(1,1))
#endif 
C
         E(1,1) = E(2,2)*E(3,3) - E(3,2)*E(2,3)
         E(2,1) = E(3,2)*E(1,3) - E(1,2)*E(3,3)
         E(3,1) = E(1,2)*E(2,3) - E(2,2)*E(1,3)
         SCL    = 1./SQRT( E(1,1)**2 + E(2,1)**2 + E(3,1)**2 )
         E(1,1) = SCL*E(1,1)
         E(2,1) = SCL*E(2,1)
         E(3,1) = SCL*E(3,1)
C
C--
C  Store E in EA.
C--
#ifdef C
   CALL UVCOPY(9,E,EA)
#endif 
      DO 30 I = 1, 3
      DO 30 J = 1, 3
        EA(I,J) = E(I,J)
30    CONTINUE
C--
C  That's it.
C--
      RETURN
      END
