C ====================================
C  Calculate 3 body phase space.
C ====================================
C
C
C
C *INCLUDE UHSETR
C *INCLUDE BOOST$
C
C       REAL*8  XDBL(8)
C       REAL*4  P1(0:3), P2(0:3), P3(0:3), P4(0:3)
 
C       ROTS    = 600.
C       XDBL(1) =  0.33978522418841613
C       XDBL(2) =  0.23385781172712523
C       XDBL(3) = -0.34829036099836222
C       XDBL(4) =  0.30142633873037998
C       XDBL(5) = -0.84336724947206676
C       XDBL(6) =  0.27477442659437659
C       XDBL(7) = -0.44707739399746055
C       XDBL(8) =  0.37580230110324925
 
C       AM1     = 1.E-3
C       AM2     = 1.E-3
C       AM3     = 1.E-3
C       AM4     = 1.E-3
C       AMV1    = 91.16
C       AWV1    = 2.5
C       AMV2    = 91.16
C       AWV2    = 2.5
 
C       CALL UHPH4B( XDBL, ROTS, AMV1, AWV1, AMV2, AWV2,
C     >              AM1, AM2, AM3, AM4, P1,P2,P3,P4, PHFACT)
 
C       PRINT *,' PHFACT=',PHFACT
C       PRINT *,' P1=',P1
C       PRINT *,' P2=',P2
C       PRINT *,' P3=',P3
C       PRINT *,' P4=',P4
 
C       PSUM0 = P1(0) + P2(0) + P3(0) + P4(0)
C        PSUM1 = P1(1) + P2(1) + P3(1) + P4(1)
C        PSUM2 = P1(2) + P2(2) + P3(2) + P4(2)
C        PSUM3 = P1(3) + P2(3) + P3(3) + P4(3)
C
C        PRINT *,' PSUM0=',PSUM0
C        PRINT *,' PSUM1=',PSUM1
C        PRINT *,' PSUM2=',PSUM2
C        PRINT *,' PSUM3=',PSUM3
C
C
C        STOP
C        END
C***********************************************************************
C*
C* ---------------------------------------------------
C*  Subroutine  UHPH3B( XDBL, ROTS,
C*                      AM1,AM2,AM3,P1,P2,P3,PHFACT)
C* --------------------------------===================
C*
C*(Function)
C*  Calculate 3 body phase space according to the Hagiwara's note.
C*
C*   X --> Q1 + V3 --> V1 + V2 + V3
C*
C*(Input)
C*  XDBL(5)  ; Double precision integration variables.
C*       XDBL(1)   = Invariant mass of Q1
C*           (2:3) = Cos(th) and Phi/2pi of (V3) system
C*           (4:5) = Cos(th)-1hat and Phi/2pi-1hat of V1 in Q1 system
C*(Output)
C*  P1(0:3) to P3(0:3) ; Four momentum of produced particle.
C*  PHFACT             ; Phase space factor.
C*            = (1./2pi)*(1./16pi)**2*BetaB(S,q3)*BetaB(M1,M2)
C*
C*(Author)
C*  A. Miyamoto  Original version   21-Jun-1990
C*
C***********************************************************************
C
      SUBROUTINE UHPH3B( XDBL, ROTS,
     >                               AM1, AM2, AM3,
     >                               P1,  P2,  P3,  PHFACT )
C
      REAL*8  XDBL(5)
      REAL*4  ROTS, AM1, AM2, AM3
      REAL*4  P1(0:3), P2(0:3), P3(0:3)
C
C
      REAL*4  Q1(0:3)
      REAL*4  P1H(0:3), P2H(0:3)
C
      REAL*4  E(3,3), PWRK(0:3)
C
C
C ... Constants.
      REAL*4  PI
      PARAMETER (PI=3.14159265358)
      REAL*4  TWOPI
      PARAMETER (TWOPI=2.*PI)
      REAL*4  SXTNPI
      PARAMETER (SXTNPI=16.*PI)
      REAL*4  PIFACT
      PARAMETER (PIFACT=1./(SXTNPI**2*TWOPI))
C
      BETABA(S,A,B) = DSQRT( 1.D0 - 2.D0*(DBLE(A)+DBLE(B))/DBLE(S)
     >              + ((DBLE(A)-DBLE(B))/DBLE(S))**2 )
C
C
C ======< Entry Point >=================================================
C
C
      PHFACT = 0
      IF( AM1+AM2+AM3 .GE. ROTS ) RETURN
C
      S      = ROTS*ROTS
      Q1MAX  = (ROTS - AM3)**2
      Q1MIN  = (AM1+AM2)**2
      Q1SQ   = Q1MIN + (Q1MAX-Q1MIN)*XDBL(1)
      Q1RT   = SQRT( Q1SQ )
 
C
C ... 4 momentum of quasi-two body system.
C
      AM3SQ  = AM3*AM3
      BQSA   = BETABA( S, Q1SQ, AM3SQ )
      BQS    = ROTS*0.5*BQSA
      QEF    = (AM3SQ-Q1SQ)/S
      SINTH = SQRT( (1.-XDBL(2))*(1.+XDBL(2)) )
      PHI   = XDBL(3)*2.*PI
C
C     P3(0) = 0.5*ROTS*(1.+QEF)
C     P3(1) = BQS*SINTH*COS(PHI)
C     P3(2) = BQS*SINTH*SIN(PHI)
C     P3(3) = BQS*XDBL(2)
 
C     Q1(0) = 0.5*ROTS*(1.-QEF)
C     Q1(1) = -P3(1)
C     Q1(2) = -P3(2)
C     Q1(3) = -P3(3)
C
      Q1(0) = 0.5*ROTS*(1.-QEF)
      Q1(1) = BQS*SINTH*COS(PHI)
      Q1(2) = BQS*SINTH*SIN(PHI)
      Q1(3) = BQS*XDBL(2)
 
      P3(0) = 0.5*ROTS*(1.+QEF)
      P3(1) = -Q1(1)
      P3(2) = -Q1(2)
      P3(3) = -Q1(3)
 
C
C   Q1 --> AM1 + AM2 system
C ... 4 momentum of decayed two particle at its CMS.
C
      AM1S = AM1*AM1
      AM2S = AM2*AM2
      B1SA = BETABA( Q1SQ, AM1S, AM2S)
      B1S  = Q1RT*0.5*B1SA
      QEF  = ((AM1-AM2)*(AM1+AM2) )/Q1SQ
      SINTH = SQRT( (1.-XDBL(4))*(1.+XDBL(4)) )
      PHI   = XDBL(5)*2.*PI
C
      P1H(0) = 0.5*Q1RT*(1.+QEF)
      P1H(1) = B1S*SINTH*COS(PHI)
      P1H(2) = B1S*SINTH*SIN(PHI)
      P1H(3) = B1S*XDBL(4)
C
      P2H(0) = 0.5*Q1RT*(1.-QEF)
      P2H(1) = -P1H(1)
      P2H(2) = -P1H(2)
      P2H(3) = -P1H(3)
 
C
C ... Rotate and Boosts to translate to lab. system
C
      CALL UHSETR( Q1(1),  E )
      CALL UBTRAN( P1H(1), E, PWRK(1))
      PWRK(0) = P1H(0)
      CALL BOOSTX( PWRK, Q1, P1 )
 
      CALL UBTRAN( P2H(1), E, PWRK(1))
      PWRK(0) = P2H(0)
      CALL BOOSTX( PWRK, Q1, P2 )
 
C
      PHFACT =   (Q1MAX-Q1MIN)*BQSA*B1SA*PIFACT
C
      RETURN
      END
