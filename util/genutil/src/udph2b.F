C***********************************************************************
C*
C* ---------------------------------------------------
C*  Subroutine  UDPH2B( XCOS, XPHI, PINI, EINI,
C*                      AM0, AM1, AM2,P1,P2, PHFACT )
C* --------------------------------===================
C*
C*(Function)
C*  Calculate 2 body phase space. To decay particle including width
C*
C*   PINI --> P1 + P2
C*
C*   Double precision version.
C*
C*(Input)
C*  XCOS     ; Integral variable for decay cos(th) in PINI rest
C*             frame.  -1 < XCOS < 1
C*  XPHI     ; INtegral variable for decay phi in PINI rest frame.
C*                      0 < XPHI < 1
C*
C*  PINI(0:3) ; 4 momentum of parent particle.
C*              PINI(0) < 0 for initial CM system (no boost.)
C*  EINI(i,3) ; i-th axis of old reference frame.
C*  AM0       ; Mass of parent particle.
C*  AM1       ; Mass of decay daughter 1.
C*  AM2       ; Mass of decay daughter 2.
C*
C*(Output)
C*  P1(0:3) to P2(0:3) ; Four momentum of produced particle.
C*                       momentum in lab. system.
C*  PHFACT             ; Phase space factor.
C*            = (1./16pi)*BetaB(M1,M2))
C*
C*(Author)
C*  A. Miyamoto  Original version   20-Aug-1990
C*
C***********************************************************************
C
      SUBROUTINE UDPH2B( XCOS, XPHI, PINI, EINI,
     >                   AM0, AM1, AM2, P1, P2, PHFACT )
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8  XCOS, XPHI
      REAL*8  PINI(0:3), AM1,AM2
      REAL*8  P1(0:3), P2(0:3), PHFACT
C
      REAL*8  Q1(0:3), Q2(0:3)
#ifdef C
   REAL*8  EINI(3,3), E(3,3), PWRK(0:3)
#endif 
      REAL*8  EINI(3,3), PWRK(0:3)
C
C
C ... Constants.
      REAL*8  PI
      PARAMETER (PI=3.14159265358)
      REAL*8  TWOPI
      PARAMETER (TWOPI=2.*PI)
      REAL*8  SXTNPI
      PARAMETER (SXTNPI=16.*PI)
      REAL*8  PIFACT
      PARAMETER (PIFACT=1./SXTNPI)
C
#ifdef C
   BETABA(S,A,B) = DSQRT( 1.D0 - 2.D0*(DBLE(A)+DBLE(B))/DBLE(S)
#endif 
#ifdef C
  >              + ((DBLE(A)-DBLE(B))/DBLE(S))**2 )
#endif 
C
C ======< Entry Point >=================================================
C
      PHFACT = 0.D0
      IF( AM0 .LE. AM1 + AM2 ) RETURN
      X1 = (AM1/AM0)**2
      X2 = (AM2/AM0)**2
      AM0S   = AM0*AM0
      AM1S   = AM1*AM1
      AM2S   = AM2*AM2
      X1MX2  = (AM1-AM2)*(AM1+AM2)/AM0S
      BETASQ = 1. - 2.*(AM1S+AM2S)/AM0S + X1MX2**2
      IF( BETASQ .LE. 0. ) RETURN
      BETA   = SQRT(BETASQ)
C     PRINT *,'UHPH2B...AM0,AM1,AM2=',AM0,AM1,AM2
C     PRINT *,'         X1,X2,BETA =',X1,X2,BETA
C
C ... Set momentum in AM0 rest frame.
C
      PHI    = XPHI*TWOPI
      XSIN2  = (1.-XCOS)*(1.+XCOS)
      IF( XSIN2 .LE. 0.D0 ) THEN
        XSIN  = 0.
      ELSE
        XSIN  = SQRT(XSIN2)
        IF( XSIN .GT. 1.0D0 ) XSIN = 1.0D0
      ENDIF
      AMHLF  = 0.5*AM0
      Q1(0)  = AMHLF*(1.+X1MX2)
      Q1(1)  = AMHLF*BETA*XSIN*COS(PHI)
      Q1(2)  = AMHLF*BETA*XSIN*SIN(PHI)
      Q1(3)  = AMHLF*BETA*XCOS
      Q2(0)  = AMHLF*(1.-X1MX2)
      Q2(1)  = -Q1(1)
      Q2(2)  = -Q1(2)
      Q2(3)  = -Q1(3)
C     PRINT *,' Q1=',Q1
C     PRINT *,' Q2=',Q2
C     PRINT *,' PINI=',PINI
C
C
C ... Translate to lab. system
C
      IF( PINI(0) .GT. 0. ) THEN
#ifdef CC
    CALL UHSETF( PINI(1), EINI, E )
#endif 
        CALL UDTRAN( Q1(1), EINI, PWRK(1) )
        PWRK(0) = Q1(0)
C       PRINT *,' PWRK=',PWRK
        CALL BOOSTD( PWRK, PINI, P1 )
C       PRINT *,' P1  =',P1
 
        CALL UDTRAN( Q2(1), EINI, PWRK(1))
        PWRK(0) = Q2(0)
C       PRINT *,' PWRK=',PWRK
        CALL BOOSTD( PWRK, PINI, P2 )
C       PRINT *,' P2  =',P2
      ELSE
        DO 300 I = 0, 3
          P1(I) = Q1(I)
          P2(I) = Q2(I)
300     CONTINUE
      ENDIF
C
      PHFACT = PIFACT*BETA
C
      RETURN
      END
