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
C*  Subroutine  UHPH4B( XDBL, ROTS, AMV1, AWV1, AMV2, AWV2,
C*                      AM1,AM2,AM3,AM4, P1,P2,P3,P4 ,PHFACT)
C* --------------------------------===================
C*
C*(Function)
C*  Calculate 4 body phase space according to the Hagiwara's note.
C*
C*   X --> V1 + V2
C*         !     +--> F3 + F4
C*         +--------> F1 + F2
C*
C*(Input)
C*  XDBL(8)  ; Double precision integration variables.
C*       XDBL(1:2) = X1 and X2
C*           (3:4) = Cos(th) and Phi/2pi of (AM1,AM2) system
C*           (5:6) = Cos(th)-1hat and Phi/2pi-1hat
C*           (7:8) = Cos(th)-2hat and Phi/2pi-2hat
C*(Output)
C*  P1(0:3) to P4(0:3) ; Four momentum of produced particle.
C*  PHFACT             ; Phase space factor.
C*            = (1./16pi)**3*BetaB(q1,q2)*BetaB(M1,M2)*BetaB(M3,M4)
C*
C*(Author)
C*  A. Miyamoto  Original version   14-Jun-1990
C*
C***********************************************************************
C
      SUBROUTINE UHPH4B( XDBL, ROTS, AMV1, AWV1, AMV2, AWV2,
     >                               AM1, AM2, AM3, AM4,
     >                               P1,  P2,  P3,  P4 , PHFACT )
C
      REAL*8  XDBL(8)
      REAL*4  ROTS, AM1, AM2, AM3, AM4
      REAL*4  AMV1, AWV1, AMV2, AWV2
      REAL*4  P1(0:3), P2(0:3), P3(0:3), P4(0:3)
C
C
      REAL*4  Q1(0:3),  Q2(0:3)
      REAL*4  P1H(0:3), P2H(0:3), P3H(0:3), P4H(0:3)
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
      PARAMETER (PIFACT=1./(SXTNPI**3*TWOPI**2))
C
      BETABA(S,A,B) = DSQRT( 1.D0 - 2.D0*(DBLE(A)+DBLE(B))/DBLE(S)
     >              + ((DBLE(A)-DBLE(B))/DBLE(S))**2 )
C
C
C ======< Entry Point >=================================================
C
C
*     PRINT *,' XDBL=',(XDBL(I),I=1,4)
*     PRINT *,'      ',(XDBL(I),I=5,8)
      PHFACT = 0.
 
      S      = ROTS*ROTS
#ifdef C
   TH1MIN = ATAN( -AMV1/AWV1 )
#endif 
#ifdef C
   TH1MAX = ATAN( (S-AMV1*AMV1)/(AMV1*AWV1) )
#endif 
      TH1MIN = ATAN( (AM1+AM2+AMV1)*(AM1+AM2-AMV1)/(AMV1*AWV1) )
      TH1MAX = ATAN( (ROTS-AM1-AM2-AM3-AM4+AMV1)*
     >               (ROTS-AM1-AM2-AM3-AM4-AMV1)/(AMV1*AWV1) )
      TH1    = TH1MIN + (TH1MAX-TH1MIN)*XDBL(1)
      Q1SQ   = AMV1*AMV1 + AMV1*AWV1*TAN(TH1)
      Q1RT   = SQRT( Q1SQ )
 
#ifdef C
   TH2MIN = ATAN( -AMV2/AWV2 )
#endif 
#ifdef C
   TH2MAX = ATAN( (ROTS-Q1RT-AMV2)*(ROTS-Q1RT+AMV2) / (AMV2*AWV2) )
#endif 
      TH2MIN = ATAN( (AM3+AM4+AMV2)*(AM3+AM4-AMV2)/(AMV2*AWV2) )
      TH2MAX = ATAN( (ROTS-Q1RT-AMV2)*(ROTS-Q1RT+AMV2) / (AMV2*AWV2) )
      TH2    = TH2MIN + (TH2MAX-TH2MIN)*XDBL(2)
      Q2SQ   = AMV2*AMV2 + AMV2*AWV2*TAN(TH2)
      Q2RT   = SQRT( Q2SQ )
 
*     PRINT *,' TH1=',TH1,' TH2=',TH2
*     PRINT *,' Q1SQ=',Q1SQ,' Q2SQ=',Q2SQ
      IF( Q1RT+Q2RT .GE. ROTS ) RETURN
 
C
C ... 4 momentum of quasi-two body system.
C
#ifdef X
      PRINT *,'TH1MIN,TH1MAX=',TH1MIN,TH1MAX,' TH1=',TH1
#endif 
#ifdef X
      PRINT *,'TH2MIN,TH2MAX=',TH2MIN,TH2MAX,' TH2=',TH2
#endif 
#ifdef X
      PRINT *,'Q1SQ=',Q1SQ,' Q2SQ=',Q2SQ
#endif 
      BQSA = BETABA( S, Q1SQ, Q2SQ )
      BQS  = ROTS*0.5*BQSA
      QEF  = (Q1SQ-Q2SQ)/S
      SINTH = SQRT( (1.-XDBL(3))*(1.+XDBL(3)) )
      PHI   = XDBL(4)*2.*PI
C
      Q1(0) = 0.5*ROTS*(1.+QEF)
      Q1(1) = BQS*SINTH*COS(PHI)
      Q1(2) = BQS*SINTH*SIN(PHI)
      Q1(3) = BQS*XDBL(3)
 
#ifdef X
      TQ1SQ = Q1(0)*Q1(0) - Q1(1)*Q1(1) - Q1(2)*Q1(2) - Q1(3)*Q1(3)
#endif 
#ifdef X
      PRINT *,' TQ1SQ=',TQ1SQ
#endif 
C
      Q2(0) = 0.5*ROTS*(1.-QEF)
      Q2(1) = -Q1(1)
      Q2(2) = -Q1(2)
      Q2(3) = -Q1(3)
 
C
C   Z1 system
C ... 4 momentum of decayed two particle at its CMS.
C
      Q1RT = SQRT(Q1SQ)
      AM1S = AM1*AM1
      AM2S = AM2*AM2
      IF( AM1 +AM2  .GE. Q1RT ) RETURN
      B1SA = BETABA( Q1SQ, AM1S, AM2S)
      B1S  = Q1RT*0.5*B1SA
      QEF  = ((AM1-AM2)*(AM1+AM2) )/Q1SQ
      SINTH = SQRT( (1.-XDBL(5))*(1.+XDBL(5)) )
      PHI   = XDBL(6)*2.*PI
C
      P1H(0) = 0.5*Q1RT*(1.+QEF)
      P1H(1) = B1S*SINTH*COS(PHI)
      P1H(2) = B1S*SINTH*SIN(PHI)
      P1H(3) = B1S*XDBL(5)
C
      P2H(0) = 0.5*Q1RT*(1.-QEF)
      P2H(1) = -P1H(1)
      P2H(2) = -P1H(2)
      P2H(3) = -P1H(3)
 
C
C ... Rotate and Boosts to translate to lab. system
C
      CALL UHSETR( Q1(1),  E )
*     PRINT *,' E=',(E(I,1),I=1,3)
*     PRINT *,'   ',(E(I,2),I=1,3)
*     PRINT *,'   ',(E(I,3),I=1,3)
      CALL UBTRAN( P1H(1), E, PWRK(1))
      PWRK(0) = P1H(0)
      CALL BOOSTX( PWRK, Q1, P1 )
C     CALL PSUB00( Q1, P1, P2 )
 
*     PRINT *,' E=',(E(I,1),I=1,3)
*     PRINT *,'   ',(E(I,2),I=1,3)
*     PRINT *,'   ',(E(I,3),I=1,3)
      CALL UBTRAN( P2H(1), E, PWRK(1))
      PWRK(0) = P2H(0)
      CALL BOOSTX( PWRK, Q1, P2 )
 
      PWRKSQ = PWRK(0)*PWRK(0) - PWRK(1)*PWRK(1)
     >       - PWRK(2)*PWRK(2) - PWRK(3)*PWRK(3)
      P1HSQ = P1H(0)*P1H(0)-P1H(1)*P1H(1)-P1H(2)*P1H(2)-P1H(3)*P1H(3)
      P2HSQ = P2H(0)*P2H(0)-P2H(1)*P2H(1)-P2H(2)*P2H(2)-P2H(3)*P2H(3)
      P1SQ  = P1 (0)*P1 (0)-P1 (1)*P1 (1)-P1 (2)*P1 (2)-P1 (3)*P1(3)
      P2SQ  = P2 (0)*P2 (0)-P2 (1)*P2 (1)-P2 (2)*P2 (2)-P2 (3)*P2(3)
*     PRINT *,' P1HSQ=',P1HSQ,' P2HSQ=',P2HSQ
*     PRINT *,' P1SQ,P2SQ=',P1SQ,P2SQ
*     PRINT *,' Q1SQ=',Q1SQ,' PWRKSQ=',PWRKSQ
 
      PSUM0 = Q1(0) - P1(0) - P2(0)
      PSUM1 = Q1(1) - P1(1) - P2(1)
      PSUM2 = Q1(2) - P1(2) - P2(2)
      PSUM3 = Q1(3) - P1(3) - P2(3)
*     PRINT *,' PSUM0=',PSUM0
*     PRINT *,' PSUM1=',PSUM1
*     PRINT *,' PSUM2=',PSUM2
*     PRINT *,' PSUM3=',PSUM3
C
C
C   Z2 system
C ... 4 momentum of decayed two particle at its CMS.
C
      Q2RT = SQRT(Q2SQ)
      AM3S = AM3*AM3
      AM4S = AM4*AM4
      IF( AM3 +AM4  .GE. Q2RT ) RETURN
      B2SA = BETABA( Q2SQ, AM3S, AM4S )
      B2S  = Q2RT*0.5*B2SA
      QEF  = ((AM3-AM4)*(AM3+AM4) )/Q2SQ
      SINTH = SQRT( (1.-XDBL(7))*(1.+XDBL(7)) )
      PHI   = XDBL(8)*2.*PI
C
      P3H(0) = 0.5*Q2RT*(1.+QEF)
      P3H(1) = B2S*SINTH*COS(PHI)
      P3H(2) = B2S*SINTH*SIN(PHI)
      P3H(3) = B2S*XDBL(7)
C
      P4H(0) = 0.5*Q2RT*(1.-QEF)
      P4H(1) = -P3H(1)
      P4H(2) = -P3H(2)
      P4H(3) = -P3H(3)
 
C
C ... Rotate and Boosts to translate to lab. system
C
      CALL UHSETR( Q2(1),  E )
      CALL UBTRAN( P3H(1), E, PWRK(1))
      PWRK(0) = P3H(0)
      CALL BOOSTX( PWRK, Q2, P3 )
C     CALL PSUB00( Q2, P3, P4 )
 
      CALL UBTRAN( P4H(1), E, PWRK(1))
      PWRK(0) = P4H(0)
      CALL BOOSTX( PWRK, Q2, P4 )
 
      P3HSQ = P3H(0)*P3H(0)-P3H(1)*P3H(1)-P3H(2)*P3H(2)-P3H(3)*P3H(3)
C     P4HSQ = PH4(0)*PH4(0)-PH4(1)*PH4(1)-PH4(2)*PH4(2)-PH4(3)*PH4(3)
*     PRINT *,' P3HSQ=',P3HSQ
C
      PHFACT =   (TH1MAX-TH1MIN)*(TH2MAX-TH2MIN)
     >       * ( (AMV1*AWV1)**2+(Q1SQ-AMV1*AMV1)**2 )/ (AMV1*AWV1)
     >       * ( (AMV2*AWV2)**2+(Q2SQ-AMV2*AMV2)**2 )/ (AMV2*AWV2)
     >       *    BQSA*B1SA*B2SA*PIFACT
C
      RETURN
      END
