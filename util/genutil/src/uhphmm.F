C***********************************************************************
C*
C*  ---------------------------------------------============
C*  Subroutine UHPHMM( X, QMAX, QMIN, AMAS, AWID, QNOW, PHFACT)
C*  ---------------------------------------------============
C*(Function)
C*  Generate Q of intermidiate resonance.
C*(Input)
C*  X     ; Real*8 variable for integration
C*  QMAX  ; Maximum Q value.
C*  QMIN  ; Minimum Q value.
C*  AMAS  ; Mass of intermidiate particle.
C*  AWID  ; Width of intermidiate particle.
C*(Output)
C*  QNOW  ; Generated Q
C*  PHFACT ; Phase space factor for this integration.
C*       1/2pi*( (M*G)**2 + (Q*Q-M*M)**2)*(THMAX-THMIN)/(M*G) )
C*(Auhtor)
C*  A. Miyamoto  21 Aug 1990
C*
C***********************************************************************
C*
      SUBROUTINE UHPHMM( X, QMAX, QMIN, AMAS, AWID, QNOW, PHFACT)
C*
      REAL*8 X
      REAL*4 QMAX,QMIN,AMAS,AWID,QNOW,PHFACT
 
      REAL*4  PI
      PARAMETER (PI=3.14159265358)
      REAL*4  TWOPI
      PARAMETER (TWOPI=2.*PI)
 
      IF( AWID .LT. 0.E0 ) THEN
        QNOW = AMAS
#ifdef C
     PHFACT = 1./TWOPI
#endif 
        PHFACT = 0.5/ABS(AMAS*AWID)
        RETURN
      ENDIF
C
      Q2MX   = QMAX*QMAX
      Q2MN   = QMIN*QMIN
      XMSQ   = AMAS*AMAS
      XMW    = AMAS*AWID
      THMX   = ATAN2( Q2MX - XMSQ, XMW )
      THMN   = ATAN2( Q2MN - XMSQ, XMW )
      TH     = THMN + (THMX-THMN)*X
      TANTH  = TAN(TH)
      Q2     = XMSQ + XMW*TANTH
#ifdef C
   Q2     = XMSQ + XMW*TAN(TH)
#endif 
      QNOW   = SQRT(Q2)
#ifdef C
   PHFACT = (XMW*XMW+(Q2-XMSQ)**2)/XMW*(THMX-THMN)/TWOPI
#endif 
      PHFACT = XMW*(1.+TANTH**2)*(THMX-THMN)/TWOPI
 
      RETURN
      END
