C***********************************************************************
C*
C*  ---------------------------------------------============
C*  Subroutine UDPHMM( X, QMAX, QMIN, AMAS, AWID, QNOW, PHFACT)
C*  ---------------------------------------------============
C*(Function)
C*  Generate Q of intermidiate resonance.
C*  Same as UHPHMM, but double precision routine.
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
      SUBROUTINE UDPHMM( X, QMAX, QMIN, AMAS, AWID, QNOW, PHFACT)
C*
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X
      REAL*8 QMAX,QMIN,AMAS,AWID,QNOW,PHFACT
 
      REAL*8  PI
      PARAMETER (PI=3.14159265358)
      REAL*8  TWOPI
      PARAMETER (TWOPI=2.*PI)
 
      Q2MX   = QMAX*QMAX
      Q2MN   = QMIN*QMIN
      XMSQ   = AMAS*AMAS
      XMW    = AMAS*AWID
      THMX   = ATAN2( Q2MX - XMSQ, XMW )
      THMN   = ATAN2( Q2MN - XMSQ, XMW )
      TH     = THMN + (THMX-THMN)*X
      Q2     = XMSQ + XMW*TAN(TH)
      QNOW   = SQRT(Q2)
      PHFACT = (XMW*XMW+(Q2-XMSQ)**2)/XMW*(THMX-THMN)/TWOPI
 
      RETURN
      END
