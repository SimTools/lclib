C***********************************************************************
C*=======================                                              *
C* FUNCTION BEAMFN*8(EPS)                                              *
C*=======================                                              *
C*                                                                     *
C*     This routine calculates a value of the distribution function    *
C*     at the energy ratio eps( = E/E0 ).                              *
C*     To be used for Beamstrahlung ( togather with BEAMIN and BEAMGN) *
C*                                                                     *
C*   Coded by S. Kawabata  April '89                                   *
C*   A.Miyamoto  3-May-1990  Change function name                      *
C*                                                                     *
C***********************************************************************
C
C------------------------------------------------------------------
C
      REAL*8 FUNCTION BEAMFN( EPS, TAU )
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     REAL*8 NG,NCL,N1,N,KSI1,EPS,TAU
C     COMMON/CNST/ PI,ALF,RE,CONST
C     COMMON/PARM/ E0,N,SIGX,SIGY,SIGZ,NG,NCL,KSI1,WIDTH
      REAL*8   EPS, TAU
      include 'beamcm.inc'
c      INCLUDE (@BEAMCM)
C     T#FP.GENLIB.COM
C     DATA IFL / 0/
C
      IF( EPS .GT. 1.D0 ) THEN
          BEAMFN = 0.D0
      ELSE
          EPSM  = 1.D0 - EPS
          Y     = EPSM/(KSI1*EPS)
          YKSI  = KSI1*Y
          N1    = (NCL + YKSI*NG)/(1.D0 + YKSI)
          X     = N1*TAU*Y**(1.D0/3.D0)
          X3    = X/3.D0
C
          H    = CONST*(SQRT(X3)/(1.D0+0.53D0*X**(-5.D0/6.D0)))**0.75D0
     .                 *DEXP(4.D0*X3**0.75D0)
C
          BEAMFN = DEXP(-Y)*H/EPSM
 
          IF( EPSM .LE. WIDTH) THEN
C         IF( EPSM .LE. 1.D-10) THEN
              BEAMFN = 1.D0/WIDTH + BEAMFN
          ENDIF
 
          BEAMFN = DEXP(-NG*TAU)*BEAMFN/0.988128D0
      ENDIF
 
C     EXPY = DEXP(-Y)
C     EPINV = 1.D0/EPSM
C     IF( IFL .EQ. 0 ) WRITE(6,9000)
C 9000 FORMAT(/1H1,/1H ,12X,'Eps',14X,'y',13X,'N1',14X,'X',14X,'H',
C    .                 10X,'EPINV',11X,'EXPY',10X,'BEAMFN')
C     WRITE(6,9100) EPS,Y,N1,X,H,EPINV,EXPY,BEAMFN
C9100 FORMAT(1X,F15.4,7G15.4)
C     IFL = 1
      RETURN
      END
