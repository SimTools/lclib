C***********************************************************************
C*===================                                                  *
C*    Subroutine BEAMGN( XX, EMINS, EPLUS, FUNC )                      *
C*===================                                                  *
C*(Function)                                                           *
C*    Simulate beamstralung of linear collider and generate effective  *
C*    root-s of a event.  To be used togather with BEAMIN and BEAMFN  *
C*    in T#FP.GENLIB.FORT.                                             *
C*(Input)                                                              *
C*    XX(*) ; An array of BASES integral variables. XX(MXPOS) to       *
C*            XX(MXPOS+3) are used by this routine.  MXPOS should be   *
C*            defined in BEAMIN.                                       *
C*(Output)                                                             *
C*    EMINS ; Generated e- energy (GeV)                                *
C*    EPLUS ; Generated e+ energy (GeV)                                *
C*    FUNC  ; Weight of this event.                                    *
C*                                                                     *
C*(Autho)                                                              *
C*   Coded by S. Kawabata  Nov. '84                                    *
C*   Modified for FULL system  by A.Miyamoto  3-May-1990               *
C*                                                                     *
C***********************************************************************
C
C------------------------------------------------------------------
C
      SUBROUTINE BEAMGN( XX, EMINS, EPLUS, FUNC )
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
C     REAL*8 N,NG,NCL,KSI1
C
C     COMMON/CNST/ PI,ALF,RE,CONST,SNORM, MXPOS, MHSTP1, MHSTP2
C     COMMON/PARM/ E0,N,SIGX,SIGY,SIGZ,NG,NCL,KSI1
      include 'beamcm.inc'
c      INCLUDE (@BEAMCM)
C     T#FP.GENLIB.COM
C
      DIMENSION XX(10)
 
      FUNC = 0.0D0
 
      X      = XX(MXPOS)
      DEPS1  = 1.D0 - X
      EPS1   = DEPS1*XX(MXPOS+1) + X
      TAU1   = XX(MXPOS+2)
      EPS2   = X/EPS1
      TAU2   = XX(MXPOS+3)
C
      EPLUS  = EPS1*E0
      EMINS  = EPS2*E0
 
C      PRINT *,X,DEPS1,XX(MXPOS+1),EPS1,TAU1,TAU2
 
      FUNC   = SNORM*BEAMFN(EPS1,TAU1)*DEPS1*BEAMFN(EPS2,TAU2)/EPS1
 
      IF( FUNC .LT. 0.0 ) THEN
          FUNC   = 0.0
          RETURN
      ENDIF
 
      RSEF  = DSQRT( 4.D0*EPS1*EPS2 )*E0
 
      CALL XHFILL(MHSTP1,    X, FUNC )
      CALL XHFILL(MHSTP1+1, EPS1, FUNC )
      CALL XHFILL(MHSTP1+2, EPS2, FUNC )
      CALL XHFILL(MHSTP1+3, RSEF, FUNC )
      CALL XHFILL(MHSTP1+4, RSEF, FUNC )
      CALL DHFILL(MHSTP2, EPS1, EPS2, FUNC )
 
      RETURN
 
      END
