C***********************************************************************
C*===================                                                  *
C* Subroutine BEAMIN( EBEAM, NXPOS ,NHISTP1, NHSTP2)                   *
C*===================                                                  *
C*                                                                     *
C*   This is the User initialization routine for Beam-Strahlung        *
C*   generation.  Beam parameters for 1000 GeV collider are            *
C*   temporary set in this routine.  May not be adequate for the       *
C*   generation at different energies.                                 *
C*                                                                     *
C*(Input)
C*   EBEAM  : Beam  energy (GeV)                                       *
C*   NXPOS  : (X(NXPOS:NXPOS+3) is used for BASES integration.        *
C*   NHSTP1 : Histogram ID pedestal for 1 dim. Histogram.
C*   NHSTP2 : Histogram ID pedestal for 2 dim. Histogram.
C*       1 dim histogram of ID NHSTP1 to NHSTP1+4 and 2 dim            *
C*       histogram of ID NHSTP2 are used in BASES.                     *
C*                                                                     *
C*                                                                     *
C*     This is USERIN for the beam strahlung calculation.he            *
C*                                                                     *
C*   Coded by S. Kawabata  April '89                                   *
C*   A.Miyamoto  3-May-1990  Modified for use in FULL system           *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BEAMIN( EBEAM, NXPOS, NHSTP1, NHSTP2)
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON / BASE1 / XL(10),XU(10),NDIM,NOCUB,NTRIAL
      COMMON / BASE2 / ACC1,ACC2,ITMX1,ITMX2
C
C     REAL*8 ME,NG,NCL,N,KSI1
C     COMMON/CNST/ PI,ALF,RE,CONST,SNORM, MXPOS, MHSTP1, MHSTP2
C     COMMON/PARM/ E0,N,SIGX,SIGY,SIGZ,NG,NCL,KSI1,WIDTH
C
      REAL*8   ME
      include 'beamcm.inc'
c      INCLUDE (@BEAMCM)
C     T#FP.GENLIB.COM
C
C**********************************************************************
C                    Beam Energy
      E0    = EBEAM
C
      N     = 8.D9
      SIGX  = 0.19D0
      SIGY  = 0.001D0
      SIGZ  = 26.D0
      WIDTH = 0.02D0
C
      MXPOS = NXPOS
      MHSTP1 = NHSTP1
      MHSTP2 = NHSTP2
C
      ME    = 0.5109996D-3
      RE    = 2.817940292D-9
      ALF   = 1.D0/137.0359895
C
      SIGXY = 2.D0/(SIGX+SIGY)
      KSI1  = RE*RE*E0*N*SIGXY/(ME*ALF*SIGZ)
      NCL   = 1.06D0*ALF*RE*N*SIGXY
      NG    = NCL*(1.D0 - 0.598D0*KSI1 + 1.061D0*KSI1**(5.D0/3.D0))
     .          /(1.D0 + 0.922D0*KSI1**2 )
 
      PRINT *,'KSI1 = ',KSI1
      PRINT *,'NG   = ',NG
      PRINT *,'EXP(-NG) = ',DEXP(-NG)
      PRINT *,'EBEAM =', E0
C**********************************************************************
      PI   = DACOS(-1.0D0)
      CONST= DSQRT( 3.D0/(8.D0*PI))
C
#ifdef X
   NDIM  = 4
#endif 
#ifdef X
   NTRIAL= 10
#endif 
#ifdef X
   ITMX2 = 200
#endif 
      XL(NXPOS) = 0.0D0
      XU(NXPOS) = 1.0D0
      XL(NXPOS+1) = 0.0D0
      XU(NXPOS+1) = 1.0D0
      XL(NXPOS+2) = 0.0D0
      XU(NXPOS+2) = 1.0D0
      XL(NXPOS+3) = 0.0D0
      XU(NXPOS+3) = 1.0D0
      SNORM = 1.0D0/0.36744
C
C
      SQS0  = 2.D0*E0
C
      CALL XHINIT(NHSTP1,XL(NXPOS),XU(NXPOS), 50,'X   = S/(4E0**2)')
      CALL XHINIT(NHSTP1+1,XL(NXPOS+2),XU(NXPOS+2), 50,'Eps1 = E1/E0')
      CALL XHINIT(NHSTP1+2,XL(NXPOS+2),XU(NXPOS+2), 50,'Eps2 = E2/E0')
      CALL XHINIT(NHSTP1+3,      0.D0,  SQS0, 50,'Effective Root(S)')
      CALL XHINIT(NHSTP1+4,  0.9D0*SQS0,  SQS0, 50,'Effective Root(S)')
C
      CALL DHINIT(NHSTP2+1,XL(NXPOS),XU(NXPOS),50,
     .                     XL(NXPOS+2),XU(NXPOS+2), 50,
     .           ' Eps1 vs Eps2 ')
C
      RETURN
      END
