CC********************************************************************CC
C*                                                                    *C
C*==============================--------------==                      *C
C*  Subroutine UFCLSP(HELX,XVRT,XV,PV,DIST,PHI                        *C
C*==============================--------------==                      *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*     Finds a point on a helix which is closest to a given point     *C
C*     and calculates tangent vector and deflection angle therat.     *C
C* (Inputs)                                                           *C
C*        HELX(*)   : (R*4) ; helix parameters.                       *C
C*                            1-st 8 words in                         *C
C*                            Production:TPC;Track_Parameter.         *C
C*        XVRT(*)   : (R*4) ; position at which closest approach is   *C
C*                            calculated.                             *C
C* (Outputs)                                                          *C
C*        XV  (*)   : (R*4) ; point of closest approach.              *C
C*        PV  (*)   : (R*4) ; unit tangent vector thereat.            *C
C*        DIST      : (R*4) ; distance of closest approach.           *C
C*        PHI       : (R*4) ; deflection angle therat.                *C
C* (Relation)                                                         *C
C* (Update Record)                                                    *C
C*    6/25/87  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UFCLSP(HELX,XVRT,XV,PV,DIST,PHI)
   
      IMPLICIT    REAL*8 ( A-H, O-Z )
#include "unumcn.inc"
#include "prfeld.inc"
      REAL   *4   HELX(8),XVRT(3),XV(3),PV(3),DIST,PHI
      REAL   *8   XC(3),DX(3),TNV(3)
   
      DATA  NTRYMX / 100 /  DISTST /  1.D-6 /
   
      EXTERNAL UCONST, PRBLK1
   
C  
C========< Entry Point >================================================
C  
C--
C  Set initial parameters.
C--
      DISTS = 1.D10
      DO 10 I = 1, 3
         XV(I) = XVRT(I)
10    CONTINUE
C--
C  Convert helix parameterization.
C     X = XC(1) - R*cos(FI+FI0)
C     Y = XC(2) - R*sin(FI+FI0)
C     Z = XC(3) - R*TNL*FI
C--
      FI0    = HELX(2)
      TNL    = HELX(5)
      ICHG   = SIGN(1.1,HELX(3))
      R      = PTOR/HELX(3)
      FACT   = 1.D0/(1.D0+TNL*TNL)/R
      XC(1)  = HELX(6) + ( R + HELX(1) )*DCOS(FI0)
      XC(2)  = HELX(7) + ( R + HELX(1) )*DSIN(FI0)
      XC(3)  = HELX(8) +  HELX(4)
      FI     = DATAN2(XV(2)-XC(2),XV(1)-XC(1))
      FI     = FI - FI0 - xHPID*( 1 + ICHG )
      FI     = DMOD(FI+10.D0*x2PID,x2PID)
      IF ( FI.GT. xPID ) FI = FI - x2PID
C--
C  Iteration loop starts here.
C--
      NTRY  = 0
1     NTRY = NTRY + 1
C--
C  Get DX and TNV(tangent verctor).
C--
      CSF    =  DCOS(FI+FI0)
      SNF    =  DSIN(FI+FI0)
      XV(1)  =  XC(1) - R*CSF
      XV(2)  =  XC(2) - R*SNF
      XV(3)  =  XC(3) - R*TNL*FI
      TNV(1) =  SNF
      TNV(2) = -CSF
      TNV(3) = -TNL
      DO 20 I = 1, 3
         DX(I) = XV(I) - XVRT(I)
20    CONTINUE
C--
C  Calculate dist.
C--
      DIST  = DX(1)*DX(1)+DX(2)*DX(2)+DX(3)*DX(3)
      IF ( DABS(DIST-DISTS).LT.DISTST )          GO TO 3000
C--
C  Correct FI.
C--
      DISTS = DIST
      DXDF  = 0.D0
      DO 30 I = 1, 3
         DXDF = DXDF + DX(I)*TNV(I)
30    CONTINUE
      FI    = FI - FACT*DXDF
C--
C  Continue iteration if NTRY < NTRYMX.
C--
      IF ( NTRY.LT.NTRYMX )                      GO TO 1
C--
C  Now calculate momentum and distance.
C--
3000  DIST = SQRT(DIST)
      PHI  = FI
      ATNV = 1.D0/DSQRT(1.D0+TNL*TNL)
      DO 40 I = 1, 3
         PV(I) =  ATNV*TNV(I)
40    CONTINUE
C--
C  End iteration loop.
C--
9000  RETURN
      END
