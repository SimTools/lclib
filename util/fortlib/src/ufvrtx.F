CC********************************************************************CC
C*                                                                    *C
C*==========================================-------------===          *C
C*  Subroutine UFVRTX(NTRK,LNxHLX,HELX,XVST,XV,CHI2,DCHI2)            *C
C*==========================================-------------===          *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates vertex point consists of NTRK tracks.                *C
C* (Inputs)                                                           *C
C*      NTRK        : (I*4) ; # tracks.                               *C
C*      LNxHLX      : (I*4) ; length of helix parameter ( > 7 )       *C
C*      HELX(*,ITK) : (R*4) ; helix parameters. The 1-st 8 words      *C
C*                            should have the same format as that in  *C
C*                            Production:TPC;Track_Parameter.         *C
C*      XVST(*)     : (R*4) ; start vertex.                           *C
C* (Outputs)                                                          *C
C*      XV  (*)     : (R*4) ; vertex.                                 *C
C*      CHI2        : (R*4) ; chi**2 defined to be sum  of squared    *C
C*                            distance from vertex to helices.        *C
C*      DCHI2(*)    : (R*4) ; chi**2 contribution of each track.      *C
C* (Relation)                                                         *C
C*    Calls : UVZERO and UMATIN.                                      *C
C* (Update Record)                                                    *C
C*    5/23/87  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UFVRTX(NTRK,LNxHLX,HELX,XVST,XV,CHI2,DCHI2)
   
      IMPLICIT    REAL*8 ( A-H, O-Z )
#include "unumcn.inc"
#include "prfeld.inc"
      REAL   *4   HELX(LNxHLX,NTRK),XVST(3),XV(3),DCHI2(NTRK),CHI2
      INTEGER*4   NTRK,LNxHLX
      REAL   *8   XC(3),EE(3,3),RS(3),X0(3),TNV(3)
   
      DATA  NTRYMX / 100 /  CHITST /  1.D-6 /
   
      EXTERNAL    PRBLK1, UCONST
C  
C  
C========< Entry Point >================================================
C  
#ifdef HIUXF77
      CALL PRBLK1
      CALL UCONST
#endif
C  
C--
C  Set initial parameters.
C--
      CHI2S = 1.D10
      DO 10 I = 1, 3
         XV(I) = XVST(I)
10    CONTINUE
C--
C  Start iteration.
C--
      NTRY = 0
1     NTRY = NTRY + 1
C--
C  Clear chi**2, Dchi**2, error matrix and residulal vector.
C--
      CHI2 = 0.E0
      CALL UVZERO(NTRK,DCHI2)
      CALL UVZERO( 18,EE)
      CALL UVZERO(  6,RS)
C--
C  Loop over tracks to accumulate chi**2 and its derivatives.
C--
      DO 200 ITRK = 1, NTRK
C--
C  Convert helix parameterization.
C     X = XC(1) - R*cos(FI+FI0)
C     Y = XC(2) - R*sin(FI+FI0)
C     Z = XC(3) - R*TNL*FI
C--
         FI0    = HELX(2,ITRK)
         TNL    = HELX(5,ITRK)
         ICHG   = SIGN(1.1,HELX(3,ITRK))
         R      = PTOR/HELX(3,ITRK)
         XC(1)  = HELX(6,ITRK) + ( R + HELX(1,ITRK) )*DCOS(FI0)
         XC(2)  = HELX(7,ITRK) + ( R + HELX(1,ITRK) )*DSIN(FI0)
         XC(3)  = HELX(8,ITRK) +  HELX(4,ITRK)
         FI     = DATAN2(XV(2)-XC(2),XV(1)-XC(1))
         FI     = FI - FI0 - xHPID*( 1 + ICHG )
         FI     = DMOD(FI,x2PID)
         IF ( FI.GT. xPID ) FI = FI - x2PID
         IF ( FI.LT.-xPID ) FI = FI + x2PID
C--
C  Get X0 and TNV(tangent verctor at X0).
C--
         CSF    = DCOS(FI+FI0)
         SNF    = DSIN(FI+FI0)
         X0(1)  = XC(1) - R*CSF
         X0(2)  = XC(2) - R*SNF
         X0(3)  = XC(3) - R*TNL*FI
         ATNV   = 1.D0/DSQRT(1.D0+TNL*TNL)
         TNV(1) = -SNF*ATNV
         TNV(2) =  CSF*ATNV
         TNV(3) =  TNL*ATNV
C--
C  Accumulate chi**2, Dchi**2, error matrix and residulal vector.
C--
         DO 20 I = 1, 3
            DCHI2(ITRK) = DCHI2(ITRK) + ( X0(I) - XV(I) )**2
            RS(I)       = RS(I)   + X0(I)
            EE(I,I)     = EE(I,I) + 1.D0
            DO 2 J = 1, 3
               RS(I)   = RS(I)   - TNV(I)*TNV(J)*X0(J)
               EE(I,J) = EE(I,J) - TNV(I)*TNV(J)
2           CONTINUE
20       CONTINUE
         CHI2 = CHI2 + DCHI2(ITRK)
200   CONTINUE
C--
C  Check chi**2 change. If converged, jump out the iteration loop.
C--
      DELCHI = CHI2S - CHI2
      IF ( DABS(DELCHI).LT.CHITST )              GO TO 9000
C--
C  Invert EE and get next step.
C--
      CALL UMATIN(3,3,EE,DET,IRT)
      DO 60 I = 1, 3
         XV(I) = 0.
         DO 6 J = 1, 3
            XV(I) = XV(I) + EE(I,J)*RS(J)
6        CONTINUE
60    CONTINUE
C--
C  Save CHI2.
C--
      CHI2S = CHI2
C--
C  Continue iteration if NTRY < NTRYMX.
C--
      IF ( NTRY.LT.NTRYMX )                      GO TO 1
C--
C  End iteration loop.
C--
9000  RETURN
      END
