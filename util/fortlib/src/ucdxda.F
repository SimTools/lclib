CC********************************************************************CC
C*                                                                    *C
C*=============================----===                                *C
C*  Subroutine UCDXDA(PHI,HELX,DXDA)                                  *C
C*=============================----===                                *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculate @X/@A at given deflection angle, where X is positon   *C
C*    vector and A is helix parameter vector.                         *C
C* (Inputs)                                                           *C
C*      PHI       : R*4 ; deflection angle in degree measured from    *C
C*                        pivot postion ( HELX(6-8) ).                *C
C*      HELX(*)   : R*4 ; helix parameter vector.                     *C
C*                        1-st 8 words in                             *C
C*                        Production:TPC;Track_Parameter.             *C
C* (Output)                                                           *C
C*      DXDA(I,J) : R*8 ; derivative matrix. I=1..3, J=1..5           *C
C*                        I = (1,2,3) = (X,Y,Z)                       *C
C*                        J = (1,2,3,4,5) = (DR,FI0,CPA,DZ,TNL)       *C
C* (Relation)                                                         *C
C* (Update Record)                                                    *C
C*    6/12/87  K.Fujii            Original version.                   *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UCDXDA(PHI,HELX,DXDA)
   
      IMPLICIT REAL*8 ( A-H, O-Z )
   
#include "unumcn.inc"
#include "prfeld.inc"
      REAL*4 PHI,HELX(5)
      REAL*8 DXDA(3,5)
   
      EXTERNAL PRBLK1, UCONST
C  
C========< Entry Point >================================================
C  
#ifdef HIUXF77
      CALL PRBLK1
      CALL UCONST
#endif
C--
C  Copy helix parameters to local variables.
C--
      DR    = HELX(1)
      FI0   = HELX(2)
      CPA   = HELX(3)
      DZ    = HELX(4)
      TNL   = HELX(5)
      FI    = PHI
      R     = PTOR/CPA
C--
C  Calculate trigonometric functions.
C--
      FI0   = MOD(FI0+10.D0*x2PID,x2PID)
      CSFI0 = DCOS(FI0)
      SNFI0 = DSQRT(MAX(0.D0,1.D0-CSFI0*CSFI0))
      IF ( FI0.GT.xPID ) SNFI0 = - SNFI0
      PSI   = MOD(FI+FI0+10.D0*x2PID,x2PID)
      CSPSI = DCOS(PSI)
      SNPSI = DSQRT(MAX(0.D0,1.D0-CSPSI*CSPSI))
      IF ( PSI.GT.xPID ) SNPSI = - SNPSI
C--
C  Calculate @X/@a matrix.
C--
      DXDA(1,1) = CSFI0
      DXDA(1,2) = - DR*SNFI0 - R*(SNFI0-SNPSI)
      DXDA(1,3) = - (R/CPA)*(CSFI0-CSPSI)
      DXDA(1,4) = 0.D0
      DXDA(1,5) = 0.D0
   
      DXDA(2,1) = SNFI0
      DXDA(2,2) = DR*CSFI0 + R*(CSFI0-CSPSI)
      DXDA(2,3) = - (R/CPA)*(SNFI0-SNPSI)
      DXDA(2,4) = 0.D0
      DXDA(2,5) = 0.D0
   
      DXDA(3,1) = 0.D0
      DXDA(3,2) = 0.D0
      DXDA(3,3) = (R/CPA)*TNL*FI
      DXDA(3,4) = 1.D0
      DXDA(3,5) = - R*FI
C--
C  That's it.
C--
      RETURN
      END
