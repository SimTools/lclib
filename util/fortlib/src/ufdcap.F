CC********************************************************************CC
C*                                                                    *C
C*=========================-------===                                 *C
C*  Subroutine UFDCAP(HELX,XVT,PVT)                                   *C
C*=========================-------===                                 *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates position and momentum at closest point to Z-axis.    *C
C* (Input)                                                            *C
C*      HELX(4) : (R*8) ; helix parameter.                            *C
C* (Outputs)                                                          *C
C*      XVT (3) : (R*4) ; closest point to Z-axis.                    *C
C*      PVT (3) : (R*4) ; momentum thereat.                           *C
C* (Update Record)                                                    *C
C*    8/16/87  K.Fujii                Original version.               *C
C*    4/28/90  K.Fujii                Modified to treat the pivot     *C
C*                                    placed near origin.             *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UFDCAP(HELX,XVT,PVT)
   
      IMPLICIT REAL*8 ( A-H, O-Z )
#include "prfeld.inc"
#include "unumcn.inc"
      REAL*8  HELX(8)
      REAL*4  XVT(3), PVT(3)
C>>>
      DATA RMIN / 1.D0 /
C>>>
   
      EXTERNAL UCONST, PRBLK1
C  
C========< Entry Point >================================================
C  
#if defined(HIUXF77) || defined(JavaVM)
      CALL PRBLK1
      CALL UCONST
#endif
C--
C  Get X and P at closest point to Z-axis.
C--
         DR   = HELX(1)
         F0   = HELX(2)
         CPA  = HELX(3)
         DZ   = HELX(4)
         TNL  = HELX(5)
         X0   = HELX(6)
         Y0   = HELX(7)
         Z0   = HELX(8)
         R    = PTOR/CPA
         PT   = ABS(1.D0/CPA)
         CHRG = SIGN(1.D0,CPA)
         RDR  = R + DR
         CSF0 = COS(F0)
         SNF0 = SIN(F0)
         XC   = (X0+RDR*CSF0)*CHRG
         YC   = (Y0+RDR*SNF0)*CHRG
         FI   = ATAN2(YC,XC)
         CSFI = COS(FI)
         SNFI = SIN(FI)
         DFI  = FI - F0
C--
C  Choose appropriate DFI.
C--
         DFI  = MOD(DFI+2.*x2PID,x2PID)
         R0 = SQRT(X0*X0+Y0*Y0)
         IF ( R0.LT.RMIN ) THEN
            IF ( DFI.GT.xPID ) DFI = DFI - x2PID
         ELSE
            IF ( CPA.LT.0.D0 ) DFI = DFI - x2PID
         ENDIF
C--
         XVT(1) = X0 + DR*CSF0 + R*(CSF0-CSFI)
         XVT(2) = Y0 + DR*SNF0 + R*(SNF0-SNFI)
         XVT(3) = Z0 + DZ - R*TNL*DFI
         PVT(1) = -PT*SNFI
         PVT(2) =  PT*CSFI
         PVT(3) =  PT*TNL
C--
C  That's it.
C--
      RETURN
      END
