C**********************************************************************
C* 
C*  ------------------------------------==================
C*  Subroutine UFGEOK( NPAR, MXxPAR, X, CHIS, GRAD, SECOND)
C*  ------------------------------------==================
C* 
C*(Function)
C*   Calculate chi-squre, first derivative of Chi-square and
C*   2nd-derivative of the matrix.
C* 
C*(Input)
C*   NPAR   ; # of parameter
C*   MXxPAR ; Size of matrix second.
C*   X  ; Fitted parameter array.
C*      X(1) = Xv
C*       (2) = Yv
C*       (3) = Zv
C*       (4) = Phi0 of 1st track
C*       (5) = K    of 1st track
C*       (6) = Tanl of 1st track
C*       (4) = Phi0 of 2nd track
C* 
C*      Dimension of X is 3 + 3*NUMTRK
C* 
C*(Output)
C*   CHIS   ; Chi-square
C*   GRAD   ; First derivative of the chi-square.
C*   SECOND ; Second derivative of the chi-square.
C* 
C*(Author)
C*  A. Miyamoto  27-Jun-1987
C* 
C**********************************************************************
C* 
      SUBROUTINE UFGEOK( NPAR, MXxPAR, X, CHIS, GRAD, SECOND)
C  
      IMPLICIT  REAL*8 ( A-H, O-Z)
#include "ufgeoc.inc"
#include "ufgeop.inc"
      REAL*8          X(NPAR), GRAD(NPAR), SECOND(MXxPAR, MXxPAR)
      REAL*8          DA(5), DAE(5), DADP(5,6)
      DATA            DADP/30*0.D0/
C  
C*    DADP(i,1:6) : Derivative of i-th measured track parameter with
C*                  respect to the Xv, Yv, Zv, Phi0, K, Tanl
C*    DAE(i)      : Product of dA vector and error matrix.
C*    DXDA(i,j)   : Derivative of the calculated position with respect
C*                  to the track parameter, Xv, Yv, Zv, Phi0, K, Tanl
C  
C =====< Entry Point >=================================================
C  
      CHIS    = 0.0
      XV      = X(1)
      YV      = X(2)
      ZV      = X(3)
      CALL UVZERO(2*NPAR, GRAD)
      CALL UVZERO(2*MXxPAR*MXxPAR, SECOND)
      DADP(3,5) = 1.D0
      DADP(4,3) = 1.D0
      DADP(5,6) = 1.D0
C  
C ---------------------------------------------------------------------
C (2) Track Loop
C ---------------------------------------------------------------------
C  
      DO 200 ITRK = 1, NUMTRK
        IP    = 3*(ITRK-1) + 3
        A     = X(IP+1)
        B     = X(IP+2)
        C     = X(IP+3)
        X0    = TRKPAR(6,ITRK)
        Y0    = TRKPAR(7,ITRK)
        Z0    = TRKPAR(8,ITRK)
        RHO   = DPTOR/B
C  
        COSA  = DCOS(A)
        SINA  = DSIN(A)
        XC    = XV  + RHO*COSA
        YC    = YV  + RHO*SINA
C  
        IF( TRKPAR(3, ITRK) .GT. 0.D0 ) THEN
           PHI0 = DATAN2( YC-Y0, XC-X0 )
        ELSE
           PHI0 = DATAN2( Y0-YC, X0-XC)
        ENDIF
C  
        COSP0 = DCOS(PHI0)
        SINP0 = DSIN(PHI0)
        XP0   = XV + RHO*(COSA-COSP0)
        YP0   = YV + RHO*(SINA-SINP0)
        ZP0   = ZV - RHO*C*(PHI0-A)
C  
        DR    = (XP0-X0)*COSP0 + (YP0-Y0)*SINP0
        DA(1) = TRKPAR(1,ITRK) - DR
        DA(2) = TRKPAR(2,ITRK) - PHI0
        DA(3) = TRKPAR(3,ITRK) - B
        DA(4) = TRKPAR(4,ITRK) - ( ZP0 - Z0 )
        DA(5) = TRKPAR(5,ITRK) - C
C  
C  
C ... Calculate Chi-Square contribution of this track.
C  
        CHI   = 0.0
        DO 210 I = 1, 5
          DO 220 J = 1, 5
            CHI  = CHI  + DA(I)*TRKERR(I,J,ITRK)*DA(J)
220       CONTINUE
210     CONTINUE
C  
        TRKPAR(9,ITRK) = CHI
        CHIS           = CHI + CHIS
C  
C (DADP(i,j)
C       i = dr,Phi0,K,dZ,Tanl
C       j = Xv,Yv,Zv,A,B,C
C  
        RHODRI    =  1.D0/(RHO + DR)
        RHOB      =  RHO/B
        DADP(1,1) =  COSP0
        DADP(1,2) =  SINP0
        DADP(1,4) =  RHO* (        SINP0*COSA - COSP0*SINA )
        DADP(1,5) =  RHOB*( 1.D0 - SINP0*SINA - COSP0*COSA )
C  
        DADP(2,1) = -RHODRI*SINP0
        DADP(2,2) =  RHODRI*COSP0
        DADP(2,4) =  RHO *RHODRI*( SINP0*SINA + COSP0*COSA)
        DADP(2,5) =  RHOB*RHODRI*( SINP0*COSA - COSP0*SINA)
C  
        DADP(4,1) =  RHO*RHODRI*C*SINP0
        DADP(4,2) = -RHO*RHODRI*C*COSP0
        DADP(4,4) =  RHO*C*(  1.D0 - DADP(2,4))
        DADP(4,5) =  RHO*C*( (PHI0-A)/B - DADP(2,5) )
        DADP(4,6) = -RHO*(PHI0-A)
C  
C (Derivative of chi-square.
C  
        DO 300 I = 1, 5
          DAE(I) = 0
          DO 310 J = 1, 5
            DAE(I) = DAE(I) + DA(J)*TRKERR(J,I,ITRK)
310       CONTINUE
300     CONTINUE
C  
C  
C Derivatives with respect to A, B, C
C  
        DO 400 I = 1, 6
          MP  = I
          IF( I .GT. 3 ) MP = IP + I - 3
          DO 410 IX = 1, 5
            GRAD(MP) = GRAD(MP) - DAE(IX)*DADP(IX,I)
410       CONTINUE
C  
          DO 420 J = I, 6
            KP = J
            IF( J .GT. 3 ) KP = IP + J - 3
            DO 430 IX = 1, 5
            DO 430 IY = 1, 5
              SECOND(MP, KP) = SECOND(MP, KP) +
     >                         DADP(IX,I)*TRKERR(IX,IY,ITRK)*DADP(IY,J)
430         CONTINUE
            SECOND(KP, MP) = SECOND(MP, KP)
420       CONTINUE
400     CONTINUE
C  
C  
200   CONTINUE
C  
CC    PRINT *,' UFGEOK..Chi-square = ', CHIS
C  
      RETURN
      END
