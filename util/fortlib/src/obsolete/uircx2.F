CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C*  SUBROUTINE UIRCX2(IMODE,N,X,Y,RO,XC,YC,SIGMR,SIGMXC,SIGMYC,RCHISQ) *
C*====================                                                 *
C*                                                                     *
C*  (Purpose)                                                          *
C*       Circle fit                                                    *
C*                                                                     *
C*                                                                     *
C*(Input Parameters)                                                   *
C*      IMODE   - Determines method of minimizing the function.        *
C*             0  uses CIRCLE in the circle fit stage which is based on*
C*                J.F. Crawford, NIM 211(1983)223.                     *
C*             1  uses MRADLS in the minimizing stage which is based on*
C*                GRADLS of the Bevington library.                     *
C*             2  uses MHIFIT in the minimizing stage which is based on*
C*                CHIFIT of the Bevington library.                     *
C*           Else uses MURFIT in the minimizing stage which is based on*
C*                CURFIT of the Bevington library.                     *
C*      N       - Number of points.                                    *
C*      X       - Array of x-coordinate.                               *
C*      Y       - Array of y-coordinate.                               *
C*                                                                     *
C*(Output Parameters)                                                  *
C*      RO      - Radius of the circle.                                *
C*      XC      - x-coordinate of the center of the circle.            *
C*      YC      - y-coordinate of the center of the circle.            *
C*      SIGMR   - Standard deviation in determning RO.                 *
C*      SIGMXC  - Standard deviation in determning XC.                 *
C*      SIGMYC  - Standard deviation in determning YC.                 *
C*      RCHISQ  - Reduced CHI squre for the fit.                       *
C*                                                                     *
C*                                                                     *
C*  (Author)                                                           *
C*       T. Ishii (INS : Tel 0424 (61) 4131 - X326)                    *
C*                                                                     *
C*  (Date)                                                             *
C*          .  , 1985                                                  *
C*       June 8, 1986  Added IPRINT in the arguments of MINIMZ.        *
C*                                                                     *
CC**********************************************************************
C  
      SUBROUTINE UIRCX2(IMODE,N,X,Y,RO,XC,YC,SIGMR,SIGMXC,SIGMYC,RCHISQ)
      COMMON /COMNXY/ NPNT,XI(11),YI(11)
      DIMENSION X(*),Y(*)
      DIMENSION PARAM(3),DPARAM(3),SIGMAA(3)
      EXTERNAL UCHISQ
CCC   DATA CHICUT / 0.01 /
      DATA CHICUT / 0.1 /
      DATA SMALL  / 1.E-30 /
      DATA PI / 3.141593 /
C  
      IF(IMODE.EQ.0) THEN
        CALL CIRCLE(N,X,Y,RO,XC,YC,SIGMR,SIGMXC,SIGMYC,RCHISQ)
CC      PHI    = ATAN2(Y(N),X(N))
CC      PHIC   = ATAN2(YC,XC)
CC      DPHI   = PHI - PHIC
CC      IF(DPHI.LT.0.) DPHI = DPHI + 2.*PI
CC      IF(DPHI.GT.PI) RO = -RO
        RETURN
      ENDIF
C  
      NPNT   = N
      DO 1000 I = 1,NPNT
        XI(I) = X(I)
        YI(I) = Y(I)
 1000 CONTINUE
      NTERMS = 3
C  
C  CALCULATE INITIAL VALUES
C  
      X1     = XI(1)
      Y1     = YI(1)
      MIDL   = (NPNT + 1)/2
      X2     = XI(MIDL)
      Y2     = YI(MIDL)
      X3     = XI(NPNT)
      Y3     = YI(NPNT)
      A11    = X1 - X2
      A12    = Y1 - Y2
      B1     = (X1*X1 - X2*X2 + Y1*Y1 - Y2*Y2)/2.
      A21    = X1 - X3
      A22    = Y1 - Y3
      B2     = (X1*X1 - X3*X3 + Y1*Y1 - Y3*Y3)/2.
      DELTA  = A11*A22 - A21*A12
      IF(ABS(DELTA).LT.SMALL) DELTA = SIGN(SMALL,DELTA)
      XC     = (B1*A22 - B2*A12)/DELTA
      YC     = (B2*A11 - B1*A21)/DELTA
      RO     = SQRT((X2 - XC)**2 + (Y2 - YC)**2)
C  
C  GIVE SIGN TO RO OWING TO THE PARTICLE CHARGE
      IF(ABS(A11).EQ.0.) A11 = SMALL
      IF(ABS(A21).EQ.0.) A21 = SMALL
      T1     = A12/A11
      T2     = A22/A21
      IF(T1.LT.T2) RO = -RO
C  
      PARAM(1) = RO
      PARAM(2) = XC
      PARAM(3) = YC
C  
      CHISQR = UCHISQ(NTERMS,PARAM)
      IF(CHISQR.GT.CHICUT) GO TO 9000
C  
      DPARAM(1) = 1.
      DPARAM(2) = 1.
      DPARAM(3) = 1.
C  
      CALL UINIMZ(IMODE,UCHISQ,NTERMS,PARAM,IPRINT,DPARAM,SIGMAA,
     %            CHISQR,KAISU)
C  
      RO     = PARAM(1)
      XC     = PARAM(2)
      YC     = PARAM(3)
      SIGMR  = SIGMAA(1)
      SIGMXC = SIGMAA(2)
      SIGMYC = SIGMAA(3)
 9000 CONTINUE
      RCHISQ = CHISQR/FLOAT(NPNT - 3)
C  
      RETURN
      END
