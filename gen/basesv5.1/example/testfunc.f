*     Function program for the elementary process
*           e+ e- ==> nu(mu) nu-bar(mu) gamma
*       Two independent variables
*           XK    = X(1) : the energy of photon
*           COSTH = X(2) : cos(theta) of photon
 
      REAL  FUNCTION FUNC*8(X)
 
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (MXDIM = 50 )
      DIMENSION X(MXDIM)
 
      COMMON/KINEM/W,EM,ZM,ZGAM,CZ,CV,CA,FACTOR,XK,COSTH
 
          REZ(S)= S-ZM**2
          Z2(S) = (REZ(S))**2+(ZM*ZGAM)**2
          FUNC  = 0.
          XK    = X(1)
          S     = W*W
          S1    = W*(W-2.*XK)
          E     = W/2.
          PP    = SQRT(E**2-EM**2)
          COSTH = X(2)
          D1    = XK*(E+PP*COSTH)
          D2    = XK*(E-PP*COSTH)
 
C ......... MATRIX ELEMENT SQUARE STARTS
C
C                    Z-DECAY INTO MU-NUTRINO PAIR
C
 
      ANS1=(S**2*CA**2+S**2*CV**2-2.*S*W*XK*CA**2-2.*S*W*XK*CV**2-3.*S*
     . CA**2*EM**2-3.*S*CV**2*EM**2+2.*W*XK*CA**2*EM**2+2.*W*XK*CV**2*
     . EM**2)/D1+S1*(-S**2*CA**2-S**2*CV**2+10.*S*CA**2*EM**2-2.*S*CV**
     . 2*EM**2-16.*CA**2*EM**4+8.*CV**2*EM**4)/(2.*D1*D2)+(-S**3*CA**2-
     . S**3*CV**2+2.*S**2*W*XK*CA**2+2.*S**2*W*XK*CV**2+2.*S**2*CA**2*
     . EM**2+2.*S**2*CV**2*EM**2-4.*S*W*XK*CA**2*EM**2-4.*S*W*XK*CV**2*
     . EM**2-4.*W**2*XK**2*CA**2*EM**2+4.*W**2*XK**2*CV**2*EM**2)/(2.*
     . D1*D2)-(D2*S1*EM**2)*(CA**2+CV**2)/D1**2+D2*EM**2*(-S*CA**2-S*CV
     . **2+2.*W*XK*CA**2+2.*W*XK*CV**2)/D1**2+S1*EM**2*(S*CA**2+S*CV**2
     . -8.*CA**2*EM**2+4.*CV**2*EM**2)/(2.*D1**2)+S*EM**2*(S*CA**2+S*CV
     . **2-2.*W*XK*CA**2-2.*W*XK*CV**2)/(2.*D1**2)
      TAU=-(D1*S1)*(CA**2+CV**2)/D2+D1*(-S*CA**2-S*CV**2+2.*W*XK*CA**2+
     . 2.*W*XK*CV**2+2.*CA**2*EM**2+2.*CV**2*EM**2)/D2-(D1*S1*EM**2)*(CA
     . **2+CV**2)/D2**2+D1*EM**2*(-S*CA**2-S*CV**2+2.*W*XK*CA**2+2.*W*
     . XK*CV**2)/D2**2+4.*EM**2*(CA**2+CV**2)+S1*(S*CA**2+S*CV**2-3.*CA
     . **2*EM**2-3.*CV**2*EM**2)/D2+(S**2*CA**2+S**2*CV**2-2.*S*W*XK*CA
     . **2-2.*S*W*XK*CV**2-3.*S*CA**2*EM**2-3.*S*CV**2*EM**2+2.*W*XK*CA
     . **2*EM**2+2.*W*XK*CV**2*EM**2)/D2+S1*EM**2*(S*CA**2+S*CV**2-8.*
     . CA**2*EM**2+4.*CV**2*EM**2)/(2.*D2**2)+S*EM**2*(S*CA**2+S*CV**2-
     . 2.*W*XK*CA**2-2.*W*XK*CV**2)/(2.*D2**2)-(D2*S1)*(CA**2+CV**2)/D1+
     . D2*(-S*CA**2-S*CV**2+2.*W*XK*CA**2+2.*W*XK*CV**2+2.*CA**2*EM**2+
     . 2.*CV**2*EM**2)/D1+S1*(S*CA**2+S*CV**2-3.*CA**2*EM**2-3.*CV**2*EM
     . **2)/D1+ANS1
 
*===> Calculate the value of function
 
       DSDX = -FACTOR*TAU*XK/(E*Z2(S1))
       FUNC = DSDX/E
 
*===> Fill histograms and scatter plot
 
       CALL XHFILL(1,XK,DSDX)
       CALL XHFILL(2,COSTH,DSDX)
       CALL DHFILL(1,COSTH,XK,DSDX)
 
 
      RETURN
      END
