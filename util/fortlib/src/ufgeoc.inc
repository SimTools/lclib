C**********************************************************************
C* 
C* ----------------
C*  Common /UFTBUF/
C* ----------------
C* 
C*(Contents)
C*   Buffer for Geometrical constraint fit.
C*   This common is used by UFGEOC (Geometrical constraint fit) routines
C* 
C* 
C**********************************************************************
C* 
      PARAMETER (MFxTRK=50,
     >           MFxPAR=3*MFxTRK + 3 ,
     >           MFxDIM=MFxPAR*(MFxPAR+1)/2,
     >           MFxWRK=3*MFxPAR+1)
C* 
C*    MFxTRK  ; Max # of track for fit.
C*    MFxPAR  ; Max # of fit parameter.
C*    MFxDIM  ; Size of Error matrix.
C*    MFxWRK  ; Size of Work area.
C* 
      REAL*8     TRKPAR(9, MFxTRK)
      REAL*8     TRKERR(5, 5, MFxTRK)
      REAL*8     DD(MFxPAR, MFxPAR)
C* 
      COMMON /UFTBUF/ NUMTRK, NDUMY, TRKPAR, TRKERR,
C* 
C*    NUMTRK  ;  # of track.
C*    TRKPAR  ; Track parameter data for fit
C*               ( dr, Phi0, K, dz, Tanl, X0, Y0, Z0, Chi-sqare)
C*    TRKERR  ; Hessian Matrix.
C* 
     >            DD
C* 
C*    DD(i)   ; Error matrix.
