C*
C*    Parameter COMMON for Techni-Colour study
C*
      REAL*4 EBEAM, TRMASS, TRWID
      INTEGER*4  NCAHEL, NAMPL
      REAL*4 WIDTHW, WIDTHZ, BRPROD
      COMMON /TECRPR/EBEAM, TRMASS, TRWID, NCAHEL, NAMPL,
     >        WIDTHW(0:12), WIDTHZ(0:12), BRPROD

C
C  EBEAM  ; Center of mass energy.1
C  TRMASS ; Techni-rho mass
C  TRWID  ; TEchni-Rho width
C  NCAHEL ; = 0 To calculate All helicity.
C           = 1 TO calculate WLWL cross section  
C  NAMPL  = 0 to include all amplitude.
C         = 1 to calculate all SM amplitude.
C         = 2 to calculate |W_LL(J=1)|^2.
C         = 3 to calculate 2Re(W_LL^*(J=all)W_LL(J=1)).
C         = 4 to calculate 2Im(W_LL^*(J=all)W_LL(J=1)).
C

