C**********************************************************************
C* 
C* ----------------
C*  Common /UFGEOP/
C* ----------------
C* 
C*(Contents)
C*   Parameter for the Geometrical constraint fit.
C* 
C**********************************************************************
C* 
      REAL*8     DPTOR
      REAL*8     CHITST
      INTEGER*4  MAXTRY
      COMMON /UFGEOP/ DPTOR, CHITST, MAXTRY
C* 
C*    DPTOR   ; Factor to conver Pt to R  (R = PTOR*Pt)
C*    This data is set in UFGEOI
C  
C*    CHITST  ; Maximum chi-square change to terminate fit.
C=    DATA    CHITST/1.0D-6/
C  
C*    MAXTRY  ; Maximum # of fit loop.
C=    DATA    MAXTRY/50/
