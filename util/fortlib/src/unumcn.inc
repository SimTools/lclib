C***********************************************************************
C* 
C* ---------------
C* Common /UNUMCN/
C* ---------------
C* 
C*(Contents)
C*    This common contains the various constants.
C* 
C*  1) Single Precision Constants.
C*    xPI     =  ACOS(-1.0E0)
C*    x2PI    =  2 * xPI
C*    x4PI    =  4 * xPI
C*    xHPI    =  xPI / 2
C*    xRT2    =  SQRT(1.E0)
C*    xDG2RD  =  xPI/180. (Degree to Radian.)
C*    xRD2DG  =  180./xPI(Radian to Degree.)
C*    xCNSEC  =  Light Velocity (cm/nsec) (Ref. Particle Property Data
C*              Booklet, April, 1986)
C* 
C*  2) Double Precision Constants.
C*    xDPI    =  DACOS(-1.0D0)
C*    xD2PI   =  2 * xPI
C*    xD4PI   =  4 * xPI
C*    xDHPI   =  xPI / 2
C*    xDRT2   =  DSQRT(1.D0)
C* 
C* (Comments)
C*    All constants are calculated by REAL*16 variable, then converted
C*    to REAL*4 or REAL*8.
C* 
C***********************************************************************
C* 
      REAL*8   xPID, x2PID, x4PID, xHPID, xRT2D
      REAL*4   xPI,  x2PI,  x4PI,  xHPI,  xRT2,  xDG2RD, xRD2DG,
     .         xCNSEC
      COMMON /UNUMCN/ xPID,   x2PID,  x4PID,  xHPID,  xRT2D,
     .                xPI,    x2PI,   x4PI,   xHPI,   xRT2,
     .                xDG2RD, xRD2DG, xCNSEC
C  
C=    DATA xPI   /3.141592653E0/
C=    DATA xPID  /3.141592653 589 793 238D0/
C  
C=    DATA x2PI  /6.283185307E0/
C=    DATA x2PID /6.283185307 179 586 476D0/
C  
C=    DATA x4PI  /12.56637061E00/
C=    DATA x4PID /12.56637061 435 917 295D0/
C  
C=    DATA xHPI  /1.570796326E0/
C=    DATA xHPID /1.570796326 794 896 619D0/
C  
C=    DATA xRT2  /1.414213562E0/
C=    DATA xRT2D /1.414213562 373 095 048D0/
C  
C=    DATA xDG2RD/1.745329251E-02/
C=    DATA xRD2DG/5.729577951E01/
C  
C=    DATA xCNSEC/2.99792458E01/
   
