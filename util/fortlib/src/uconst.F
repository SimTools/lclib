C***********************************************************************
C* 
C* ------------------
C* BLOCK DATA  UCONST
C* ------------------
C* 
C*(Contents)
C*    Block data for Numerical constants.
C* 
C***********************************************************************
C* 
#if defined(HIUXF77) || defined(JavaVM) || defined(__GFORTRAN__)
      SUBROUTINE UCONST
#else
      BLOCK DATA  UCONST
#endif
C* 
#include "unumcn.inc"
      DATA xPI   /3.141592653E0/
      DATA xPID  /3.141592653 589 793 238D0/
      DATA x2PI  /6.283185307E0/
      DATA x2PID /6.283185307 179 586 476D0/
      DATA x4PI  /12.56637061E00/
      DATA x4PID /12.56637061 435 917 295D0/
      DATA xHPI  /1.570796326E0/
      DATA xHPID /1.570796326 794 896 619D0/
      DATA xRT2  /1.414213562E0/
      DATA xRT2D /1.414213562 373 095 048D0/
      DATA xDG2RD/1.745329251E-02/
      DATA xRD2DG/5.729577951E01/
      DATA xCNSEC/2.99792458E01/
   
      END
