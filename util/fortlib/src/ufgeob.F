#if defined(HIUXF77) || defined(JavaVM)
      SUBROUTINE UFGEOB
#else
      BLOCK DATA UFGEOB
#endif
C  
C  Block data for geometrical constraint fit.
C  
#include "ufgeop.inc"
      DATA    CHITST/1.0D-6/
      DATA    MAXTRY/50/
C  
      END
