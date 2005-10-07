#if defined(HIUXF77) || defined(__GFORTRAN__)
      SUBROUTINE CMBBLK
#else
      BLOCK DATA CMBBLK
#endif

#include "cmbcut.inc"
C      COMMON /CMBCUT/ ADXECT, ENSGEM, ENSGHD
C      REAL*4          ADXECT, ENSGEM, ENSGHD
      DATA ADXECT /    20.0 /
      DATA ENSGEM /     2.0 /
      DATA ENSGHD /     2.0 /

      END
