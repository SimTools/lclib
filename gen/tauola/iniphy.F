
      SUBROUTINE INIPHY(XK00)
C ----------------------------------------------------------------------
C     INITIALISATION OF PARAMETERS
C     USED IN QED and/or GSW ROUTINES
C ----------------------------------------------------------------------
C
#include "qedprm.inc"
      REAL*8 PI8,XK00
C
      PI8    = 4.D0*DATAN(1.D0)
      ALFINV = 137.03604D0
      ALFPI  = 1D0/(ALFINV*PI8)
      XK0=XK00
      END
