C**********************************************************************
C*
C* ------------------
C*  Subroutine EWINIT
C* ------------------
C*
C*(Function)
C*   Initialize EW parameters.
C*
C*   A.Miyamoto   3-May-1990  Original version.
C*
C*
C**********************************************************************
C*
      SUBROUTINE EWINIT
C*
      INCLUDE 'ewparm.inc'
C     T#FP.A#JLC.FORT
C
C*
C===< Entry Point >====================================================
C*
C
      c_$PI  = DACOS(-1.D0)
      c_$2PI = 2.D0*DACOS(-1.D0)
      c_$HPI = 0.5D0*DACOS(-1.D0)
      c_$ALPHA = 1.D0/137.035989561D0
      c_$CONV  = 3.893796623D8
      c_$SN2TW = 0.23D0
      c_$AMZ   = 91.1D0
      c_$AMZ2  = c_$AMZ*c_$AMZ
      c_$GAMZ  =  2.5D0
      c_$AMW   = 80.0D0
      c_$AMW2  = c_$AMW*c_$AMW
      c_$GAMW  =  2.5D0
      c_$ALAM  =  0.2D0
      c_$ALFAS = 0.13
C
      RETURN
      END
