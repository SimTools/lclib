      SUBROUTINE TAUINT(NINP,NOUT,NJAK1,NJAK2,NTDKRC,NKEYA1,YK0DEC)

C--
C  SWITCHES FOR TAUOLA.
C--
#include "jaki.inc"
#include "idfc.inc"
C--
C  I/O UNITS  NUMBERS.
C--
#include "inout.inc"
C--
C  LUND TYPE IDENTIFIER FOR A1.
C--
#include "idpart.inc"
C--
#include "taurad.inc"
#include "testa1.inc"
C--
      REAL*4            DUMMY(4)
C
C====================< Entry Point >====================================
C
C
C-----------------------------------------------------------------------
C          INITIALIZATION
C-----------------------------------------------------------------------
C
C--
C  Set the destination of job information printout.
C--
      INUT = NINP
      IOUT = NOUT
C--
C  Set TAUOLA parameters.
C--
      JAK1   = NJAK1
      JAK2   = NJAK2
      ITDKRC = NTDKRC
      KEYA1  = NKEYA1
      XK0DEC = YK0DEC
C--
C  LUND identifier for tau+.
C--
      IDFF = -15
C--
C  Initialize TAUOLA.
C--
      CALL INIMAS
      CALL INITDK
      CALL INIPHY(0.1D0)
C--
      CALL DEXAY(-1,DUMMY)
C--
C  That's it.
C--
      RETURN
      END
