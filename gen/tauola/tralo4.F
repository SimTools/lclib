      SUBROUTINE TRALO4(KTO,P,Q,AMS)

      REAL   *4  P(4), Q(4), AMS
      INTEGER*4  KTO
C--
#include "parmas.inc"
#include "p4tau.inc"
C
C====================< Entry Point >====================================
C
C--
C  KTO is useless in this single tau mode.
C--
      KTO = KTO
C--
C  Boost P to Q. 
C--
      AMS  = AMAS4(P)
C--      
      QA4  = P(4)*P4TAU(4) + P(1)*P4TAU(1)
     .                     + P(2)*P4TAU(2) 
     .                     + P(3)*P4TAU(3)
      QA4  = QA4/AMTAU
      A    = ( P(4) + QA4 )/( P4TAU(4) + AMTAU )
C--
      Q(1) = P(1) + A*P4TAU(1)
      Q(2) = P(2) + A*P4TAU(2)
      Q(3) = P(3) + A*P4TAU(3)
      Q(4) = SQRT( Q(1)**2 + Q(2)**2 + Q(3)**2 + AMS**2 )
C--
C  That's it.
C--
      RETURN
      END
