
      SUBROUTINE TAUFIL(KTO,Q4TAU)
C
C SUBSITUTE OF tau PRODUCTION GENERATOR
C
#include "parmas.inc"
#include "idfc.inc"
#include "taupos.inc"
#include "p4tau.inc"
C
      INTEGER*4  KTO
      REAL*4     Q4TAU(4)
      REAL*4     AQF(4)
C
C====================< Entry Point >====================================
C
C--
C  Set tau 4-momentum in its rest frame.
C--
      DO 10 K = 1, 3
         AQF(K)   = 0.0
         P4TAU(K) = Q4TAU(K)
10    CONTINUE
      AQF(4)   = AMTAU
      P4TAU(4) = SQRT( AMTAU**2 + P4TAU(1)**2
     .                          + P4TAU(2)**2
     .                          + P4TAU(3)**2 )
C--
C  Transform AQF to lab. frame.
C--
      CALL TRALO4(KTO,AQF,AQF,AM)
C--
C  Fill to HEP common.
C--
      IF ( KTO.EQ.1 ) THEN
         NPA  = 3
         CALL FILHEP(NPA,1,+IDFF,0,0,0,0,AQF,AMTAU,.TRUE.)
      ELSE         
         NPB  = 4
         CALL FILHEP(NPB,1,-IDFF,0,0,0,0,AQF,AMTAU,.TRUE.)
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
