      SUBROUTINE INIMAS
C
C ----------------------------------------------------------------------
C     INITIALISATION OF MASSES
C
C     called by : KORALZ
C ----------------------------------------------------------------------
C
#include "parmas.inc"
C
C IN-COMING / OUT-GOING  FERMION MASSES
C
      AMTAU  = 1.7842
      AMNUTA = 0.010
      AMEL   = 0.0005111
      AMNUE  = 0.0
      AMMU   = 0.105659
      AMNUMU = 0.0
C
C MASSES USED IN TAU DECAYS
C
      AMPIZ  = 0.134964
      AMPI   = 0.139568
      AMRO   = 0.773
      GAMRO  = 0.145
#ifdef C
   GAMRO  = 0.666
#endif 
      AMA1   = 1.251
      GAMA1  = 0.599
      AMK    = 0.493667
      AMKZ   = 0.49772
      AMKST  = 0.8921
      GAMKST = 0.0513
C
      RETURN
      END
