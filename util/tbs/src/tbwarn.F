C*
C *********************************************************************
C*
C* ---------------------------
C* Subroutine TBWARN( ILEVEL )
C* ---------------------------
C*
C*(Function)
C*   Set/reset the warning message print out level.
C*
C*(Input)
C*   ILEVEL  .. = 0 to suppress the warning message printing.
C*              = 1 to un-suppress.
C*
C*(Author)
C*   A. Miyamoto    12-Feb-1985.
C*
C *********************************************************************
C*
      SUBROUTINE TBWARN(ILEVEL)
C
#include "tbsprm.inc"
C
      LEVEL = ILEVEL
      RETURN
      END
