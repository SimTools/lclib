C*
C*********************************************************************
C*
C* ----------------------------
C*  Subroutine TBLUNI( IUNIT )
C* ----------------------------
C*
C*(Function)
C*   Set the logical unit number, to which output the data.
C*
C*(Input)
C*   IUNIT ...  Logical unit number.
C*
C*(Author)
C*   A. Miyamoto     12-Feb-1985
C*
C**********************************************************************
C*
      SUBROUTINE TBLUNI( IUNIT )
C
#include "tbsprm.inc"
C
      LUNIT = IUNIT
C
      RETURN
      END
