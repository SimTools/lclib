C**********************************************************************
C*
C* ------------------------------
C* Subroutine MAOPTN( ID, IVALUE )
C* ------------------------------
C*
C*(Function)
C*   Set options of the manager system
C*
C*(Input)
C*   ID     ; Parameter ID.
C*           = 1 ; Change Message level.
C*           = 2 ; Change output message unit.
C*           = 3 ; Change input command unit.
C*      ID < 0 to get current value.
C*
C*
C*(Author)
C*
C*  A. MIYAMOTO.  29-May-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MAOPTN( ID, IVALUE)
C
#include "macntl.inc"
C
C============< ENTRY POINT >============================================
C
      IF( ID .LT. 0 ) GO TO 300
C
      GO TO ( 110, 120, 130) ID
110   CONTINUE
      MCxMSL = IVALUE
      GO TO 900
 
120   CONTINUE
      MCxMPT = IVALUE
      GO TO 900
 
130   CONTINUE
      MCxMGT = IVALUE
      GO TO 900
C
300   CONTINUE
      IID = IABS(ID)
      GO TO ( 310, 320, 330 ) IID
 
310   CONTINUE
      IVALUE = MCxMSL
      GO TO 900
 
320   CONTINUE
      IVALUE = MCxMPT
      GO TO 900
 
330   CONTINUE
      IVALUE = MCxMGT
      GO TO 900
 
900   CONTINUE
      RETURN
      END
