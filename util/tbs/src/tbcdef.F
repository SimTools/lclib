C**********************************************************************
C*
C* ------------------------------------
C* Subroutine TBCDEF(NUNIT, DSN, ACCESS)
C* ------------------------------------
C*
C*(Function)
C*   Modify default parameter of the data base.
C*
C*(Input)
C*   NUNIT  ; Logical unit #.     (I*4)
C*   DSN    ; Data base file name.(Character)
C*   ACCESS ; = 'READ' for read access (Character)
C*            = 'WRITE' for read/write access
C*
C*   If 0 (blank) is specified for integer (Character) variable,
C*  default parameter is not changed.
C*
C*(Author)
C*
C*  A. Miyamoto.  17-Mar-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE TBCDEF(NUNIT, DSN, ACCESS)
C
#include "tbcctl.inc"
C
      CHARACTER*(*) DSN, ACCESS
      CHARACTER     WORK*5
C
C============< ENTRY POINT >============================================
C
C ----------------------------------------------------------------------
C (1) Update default parameters.
C ----------------------------------------------------------------------
C
      IF(NUNIT.GT.0) MCxDBS = NUNIT
 
      IF(DSN(1:1).NE.' ') THEN
        CMxDBS = DSN
        LDSN   = LEN(DSN)
#ifdef MSP	
        CALL UCUPER(LDSN, CMxDBS)
#endif
      ENDIF

      IF(ACCESS(1:1).NE.' ') THEN
        WORK   = ACCESS
        CALL UCUPER(5, WORK)
        IF(WORK(1:4).EQ.'READ') THEN
           MCxOPN = 0
        ELSEIF(WORK(1:5).EQ.'WRITE') THEN
           MCxOPN = 1
        ENDIF
      ENDIF
 
      RETURN
      END
