C**********************************************************************
C*
C* -----------------
C* Subroutine MAxVRF
C* -----------------
C*
C*(Function)
C*   Check open status of the data base.
C*     (1) If data base is not opened, OPEN data base file.
C*     (2) If data base file is modified after the last data set open,
C*        close and re-open data base file.
C*
C*(Author)
C*
C*  A. Miyamoto.  18-Mar-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MAxVRF
C
#include "macntl.inc"
C
      CHARACTER*68 KEY
C
C
C============< Entry Point >============================================
C
C-----------------------------------------------------------------------
C (1) If data base is not opened yet, open it.
C-----------------------------------------------------------------------
C
      IF(MCxOPS.EQ.0) THEN
        CALL MAOPEN(IRET)
        MCxOPS = 2
C
C-----------------------------------------------------------------------
C (2) If data base is already open, chek time stamp data.
C-----------------------------------------------------------------------
C
      ELSE
#ifdef  MSP
        READ(MCxDBS,KEY=CMxDLM(:MCxKEY))
#endif
#ifdef  MSP
     >       KEY(:MCxKEY), IW, ITIME, IDATE
#endif
#ifdef  UNIX
        ITIME=0
#endif
#ifdef  UNIX
        IDATE=0
#endif
        IF(IDATE.GE.MCxOPD) THEN
          IF(ITIME.GE.MCxOPT) THEN
            IOSTAT = MCxOPS
            CALL MACLOS
            CALL MAOPEN(IRET)
            MCxOPS = IOSTAT
          ENDIF
        ENDIF
      ENDIF
C
C
C
      RETURN
      END
