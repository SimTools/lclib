CC*********************************************************************C
C*                                                                     *
C*------------------------                                             *
C* Subroutine PRWCNS(NDTC, NVERS)
C*------------------------                                             *
C*                                                                     *
C*(Purpose)                                                            *
C*    Write the version # of the constants used for the analysis
C*    to the element# 3 of bank "Full_Analysis" of Begin_Run record.   *
C*                                                                     *
C*(Input)
C*    NDTC        ; Detector ID of the constant.                       *
C*    NVERS       ; Version # of the constant.                         *
C*                                                                     *
C*    NDTC = -1 for initialize.                                        *
C*    NDTC = -2 to save versions into bank.                            *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto  28-Oct-1987  Original version.                      *
C*    A. Miyamoto  19-Sep-1989  Add upgraded detector.                 *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRWCNS( NDTC, NVERS )
C
      INTEGER*4  NVLIST(43)
#include "prdtyp.inc"
C
      SAVE       NVLIST
      DATA       IDBGN/2/
 
C==================<<  Entry Point  >>================================
 
C
C ---------------------------------------------------------------------
C (1) Stack version #, if NDTC > 0
C ---------------------------------------------------------------------
C
 
      IF( NDTC .GT. 0 ) THEN
        NVLIST(NDTC) = NVERS
        RETURN
C
C ---------------------------------------------------------------------
C (2) Clear array, if NDTC = -1
C ---------------------------------------------------------------------
C
      ELSEIF( NDTC .EQ. -1 ) THEN
        DO 200 I = 1, 43
          NVLIST(I) = -999
200     CONTINUE
 
C
C ---------------------------------------------------------------------
C (3) Else, save NVLIST into bank.
C ---------------------------------------------------------------------
C
      ELSE
        IEPED = 100*(NDTYPE(9)-1)
        IF( IEPED .LE. 0 ) THEN
          CALL TBCRTE( IDBGN, 'Full_Analysis', 0, 0, IRET)
        ENDIF
C
        CALL TBPUT(IDBGN, 'Full_Analysis', IEPED+3, 43, NVLIST, IRET)
        IF( IRET .LT. 0 ) THEN
           CALL UERROR(1, DUMMY, 1, 'PRWCNS',
     >       'Failed to TBPUT version # of the constants.')
        ENDIF
      ENDIF
C
      RETURN
      END
