CC*********************************************************************C
C*                                                                     *
C*------------------------                                             *
C* SUBROUTINE MAxEST(NEXP)                                             *
C*------------------------                                             *
C*                                                                     *
C*(Purpose)                                                            *
C*  update   EXPLIST record.  This record contains the experiment #    *
C*  of latest run.                                                     *
C*                                                                     *
C*   Record format is,                                                 *
C*          IBUF(1) = Data saved date.                                 *
C*          IBUF(2) = Data saved time.                                 *
C*          IBUF(3) = Current Experiment #. (Only one word.)           *
C*                                                                     *
C*  This record is updated only when experiment # changes.             *
C*                                                                     *
C*(Input)                                                              *
C*   NEXP  ; Experiment # of the last one.                             *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  12-Jan-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAxEST(NEXP)
C
#include "macntl.inc"
#include "maxwrk.inc"
C
      CHARACTER*68 KEY, KEYWRK
      CHARACTER*137 MESAGE
C
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Overwrite EXPLIST record.
C ----------------------------------------------------------------------
C
      KEY = 'EXPLIST'
#ifdef  UNIX
      IF( KEY(1:1).EQ.'E') THEN
#endif
#ifdef  UNIX
        PRINT *,'MAxEST .. This routine is not supported on UNIX.'
#endif
#ifdef  UNIX
        STOP
#endif
#ifdef  UNIX
      ENDIF
#endif
#ifdef  MSP
      READ(MCxDBS,KEY=KEY(:MCxKYL)) KEYWRK(:MCxKEY),
#endif
#ifdef  MSP
     >      NW, (IBUF(I), I=1,NW)
#endif
      NW  = 3
      CALL UIDATE(IBUF(1))
#ifdef  MSP
      CALL   TIME(IBUF(2))
#endif
#ifdef  MSP
      CALL  UITIME(IBUF(2))
#endif
      IBUF(3) = NEXP
#ifdef  MSP
      REWRITE(MCxDBS,ERR=800)
#endif
#ifdef  MSP
     >               KEYWRK(:MCxKEY), NW, (IBUF(I),I=1,NW)
#endif
      CALL MAMSG(1, 1, ' MAxEST .. EXPLIST key updated.')
C
      RETURN
 
C
C (8) Error.
C
800   CONTINUE
      MESAGE = ' %Warning_MAxEST..Failed to update EXPLIST key.'
      CALL MAMSG(1,1,MESAGE)
      RETURN
      END
