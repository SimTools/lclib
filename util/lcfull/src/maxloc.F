CC*********************************************************************C
C*                                                                     *
C*-------------------------------=====
C* SUBROUTINE MAxLOC(KEY, NVERS, NRET)
C*-------------------------------=====
C*                                                                     *
C*(Purpose)                                                            *
C*   Locate read position to the specified KEY and version #.          *
C*                                                                     *
C*(Input)                                                              *
C*   KEY   ; KEY                                                       *
C*   NVERS ; version #.
C*                                                                     *
C*(Output)                                                             *
C*   NRET  ; return code.
C*        = 0 ; When KEY is found.
C*        =-1 ; When KEY not found.
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto   9-Jan-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAxLOC(KEY, NVERS, NRET)
C
      CHARACTER*(*) KEY
      CHARACTER     KEYTMP*68, MESAGE*137
C
#include "macntl.inc"
#include "maxwrk.inc"
C
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Confirm that data base is not changed.
C ----------------------------------------------------------------------
C
      CALL MAxVRF
C
C ----------------------------------------------------------------------
C (2) Locate at the specified position.
C ----------------------------------------------------------------------
C
      NRET   = 0
      LKEY   = LEN(KEY)
      KEYTMP = KEY
      IF(LKEY.EQ.MCxKYL) THEN
        IF(NVERS.LE.0) THEN
          LKEY = MCxKYL
        ELSE
C         KEYTMP(MCxKYL+1:MCxKYL+1) = ';'
C         IVRS  = MCxVSM - NVERS
C         WRITE(KEYTMP(MCxKYL+2:MCxKEY),FMT=CMxVRS) IVRS
C         LKEY  = MCxKEY
C         KEYTMP(MCxKYL+1:MCxKYL+1) = ';'
          IVRS  = MCxVSM - NVERS
          WRITE(KEYTMP(MCxKYL+1:MCxKEY),FMT=CMxVRS) IVRS
          LKEY  = MCxKEY
        ENDIF
      ENDIF
#ifdef  MSP
      READ(MCxDBS,KEY=KEYTMP(:LKEY),ERR=300)
#endif
#ifdef  MSP
     >     KEYTMP(:MCxKEY), IW, (IBUF(K1),K1=1,IW)
#endif
#ifdef  UNIX
      CALL MAKRED(KEYTMP(:LKEY), KEYTMP(:MCxKEY), IW, IBUF, IRET)
#endif
#ifdef  UNIX
      IF( IRET.LT.0 ) GO TO 300
#endif
      RETURN
C
C ----------------------------------------------------------------------
C (2) When KEY not found.
C ----------------------------------------------------------------------
C
300   CONTINUE
      IF(MCxMSL.GT.3) THEN
        MESAGE = ' '//KEYTMP(:MCxKEY)//' not found '
        CALL MAMSG(4,1,MESAGE)
      ENDIF
      NRET = -1
C
C
      RETURN
      END
