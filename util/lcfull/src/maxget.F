CC*********************************************************************C
C*                                                                     *
C*---------------------------------------------------================  *
C* SUBROUTINE MAxGET(NMODE, NKEY, MXxSIZ, KEY, NVERS, NW, NDATA, NRET) *
C*---------------------------------------============================  *
C*                                                                     *
C*(Purpose)                                                            *
C*   Get data from data base.                                          *
C*                                                                     *
C*(Input)                                                              *
C*   NMODE  ; Type of access                                           *
C*       0 = Direct access to the each record.                         *
C*       1 = Sequential access except first record.                    *
C*       2 = Sequential access to the all record.                      *
C*       3 = Sequential access all records, all KEY and all version.   *
C*                                                                     *
C*   NKEY  ; # of keys                                                 *
C*   MXxSIZ; Max. size of data array.                                  *
C*   KEY(NKEY) ; Key string.                                           *
C*   NVERS(NKEY) ; Version number whose record is obtained.            *
C*               = 0 to get all version.                               *
C*                                                                     *
C*(Output)                                                             *
C*   KEY(NKEY)   ; Actually obtained Key string.                       *
C*   NVERS(NKEY) ; Version number whose record is obtained.            *
C*   NW(NKEY)    ; # of words of each data elements.                   *
C*             = -1 if the specified key not found.                    *
C*   NDATA(MXxSIZ, NKEY) ; Data array.                                 *
C*   NRET = 0 ; normal return                                          *
C*        > 0 ; Detected end-of-file while reading NRET-th record.     *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto   9-Jan-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAxGET(NMODE,NKEY, MXxSIZ, KEY, NVERS, NW, NDATA, NRET)
C
      CHARACTER*(*) KEY(NKEY)
      INTEGER*4     NVERS(NKEY), NW(NKEY), NDATA(MXxSIZ, NKEY)
C
#include "macntl.inc"
#include "mames.inc"
#include "maxwrk.inc"
C
      CHARACTER*68 KEYTMP, KEYWRK
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Verify that data base is not changed since last access.
C ----------------------------------------------------------------------
C
      MODE   = NMODE
      NRET   = 0
      LINKEY = LEN(KEY(1))
      IF(MODE.EQ.0.OR.MODE.EQ.1) CALL MAxVRF
C
C ----------------------------------------------------------------------
C (2) Read data.
C ----------------------------------------------------------------------
C
      DO 2000 IK = 1, NKEY
        KEYTMP(:MCxKEY) = KEY(IK)
CCC     PRINT *,' IN MAxGET.. KEYTMP=',KEYTMP(:MCxKEY)
C
C (2.1) Determine KEY String to search.
C
C       PRINT *,' MAxGET..MODE, IK=',MODE, IK
        IF(MODE.EQ.0.OR.(MODE.EQ.1.AND.IK.EQ.1)) THEN
          IF(NVERS(IK).LE.0) THEN
            LKEY = MCxKYL
          ELSE
            IVRS = MCxVSM - NVERS(IK)
            WRITE(KEYTMP(MCxKYL+1:MCxKEY),FMT=CMxVRS) IVRS
            LKEY = MCxKEY
          ENDIF
C
C (2.2) Do direct READ
C
CCC       PRINT *,' KEYTMP=',KEYTMP(:LKEY),'==='
#ifdef  MSP
          READ(MCxDBS, KEY=KEYTMP(:LKEY), ERR=220)
#endif
#ifdef  MSP
     >         KEYWRK(:MCxKEY), IW, (IBUF(K1),K1=1,IW )
#endif
#ifdef  UNIX
          CALL MAKRED(KEYTMP(:LKEY), KEYWRK(:MCxKEY),
#endif
#ifdef  UNIX
     >                IW, IBUF, IRET)
#endif
#ifdef  UNIX
          IF( IRET .LT. 0 ) GO TO 220
#endif
CC        PRINT *,' GOT KEY=',KEYWRK(:MCxKEY),'==='
          GO TO 250
220       CONTINUE
          NW(IK) = -1
          GO TO 2000
C
C (2.3) Do sequential read.
C
        ELSE
210       CONTINUE
#ifdef  MSP
          READ(MCxDBS )
#endif
#ifdef  MSP
     >         KEYWRK(:MCxKEY), IW, (IBUF(K1),K1=1,IW )
#endif
#ifdef  UNIX
          CALL MAKRED(' ',KEYWRK(:MCxKEY), IW, IBUF, IRET)
#endif
#ifdef  UNIX
          IF( IRET.LT.0 ) GO TO 800
#endif
CCC       PRINT *,' GOT KEY=',KEYWRK(:MCxKEY),'==='
          IF(KEYWRK(:MCxKEY).EQ.CMxDLM(:MCxKEY)) GO TO 800
          IF(MODE.EQ.3)                          GO TO 250
          IF(KEYWRK(:MCxKYL).NE.KEYTMP(:MCxKYL)) GO TO 210
        ENDIF
C
250     CONTINUE
        KEY(IK)   = KEYWRK(:MCxKYL)
        IF(LINKEY.GE.MCxKEY) KEY(IK) = KEYWRK(:MCxKEY)
        IF(KEYWRK(MCxKYL+1:MCxKEY).EQ.CMxBLK(MCxKYL+1:MCxKEY)) THEN
           IVRS = 0
        ELSE
           READ(KEYWRK(MCxKYL+1:MCxKEY),*) IVRS
        ENDIF
        NVERS(IK) = MCxVSM - IVRS
        NW(IK)    = IW
        IF(IW.GT.MXxSIZ) NW(IK) = MXxSIZ
        CALL UVCOPY(NW(IK), IBUF, NDATA(1,IK))
C
2000  CONTINUE
      RETURN
C
C ----------------------------------------------------------------------
C (8) Error message.
C ----------------------------------------------------------------------
C
800   CONTINUE
      NRET = IK
      IF(MODE.NE.3) THEN
        WRITE(MESAGE(1), 810) IK
810     FORMAT(' %Warning_MAxGET..Read End-of-File while reading ',
     >    I4,'-th Key''s record.')
        CALL MAxERR(400, 'MAxGET', 1)
      ENDIF
      RETURN
      END
