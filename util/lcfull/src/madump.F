C**********************************************************************
C*
C* ----------------------
C* Subroutine MADUMP(MODE, KEY)
C* ----------------------
C*
C*(Function)
C*   Dump contents of the data base.
C*
C*(Input)
C*   MODE ; Dump mode.
C*      0 = Dump by Z*8
C*      1 = Dump by I*4, R*4, Z*8, and A*4
C*   KEY  ; Key to be dumped.
C*
C*(Author)
C*
C*  A. Miyamoto.  12-Jan-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MADUMP(MODE, KEY)
C
#include "macntl.inc"
#include "maxwrk.inc"
C
      CHARACTER*(*) KEY
      CHARACTER*137 MESAGE
      CHARACTER*68  BLANK, KEYTMP, KEYWRK, DELIM, KEYOLD
      EQUIVALENCE  (IDATA, RDATA)
      DATA  BLANK/' '/
      DATA  DELIM
     > /'999999999999999999999999999999999999999999999999999999999'/
C
C============< Entry Point >=========================================
C
      CALL MAxVRF
C
C--------------------------------------------------------------------
C (1) If KEY=Blank, then prompt KEY string.
C--------------------------------------------------------------------
C
      LKEY = LEN(KEY)
      LOLD = LKEY
      KEYTMP = KEY
      ILOOP  = 0
      IF(KEYTMP(:LKEY).EQ.BLANK(:LKEY)) ILOOP = 1
100   CONTINUE
      IF(ILOOP.EQ.1) THEN
        CALL PROMPT(2,' Enter Key to Dump ? >',IRT)
        READ(MCxMGT,'(A)') KEYTMP
        DO 110 I = 68, 1, -1
          IF(KEYTMP(I:I).NE.' ') GO TO 120
110     CONTINUE
        GO TO 900
120     LKEY = I
        IF(KEYTMP(LKEY:LKEY).EQ.'''') THEN
          KEYWRK = KEYTMP(2:LKEY-1)
          KEYTMP = KEYWRK
          LKEY   = LKEY - 2
        ENDIF
      ENDIF
      IF(KEYTMP(1:1).EQ.'N') THEN
        KEYTMP = KEYOLD
        LKEY   = LOLD
        READ(KEYTMP(9:14),*) IRUN
        IRUN = IRUN + 1
        WRITE(KEYTMP(9:14),'(I6.6)') IRUN
      ENDIF
C
C--------------------------------------------------------------------
C (2) Read data.
C--------------------------------------------------------------------
C
200   CONTINUE
#ifdef  MSP
      READ(MCxDBS,KEY=KEYTMP(:LKEY),ERR=800)
#endif
#ifdef  MSP
     >     KEYWRK(:MCxKEY), IW, (IBUF(K1),K1=1,IW)
#endif
#ifdef  UNIX
      CALL MAKRED(KEYTMP(:LKEY), KEYWRK(:MCxKEY), IW, IBUF, IRET)
#endif
#ifdef  UNIX
      IF( IRET.LT.0 ) GO TO 800
#endif
C
220   CONTINUE
      KEYOLD = KEYWRK
      LOLD   = LKEY
      WRITE(MCxMPT, 210) KEYWRK(:MCxKEY), IW
210   FORMAT(' Key=',A,' #words=',I5)
      IF(MODE.EQ.1) THEN
        DO 230 I = 1, IW
          IDATA  = IBUF(I)
          WRITE(MCxMPT,240) I, IBUF(I), RDATA, IBUF(I), IBUF(I)
240       FORMAT(' #',I5,I15,G16.7,2X,Z8,2X,A4)
230     CONTINUE
      ELSE
        WRITE(MCxMPT,250) (IBUF(K1),K1=1,IW)
250     FORMAT( '     ',20X,' Data  =',5(1X,Z8),:,/,
     >  20('     ',20X,'        ',5(1X,Z8),:,/) )
      ENDIF
C
C--------------------------------------------------------------------
C (3) Search rest of the key.
C--------------------------------------------------------------------
C
300   CONTINUE
#ifdef  MSP
      READ(MCxDBS) KEYWRK(:MCxKEY),
#endif
#ifdef  MSP
     >      IW, (IBUF(K1),K1=1,IW)
#endif
#ifdef  UNIX
       CALL MAKRED(' ',KEYWRK(:MCxKEY), IW, IBUF, IRET)
#endif
#ifdef  UNIX
       IF( IRET.LT.0 ) GO TO 890
#endif
      IF(KEYWRK(:MCxKEY).EQ.DELIM(:MCxKEY)) GO TO 890
      IF(KEYWRK(:LKEY).NE.KEYTMP(:LKEY))    GO TO 890
C
      GO TO 220
C
C--------------------------------------------------------------------
C (8) When Key not found.
C--------------------------------------------------------------------
C
800   CONTINUE
      MESAGE = ' Key '//KEYTMP(:LKEY)//' not found.'
      CALL MAMSG(1, 1, MESAGE)
      GO TO 890
C
890   CONTINUE
      IF(ILOOP.EQ.1) GO TO 100
 
900   CONTINUE
      RETURN
      END
