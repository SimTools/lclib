C**********************************************************************
C*
C* --------------------------
C* Subroutine MAPURG( PRGKEY , NRET )
C* --------------------------
C*
C*(Function)
C*   Purge old version record.
C*
C*(Input)
C*   PRGKEY : Key to be deleted.
C*
C*(Output)
C*   NRET   : Return code.
C*           =  0  normal return
C*           = -1  when target key not found.
C*
C*(Author)
C*   A.Miyamoto  9-Dec-1987  Original version.
C*
C*
C**********************************************************************
C*
      SUBROUTINE MAPURG( PRGKEY , NRET )
C*
#include "macntl.inc"
C*
      CHARACTER*(*) PRGKEY
      PARAMETER    (MXxBUF=30)
      CHARACTER*64  KEYWRK, KEYBUF(MXxBUF), KEYKEP
      PARAMETER  (MXxREC=1000)
      COMMON /VSAMWK/ NW, NBUF(MXxREC), IW, IBUF(MXxREC)
      CHARACTER     MESAGE*130
C
C ======< Entry Point >================================================
C
C ---------------------------------------------------------------------
C (1) Decide target Key.
C ---------------------------------------------------------------------
C
#ifdef  UNIX
      IRET=1
      IF( IRET.EQ.1 ) THEN
        PRINT *,'MAPURG .. This routine is not supported on UNIX.'
        STOP
      ENDIF
#else
      IF( MCxKEY .NE. 20 ) THEN
        MESAGE = ' %Error..MAPURG Can not MAPURG for this data base.'
        CALL MAMSG(1, 1, MESAGE )
        NRET = -2
        RETURN
      ENDIF
C
      LPRG = LEN(PRGKEY)
#ifdef  MSP
      READ(MCxDBS,KEY=PRGKEY(:LPRG),ERR=110)
#endif
#ifdef  MSP
     >     KEYWRK(:MCxKEY), IW, (IBUF(I),I=1,IW)
#endif
      GO TO 190
110   CONTINUE
      MESAGE = ' %Warning..MAPURG Key '//PRGKEY(:LPRG)//' not found.'
      CALL MAMSG( 11, 1, MESAGE )
      NRET = -1
      RETURN
C
C ---------------------------------------------------------------------
C (2) Loop to purge old version key.
C ---------------------------------------------------------------------
C
190   CONTINUE
      NPURGE = 0
200   CONTINUE
      NUMSTK = 0
      KEYKEP = KEYWRK
      NW     = IW
      CALL UVCOPY(NW, IBUF, NBUF )
210   CONTINUE
#ifdef  MSP
      READ(MCxDBS,ERR=230)
#endif
#ifdef  MSP
     >    KEYWRK(:MCxKEY), IW, (IBUF(I),I=1,IW)
#endif
      GO TO 300
230   CONTINUE
      MESAGE = ' %Error..MAPURG Read error detected.'
      CALL MAMSG( 1, 1, MESAGE )
      NRET = -1
      RETURN
C
300   CONTINUE
      IF(KEYWRK(:MCxKEY).EQ.CMxDLM(:MCxKEY).OR.
     >   KEYWRK(:LPRG).NE.PRGKEY(:LPRG)) THEN
        IF( NUMSTK.NE.0 ) GO TO 400
        GO TO 600
      ENDIF
      IF(KEYWRK(:MCxKYL) .EQ. KEYKEP(:MCxKYL) ) THEN
        NUMSTK = NUMSTK + 1
        KEYBUF(NUMSTK) = KEYWRK
        GO TO 210
      ENDIF
C
C ---------------------------------------------------------------------
C (4) Delete all key in stack.
C ---------------------------------------------------------------------
C
      IF( NUMSTK .EQ. 0 ) GO TO 200
400   CONTINUE
      NUMSTK = NUMSTK + 1
      KEYBUF(NUMSTK) = KEYKEP
      DO 420 I = 1, NUMSTK
#ifdef  MSP
        DELETE(MCxDBS,KEY=KEYBUF(I)(:MCxKEY),ERR=410)
#endif
        IF( I .EQ. NUMSTK ) GO TO 420
        MESAGE = ' Delete '//KEYBUF(I)(:MCxKEY)
        CALL MAMSG(11, 1, MESAGE)
        GO TO 420
410     CONTINUE
        MESAGE = ' %Error..MAPURG .. Failed to delete Key '//KEYBUF(I)
        CALL MAMSG( 1, 1, MESAGE)
        NRET   = -1
        RETURN
420   CONTINUE
C
      NPURGE = NPURGE + 1
      KEYKEP(MCxKYL+1:MCxKEY) = CMxDLM(MCxKYL+1:MCxKEY)
      WRITE(MCxDBS) KEYKEP(:MCxKEY), NW, (NBUF(K),K=1,NW)
      MESAGE = ' '//KEYBUF(NUMSTK)(:MCxKEY)//
     >         ' saved as '//KEYKEP(:MCxKEY)
      CALL MAMSG(11, 1, MESAGE )
      READ(MCxDBS, KEY=KEYWRK(:MCxKEY))
     >     KEYWRK(:MCxKEY), IW, (IBUF(K),K=1,IW)
      IF( KEYWRK(:LPRG).EQ.PRGKEY(:LPRG))  GO TO 200
C
600   CONTINUE
      WRITE(MESAGE,610) NPURGE
610   FORMAT(' Purge ',I6,' Keys.')
      CALL MAMSG(11, 1, MESAGE)
      NRET = 0
#endif
      RETURN
      END
