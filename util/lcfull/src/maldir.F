C**********************************************************************
C*
C* -----------------
C* Subroutine MALDIR(MODE)
C* -----------------
C*
C*(Function)
C*   List Directory of the data base to the SYSOUT file.
C*
C*(Input)
C*   MODE   ; Type of list.
C*
C*
C*(Author)
C*
C*  A. Miyamoto.   9-Jan-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MALDIR(MODE)
C
#include "macntl.inc"
#include "maxwrk.inc"
C
      EXTERNAL MABLKD
      CHARACTER*68 KEY
      CHARACTER*8   HIZUKE, JIKAN
C
C============< Entry Point >=========================================
C
#ifdef HIUXF77
      CALL MABLKD
#endif
      CALL MAxVRF
C
C--------------------------------------------------------------------
C (1) Get Header.
C--------------------------------------------------------------------
C
      KEY = '      '
#ifdef  MSP
      READ(MCxDBS,KEY=KEY(:1))
#endif
#ifdef  MSP
     >     KEY(:MCxKEY),IW,(IBUF(K1),K1=1,IW)
#endif
#ifdef  UNIX
      KEY = '0000000'
#endif
#ifdef  UNIX
      CALL MAKRED(KEY(:7),KEY(:MCxKEY),IW,IBUF,IRET)
#endif
      CALL UCDATE(IBUF( 1), HIZUKE)
      CALL UCTIME(IBUF( 2), JIKAN)
      WRITE(MCxMPT,110) HIZUKE, JIKAN, (IBUF(K1),K1=3,9)
110   FORMAT(/,
     > ' ****** Data Base KEY directory list *********************',/,
     > '        Created  on ',A,2X,A,' By ',4A4,/,
     > '        Key length Total =',I5,' Valid=',I5,' Max. Vers=',I5,
     > /)
C
C--------------------------------------------------------------------
C (2) Loop till found End-of-File.
C--------------------------------------------------------------------
C
      NENTRY = 0
      CALL UVZERO(22, IBUF)
200   CONTINUE
#ifdef  MSP
      READ(MCxDBS) KEY(:MCxKEY), IW, (IBUF(K1),K1=1,2)
#endif
#ifdef  UNIX
      CALL MAKRED(' ',KEY(:MCxKEY), IW, IBUF, IRET)
#endif
#ifdef  UNIX
      IF( IRET.LT.0 ) GO TO 300
#endif
      IF(KEY(:MCxKEY).EQ.CMxDLM(:MCxKEY)) GO TO 300
      NENTRY = NENTRY + 1
      CALL UCDATE(IBUF( 1), HIZUKE)
      CALL UCTIME(IBUF( 2), JIKAN)
      WRITE(MCxMPT,220) HIZUKE, JIKAN, IW, KEY(:MCxKEY)
 
220   FORMAT(1X,A,' ',A,' ',I8,'  ',A)
      CALL UVZERO(2, IBUF)
      GO TO 200
 
C
C--------------------------------------------------------------------
C (3) Read end of file.
C--------------------------------------------------------------------
C
300   CONTINUE
#ifdef  MSP
      READ(MCxDBS,KEY=CMxDLM(:MCxKEY)) KEY(:MCxKEY),
#endif
#ifdef  MSP
     >     IW, (IBUF(K1),K1=1,IW)
#endif
#ifdef  MSP
      CALL UCDATE(IBUF( 1), HIZUKE)
#endif
#ifdef  MSP
      CALL UCTIME(IBUF( 2), JIKAN)
#endif
#ifdef  MSP
      WRITE(MCxMPT,310) NENTRY, HIZUKE, JIKAN, (IBUF(K1),K1=3,6)
#endif
#ifdef  MSP
310   FORMAT(/,' There are ',I6,' entries.',/,
#endif
#ifdef  MSP
     >         ' Last update on ',A,2X,A,' by ',4A4)
#endif
#ifdef  UNIX
      PRINT *,'MALDIR .. Reached end of gile'
#endif
#ifdef  UNIX
      PRINT *,'There are ',NENTRY,' entries.'
#endif
      CALL UVZERO(22, IBUF)
      RETURN
      END
