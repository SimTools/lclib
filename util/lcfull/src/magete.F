C**********************************************************************
C*
C* --------------------------------------============
C* Subroutine MAGETE(TYPE, NEXP, MXxBUF, NWORDS, NBUF)
C* --------------------------------------============
C*
C*(Function)
C*   Get Experimental condition data.
C*
C*(Input)
C*   TYPE   : Type of the Key.
C*            = 'EXPLIST'  NEXP no meanings.
C*            = 'RUNLIST'
C*            = 'PRODSIM'
C*            = 'SIMCONS'
C*
C* 1) EXPLIST
C*      # words = 3
C*        IBUF(1) = last updated  date
C*            (2) =               Time
C*            (3) = latest experiment #.
C*
C* 2) RUNLIST   ; Key = 'Ennnnn.RUNLIST'
C*      # words = 12
C*    IBUF(1) = Date when the largest run # info. is saved.
C*        (2) = Time
C*        (3) = Experiment # of largest run # run.
C*        (4) = Run # of largest run.
C*        (5) = Format tape vol=ser.
C*        (6) =      "
C*        (7) = Date when the smallest run # info. is saved.
C*        (8) = Time
C*        (9) = Experiment # of smallest run # run.
C*       (10) = Run # of smallest run.
C*       (11) = Format tape vol=ser.
C*       (12) =      "
C*
C* 3) PRODSIM   ; Key = 'Ennnnn.PRODSIM'
C*      version number of the constants to product simulator data.
C*      # words = n
C*        IBUF(1) = last updated  date
C*            (2) =               Time
C*            (3) = # of version # combination.
C*            (4) = # of words of each pair.
C*            (5) = start run #.
C*            (6) = last run #.
C*            (7) = IDC constant version.
C*            (8) = TPC constant version.
C*            (9) = BTOF constant version.
C*           (10) = BDC constant version.
C*           (11) = BCL constant version.
C*           (12) = BMU constant version.
C*           (13) = EDC constant version.
C*           (14) = ECL constant version.
C*           (15) = LUM constant version.
C*           (16) = Steering. constant version.
C*           (17) = VTX constant version.
C*           (18) = TCH constant version.
C*           (19) = FCL constant version.
C*           (20) = RCL constant version.
C*
C* 4) SIMCONS   ; Key = 'Ennnnn.PRODSIM'
C*      version number of the constants for the simulation.
C*      # words = n
C*        IBUF(1) = last updated  date
C*            (2) =               Time
C*            (3) = # of version # combination.
C*            (4) = # of words of each pair.
C*            (5) = start run #.
C*            (6) = last run #.
C*            (7) = Simulator Steering constants version.
C*            (8) = Trigger simulator constant version.
C*            (9) = IDC constant version.
C*           (10) = TPC constant version.
C*           (11) = BTOF constant version.
C*           (12) = BDC constant version.
C*           (13) = BCL constant version.
C*           (14) = BMU constant version.
C*           (15) = EDC constant version.
C*           (16) = ECL constant version.
C*           (17) = LUM constant version.
C*           (18) = VTX constant version.
C*           (19) = TCH constant version.
C*           (20) = FCL constant version.
C*           (21) = RCL constant version.
C*
C*   NEXP   ; Experiment #
C*   MXxBUF ; Size of NBUF.
C*
C*(Output)
C*   NWORDS ; # of data in NBUF.
C*        = -1 when the key not found.
C*   NBUF   ; Run information array.
C*
C*(Author)
C*
C*  A. Miyamoto.  19-Feb-1988   Original version.
C*                25-Aug-1988   To support FILELIST record.
C*                 7-Aug-1990   If 16-th character of KEY is not blank,
C*                              it has continuation KEY, and read
C*                              continuatoin KEY togather. If it is
C*                              'Z', this is last KEY.
C*
C**********************************************************************
C*
      SUBROUTINE MAGETE(TYPE, NEXP, MXxBUF, NWORDS, NBUF)
C
#include "macntl.inc"
#include "maxwrk.inc"
#include "mames.inc"
C
      EXTERNAL MABLKD
 
      INTEGER*4       NBUF(MXxBUF)
      CHARACTER       KEY*20, TYPE*(*), KEYTMP*20
      CHARACTER*1     ALPHAB(26)
      DATA   ALPHAB/'A','B','C','D','E',   'F','G','H','I','J',
     >              'K','L','M','N','O',   'P','Q','R','S','T',
     >              'U','V','W','X','Y',   'Z'/
C
C============< Entry Point >=========================================
C
C -------------------------------------------------------------------
C (1) Set KEY string.
C -------------------------------------------------------------------
C
      LTYP = LEN(TYPE)
      IF( TYPE(1:MIN(7,LTYP)) .EQ. 'EXPLIST' ) THEN
        KEY = 'EXPLIST'
      ELSEIF( TYPE(1:MIN(8,LTYP)) .EQ. 'FILELIST' ) THEN
        KEY = 'FILELIST'
      ELSE
#ifdef MSP
        WRITE(KEY,'(''E'',I5.5,''.'',A)') NEXP, TYPE(:LTYP)
#endif
#ifdef UNIX
        WRITE(KEY,'(''E'',I5.5,''/'',A)') NEXP, TYPE(:LTYP)
#endif
      ENDIF
C
C
      LKEY = INDEX(KEY,' ') - 1
      IPED = 0
 
200   CONTINUE
CC    PRINT *,' In MAGETE..MCxDBS=',MCxDBS,' KEY=',KEY,' LKEY=',LKEY
#ifdef  MSP
      READ(MCxDBS, KEY=KEY(:LKEY), ERR=900)
#endif
#ifdef  MSP
     >         KEYTMP, IW, (IBUF(K1),K1=1,IW )
#endif
#ifdef  UNIX
      CALL MAKRED(KEY(:LKEY), KEYTMP, IW, IBUF, IRET)
#endif
#ifdef  UNIX
      IF( IRET.LT. 0 ) GO TO 900
#endif
CC    PRINT *,' Read one record  KEY=',KEYTMP,' IW=',IW,' IPED=',IPED
      NW     = IW
      IF( IPED + NW     .GT. MXxBUF ) NW     = MXxBUF - IPED
      IF( IPED .EQ. 0 ) THEN
        CALL UVCOPY( NW    , IBUF(1), NBUF(IPED+1) )
        NWORDS = IPED + NW
      ELSE
        CALL UVCOPY( NW - 2, IBUF(3), NBUF(IPED+1) )
        NWORDS = IPED + NW - 2
      ENDIF
 
CC    PRINT *,' NWORDS=',NWORDS
C
C .. If this is last KEY, do normal return.
      IF( KEYTMP(MCxKYL:MCxKYL) .EQ. ' ' .OR.
     >    KEYTMP(MCxKYL:MCxKYL) .EQ. 'Z' )      RETURN
C
C .. If overflows, return
      IPED = NWORDS
      IF( IPED .GE. MXxBUF ) THEN
        WRITE(MESAGE(1), 300)
300     FORMAT(' %Warning_MAGETE..record size too large, data ',
     >       'truncated.')
        WRITE(MESAGE(2), 310) KEY, MXxBUF
310     FORMAT('   KEY =',A,' buffer size=',I4)
          CALL MAxERR(400, 'MAGETE', 2)
      ENDIF
C
C ..  Read continuation Key.
C
      DO 410 I = 26, 1, -1
        IF( ALPHAB(I) .NE. KEYTMP(MCxKYL:MCxKYL)) GO TO 410
        GO TO 420
410   CONTINUE
      WRITE(MESAGE(1),430) KEYTMP
430   FORMAT(' %Error_MAGETE.. Read Invalid KEY =',A)
      CALL MAxERR(800, 'MAGETE', 1)
      STOP
C
420   CONTINUE
      KEY = KEYTMP(:MCxKYL-1)//ALPHAB(I+1)
      LKEY = MCxKYL
CC    PRINT *,'Next KEY will be ',KEY,' LKEY=',LKEY
CC    PRINT *,'     KEYTMP=',KEYTMP
CC    PRINT *,'     MCxKYL=',MCxKYL
      GO TO 200
C
900   CONTINUE
      IF (IPED.EQ.0) THEN
        NWORDS = -1
        RETURN
      ELSE
        WRITE(MESAGE(1),910) KEY(:LKEY)
910     FORMAT(' %Error_MAGETE..Error to read KEY=',A,' ')
        CALL MAxERR(800, 'MAGETE', 1)
        STOP
      ENDIF
 
      END
