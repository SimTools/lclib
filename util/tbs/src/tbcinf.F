C**********************************************************************C
C*                                                                     *
C*==============================================                       *
C* Subroutine TBCINF(ID, MAXSIZ, NUMVRS, NVERS, NDATE, NTIME)
C*==============================================                       *
C*(Purpose)                                                            *
C*   Returns the number of version, version number, date and time      *
C*   of the data base record.
C*(Input)                                                              *
C*   ID       : Record ID.                                             *
C*   MAXSIZ   : Size of array.                                         *
C*(Output)                                                             *
C*   NUMVRS   : # of version .
C*          = >0  # of version on the file.
C*          =  0  Record not exists on the data base file.             *
C*          = -1  Record ID not allocated.                             *
C*   NVERS    : Version number of i-th version.                        *
C*   NDATE    : Date when data is saved.   ( TOPAZ standard format)    *
C*   NTIME    : Time when data is saved.
C*(Author)                                                             *
C*   A. Miyamoto  Mar. 18 '87  Original version.                       *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBCINF(ID, MAXSIZ, NUMVRS, NVERS, NDATE, NTIME)
 
      INTEGER*4  NVERS(MAXSIZ), NDATE(MAXSIZ), NTIME(MAXSIZ)
 
#include "tbsmes.inc"
#include "tbcctl.inc"
#ifdef UNIX
	print *,'Dummy subroutine TBCINF was called.'
	print *,'You don''t need to use TBCINF on UNIX system'
	return
#endif
#ifdef MSP
 
      CHARACTER*64 TNAME, KEY
C
C=======< Entry Point >=================================================
C
C --------------------------------------------------------------------
C (1) Look for the ID_Number.
C --------------------------------------------------------------------
C
      CALL TBCxGN( ID, TNAME, IRET)
      IF(IRET.LT.0) THEN
        NUMVRS = 0
        RETURN
      ENDIF
C
C --------------------------------------------------------------------
C (2) Confirm data base status.
C --------------------------------------------------------------------
C
      CALL TBCxVF
C
C --------------------------------------------------------------------
C (3) Read data base file.
C --------------------------------------------------------------------
C
      READ(MCxDBS,KEY=TNAME(:MCxKYL),IOSTAT=IOS)
     >     KEY(:MCxKEY), IW, IDATE, ITIME
      IF(IOS.EQ.0) THEN
        GO TO 310
      ELSEIF(IOS.EQ.158) THEN
        GO TO 910
      ELSE
        GO TO 920
      ENDIF
C
310   CONTINUE
      NUMVRS = 1
      READ(KEY(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
      NVERS(NUMVRS) = MCxVSM - ITEMP
      NDATE(NUMVRS) = IDATE
      NTIME(NUMVRS) = ITIME
C
C --------------------------------------------------------------------
C (4) Loop till completed to read requested number of data.
C --------------------------------------------------------------------
C
      DO 400 I = 2, MAXSIZ
410     READ(MCxDBS, ERR=920) KEY(:MCxKEY), IW, IDATE, ITIME
        IF( KEY(MCxKEY:MCxKEY) .NE. 'X' )   GO TO 410
        READ(MCxDBS, ERR=920) KEY(:MCxKEY), IW, IDATE, ITIME
        IF(KEY(:MCxKEY).EQ.CMxDLM(:MCxKEY)) GO TO 450
        IF(KEY(:MCxKYL).NE.TNAME(:MCxKYL))  GO TO 450
        IF(KEY(MCxKEY:MCxKEY).NE.'C')       GO TO 930
        NUMVRS = NUMVRS + 1
        READ(KEY(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
        NVERS(NUMVRS) = MCxVSM - ITEMP
        NDATE(NUMVRS) = IDATE
        NTIME(NUMVRS) = ITIME
400   CONTINUE
C
450   CONTINUE
      GO TO 990
C
C --------------------------------------------------------------------
C (9) Error handling.
C --------------------------------------------------------------------
C
910   CONTINUE
      MESAGE(1) = 'Record does not exists in the data base.'
      MESAGE(2) = 'Record name is '//TNAME(:MCxKYL)
      CALL TBxERR( 400, 'TBCINF', 2)
      NUMVRS    = -2
      GO TO 990
C
920   CONTINUE
      MESAGE(1) = 'Read error while reading data base file.'
      MESAGE(2) = 'Record name is '//TNAME(:MCxKEY)
      CALL TBxERR( 800, 'TBCINF', 2)
      STOP
C
930   CONTINUE
      MESAGE(1) = 'Invalid control and data block sequence.'
      MESAGE(2) = 'Record name is '//TNAME(:MCxKEY)
      CALL TBxERR( 800, 'TBCINF', 2)
      STOP
C
990   CONTINUE
      IF(MCxOPS.EQ.2) CALL TBCCLS(IRT)
      RETURN
#endif
      END
