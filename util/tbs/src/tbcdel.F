C**********************************************************************C
C*                                                                     *
C*==============================================                       *
C* Subroutine TBCDEL(ID, IVERS, IRET)
C*==============================================                       *
C*(Purpose)                                                            *
C*   Delete entry from the data base.                                  *
C*(Input)                                                              *
C*   ID       : Record ID.                                             *
C*   IVERS    : Version # to delete.                                   *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET =     1 : When entry is deleted.                           *
C*          =    -1 : Record ID not allocated.                         *
C*          =    -2 : Failed to delete record.                         *
C*                   (Key not exists, version data not exists )        *
C*          =    -3 : Found unexpected KEY.                            *
C*          =    -4 : Can not found expected KEY in data base.         *
C*(Author)                                                             *
C*   A. Miyamoto  Mar. 18 '87  Original version.                       *
C*   A. Miyamoto  May. 30 '91                                          *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBCDEL( ID, IVERS, IRET)
 
#include "tbsmes.inc"
#include "tbcctl.inc"
#ifdef UNIX
	print *,'Dummy subroutine TBCDEL was called.'
	print *,'You don''t need to use TBCDEL on UNIX system'
	return
#endif
#ifdef MSP
      CHARACTER*64 TNAME, KEY
      CHARACTER*1  DATKEY(21)
      DATA  DATKEY/'X', 'W', 'V', 'U', 'T', 'S', 'R', 'Q', 'P', 'O',
     >             'N', 'M', 'L', 'K', 'J', 'I', 'H', 'G', 'F', 'E',
     >             'D'/
C
C=======< Entry Point >=================================================
C
C --------------------------------------------------------------------
C (2) Being requested to read Control data
C --------------------------------------------------------------------
C
C (2.1) Look for the ID_Number ---------------------------------------
C
      CALL TBCxGN( ID, TNAME, IRET)
      IF(IRET.LT.0) RETURN
C
C --------------------------------------------------------------------
C (3) Delete Key.
C --------------------------------------------------------------------
C
300   CONTINUE
      IRET   = 1
      ITEMP  = MCxVSM - IVERS
      WRITE(TNAME(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
      TNAME(MCxKEY:MCxKEY) = 'C'
      DELETE(MCxDBS, IOSTAT=IOSC, KEY=TNAME(:MCxKEY))
C
      READ(MCxDBS,KEY=TNAME(:MCxKYL),ERR=990)
     >     KEY(:MCxKEY)
      IN = 0
      DO 100 I = 1, 21
        IF( KEY(MCxKEY:MCxKEY) .EQ. DATKEY(I)(1:1)) THEN
          IN = I
          GO TO 110
        ENDIF
100   CONTINUE
C
      IF( IN .LE. 0 ) THEN
        MESAGE(1) = 'Failed to delete '//TNAME(:MCxKYL)
        MESAGE(2) = 'Bad KEY found: KEY='//KEY(MCxKEY:MCxKEY)
        CALL TBxERR(400, 'TBCDEL', 2)
        IRET = -3
        GO TO 900
      ENDIF
C
110   CONTINUE
      DO 200 I  = IN, 1, -1
        TNAME(MCxKEY:MCxKEY) = DATKEY(I)(1:1)
        DELETE(MCxDBS, IOSTAT=IOSD, KEY=TNAME(:MCxKEY))
        IF(IOSC.NE.0.OR.IOSD.NE.0) THEN
          MESAGE(1) = 'Failed to delete '//TNAME(:MCxKYL)
          WRITE(MESAGE(2), 310) IOSC, IOSD
310       FORMAT(' IOSTAT =',2I8)
          CALL TBxERR(400, 'TBCDEL', 2)
          IRET = -2
      ENDIF
200   CONTINUE
C
C
900   CONTINUE
      CALL TBCxTS
      RETURN
C
990   CONTINUE
      MESAGE(1) = 'Failed to read '//TNAME(:MCxKYL)
      CALL TBxERR(400, 'TBCDEL', 1)
      IRET = -4
      CALL TBCxTS
      RETURN
#endif
      END
