C**********************************************************************C
C*                                                                     *
C*==============================================                       *
C* Subroutine TBCxRD( ID, IVERS, TYPE, IW, IDATA, IRET)                *
C*==============================================                       *
C*(Purpose)                                                            *
C*   Read data from the data base file.                                *
C*(Input)                                                              *
C*   ID       : Record ID.                                             *
C*   IVERS    : Version # to get.                                      *
C*         = 0 to get latest version data.                             *
C*         > 0 to get specified version data.                          *
C*   TYPE     : Read type.                                             *
C*         = 'C' to read control data, do direct read.                 *
C*         = 'D' to read data data, do sequential read.                *
C*     This routine must be twice in TBCRED.                           *
C*(Output)                                                             *
C*   IW       : # of words of the data.                                *
C*   IDATA    : Read data.                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET >     0 : Version # of obtained data.                      *
C*          =     0 : When read data block                             *
C*          =    -1 : Record ID not allocated.                         *
C*          =    -2 : Specified version data not exists.               *
C*          =    -2 : Requested record not exists on the data base.    *
C*(Author)                                                             *
C*   A. Miyamoto  Mar. 18 '87  Original version.                       *
C*   A. Miyamoto  Mar. 24 '95  for UNIX system
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBCxRD( ID, IVERS, TYPE, IW, IDATA, IRET)
 
      IMPLICIT INTEGER*4 (H)
      CHARACTER*(*)      TYPE
      INTEGER*4          IDATA(*)
#include "tbsbnk.inc"
#include "tbsmes.inc"
#include "tbcctl.inc"
#include "tbcdat.inc"
 
#ifdef UNIX
#include "tbsfmt.inc"
	character*256 dbsfile,systemcmd
	character*40  tempfile
	logical incstat
	integer getpid
C This routine assumes that this is called from TBCRED twice, 
C at first with TYPE = 'C", then TYPE='D'.
C When TBCxRD is called with TYPE='C', open a file and read first
C record. When TBCxRD is called with TYPE='D", read the second 
C record, and close file.
C  
C
      CHARACTER*64 TNAME
      save nvers,nsavfmt
      logical tbredf, tbxred
      external tbxred
C
C=======< Entry Point >=================================================
C
C
C --------------------------------------------------------------------
C (1) Switch according to the TYPE.
C --------------------------------------------------------------------
C
      IF(TYPE(1:1).NE.'C') GO TO 500
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
C (3) Confirm the status of the data base.
C --------------------------------------------------------------------
C
300   CONTINUE
CC      CALL TBCxVF
C
	nsavfmt=nrwfmt(1)
	nrwfmt(1)=0
	inquire(file=cmxdbs(:lnblnk(cmxdbs)),exist=incstat)
	if( .not.incstat ) then
          mesage(1)='Directory for constants data base does not exists.'
	  mesage(2)='Directory was'
	  mesage(3)=cmxdbs(:lnblnk(cmxdbs))
           CALL TBxERR(800, 'TBCXRD', 3)
	endif
	dbsfile=cmxdbs(:lnblnk(cmxdbs))//'/'//tname(:mcxkyl)
	inquire(file=dbsfile,exist=incstat)
	if( .not.incstat ) then
	  mesage(1)='DBS file '//tname(:mcxkyl)
	  mesage(2)='does not exist in '
	  mesage(3)=cmxdbs(:lnblnk(cmxdbs))
          CALL TBxERR(800, 'TBCXRD', 3)
	  nvers = 1
	elseif(ivers.eq.0) then
	  itim = getpid()
	  write(tempfile,4100) itim
4100      format('/tmp/tbs.ls.',i8.8)
	  write(systemcmd,4110) dbsfile(:lnblnk(dbsfile)),tempfile
4110      format('cd ',a,' && ls 0* | tail -1 > ',a)
	  call  system(systemcmd)
	  open(mcxdbs,file=tempfile)
	  read(mcxdbs,*) nvers
	  close(mcxdbs,status='DELETE')
	  ivers = nvers
	endif
	if( ivers.eq.0 ) ivers = nvers
	write(tempfile,4120) ivers
4120    format(i5.5)
	nvers = ivers
	dbsfile=dbsfile(:lnblnk(dbsfile))//'/'//tempfile
	open(mcxdbs,file=dbsfile,form='Unformatted',
     >	status='old',iostat=ios)
	if(ios.ne.0) then
	  mesage(1) = 'Unable to open DBS file, File name is'
	  mesage(2) = dbsfile
	  call tbxerr(800, 'TBCXRD', 2)
	endif
	tbredf = tbxred( mcxdbs, iw, idata )
	iret = nvers
	nrwfmt(1) = nsavfmt
	return


500     continue	
	nsavfmt=nrwfmt(1)
	nrwfmt(1) = 0
	tbredf = tbxred( mcxdbs, iw, idata )
	close(mcxdbs)
	nrwfmt(1) = nsavfmt
	iret = nvers
	return
#endif



#ifdef MSP
      CHARACTER*64 TNAME, KEY
C
C=======< Entry Point >=================================================
C
C --------------------------------------------------------------------
C (1) Switch according to the TYPE.
C --------------------------------------------------------------------
C
      IF(TYPE(1:1).NE.'C') GO TO 500
C
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
C (3) Confirm the status of the data base.
C --------------------------------------------------------------------
C
300   CONTINUE
      CALL TBCxVF
C --------------------------------------------------------------------
C (4) If IVERS .LE. 0, obtain latest version data.
C --------------------------------------------------------------------
C
      IF(IVERS.LE.0) THEN
        READ(MCxDBS,KEY=TNAME(:MCxKYL),IOSTAT=IOS)
     >    KEY(:MCxKEY), IW, IDATE, ITIME, (IDATA(K),K=1, IW-2)
C
        IF(IOS.EQ.0) THEN
          IF(KEY(MCxKEY:MCxKEY).NE.'C') GO TO 930
          GO TO 410
        ELSEIF(IOS.EQ.158) THEN
          GO TO 920
        ELSE
          GO TO 910
        ENDIF
C
410     CONTINUE
        READ(KEY(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
        IRET  = MCxVSM - ITEMP
C
C (4.2) If IVERS .GT. 0, obtain requested version data.
C
      ELSE
        ITEMP = MCxVSM - IVERS
        WRITE(TNAME(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
        TNAME(MCxKEY:MCxKEY) = 'C'
        READ(MCxDBS,KEY=TNAME(:MCxKEY),IOSTAT=IOS)
     >    KEY(:MCxKEY), IW, IDATE, ITIME, (IDATA(K),K=1, IW-2)
C
        IF(IOS.EQ.0) THEN
          GO TO 430
        ELSEIF(IOS.EQ.158) THEN
          GO TO 920
        ELSE
          GO TO 910
        ENDIF
C
430     CONTINUE
        READ(KEY(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
        IRET  = MCxVSM - ITEMP
      ENDIF
      NCDATE = IDATE
      NCTIME = ITIME
      RETURN
C
C --------------------------------------------------------------------
C (5) Read data block.
C --------------------------------------------------------------------
C
500   CONTINUE
      TNAME              = KEY
      READ(MCxDBS,KEY=TNAME(:MCxKEY-1),IOSTAT=IOS)
     >    KEY(:MCxKEY), IW, IDATE, ITIME
      I1ST               = 1
510   CONTINUE
      READ(MCxDBS,IOSTAT=IOS)
     >    KEY(:MCxKEY), IW, IDATE, ITIME, (IDATA(K),K=I1ST, I1ST+IW-3)
C
      IF(IOS.EQ.0) THEN
        IF( KEY(MCxKEY:MCxKEY) .EQ. 'X' ) GO TO 590
        I1ST = I1ST + IW - 2
        GO TO 510
      ELSEIF(IOS.EQ.158) THEN
        GO TO 940
      ELSE
        GO TO 950
      ENDIF
 
      IRET = 0
C
590   CONTINUE
      IW   = I1ST + IW - 3
      IF(MCxOPS.EQ.2) CALL TBCCLS(IRT)
      RETURN
C
C
C --------------------------------------------------------------------
C (9) Error process.
C --------------------------------------------------------------------
C
910   CONTINUE
      MESAGE(1) = 'Read error while reading control block.'
      MESAGE(2) = 'Requested to read record '//TNAME(:MCxKEY)
      GO TO 999
920   CONTINUE
      MESAGE(1) = 'Record not exist in the data base file.'
      MESAGE(2) = 'Requested to read record '//TNAME(:MCxKEY)
      GO TO 990
930   CONTINUE
      MESAGE(1) = 'Control block data not found'
      MESAGE(2) = 'Requested to read record '//TNAME(:MCxKEY)
      GO TO 999
940   CONTINUE
      MESAGE(1) = 'Data block data not found in the data base file.'
      MESAGE(2) = 'Requested to read record '//TNAME(:MCxKEY)
      GO TO 999
950   CONTINUE
      MESAGE(1) = 'Read error while reading data block'
      MESAGE(2) = 'Requested to read record '//TNAME(:MCxKEY)
      GO TO 999
C
990   CONTINUE
      CALL TBxERR(400, 'TBCxRD', 2)
      IRET = -2
      GO TO 590
C
999   CONTINUE
      CALL TBxERR(800, 'TBCxRD', 2)
      STOP
#endif
      END
