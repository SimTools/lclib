C *********************************************************************C
C*                                                                     *
C*==============================================                       *
C* Subroutine TBCxWT(TNAME, IVERS, NWORDS, NRET)                       *
C*==============================================                       *
C*(Purpose)                                                            *
C*   Output packed data to the data base.                              *
C*(Input)                                                              *
C*   TNAME    : Record name                                            *
C*   IVERS    : Version number.                                        *
C*          = 0 to save as latest version data.                        *
C*          > 0 to save as given version number data.                  *
C*   NWORDS   : Data size of data block to write                       *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*        > 0 : Actual version number.                                 *
C*        =-3 : Write error.                                           *
C*(Author)                                                             *
C*   A. Miyamoto  Mar. 18 '87  Original version.                       *
C*                Jul.  8 '87  Devide data records to several block.   *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBCxWT( TNAME, IVERS, NWORDS, NRET)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsctl.inc"
#include "tbsmes.inc"
#include "sswork.inc"
#include "tbsiob.inc"
#include "tbcctl.inc"
#include "tbcdat.inc"
 
#ifdef MSP
      CHARACTER*64 TNAME, KEY
      DATA         NPRINT/0/, MXPRNT/10/
      CHARACTER*1  DATKEY(21)
      DATA  DATKEY/'X', 'W', 'V', 'U', 'T', 'S', 'R', 'Q', 'P', 'O',
     >             'N', 'M', 'L', 'K', 'J', 'I', 'H', 'G', 'F', 'E',
     >             'D'/
      PARAMETER   ( LNxBLK = 11500 )
#endif
C
C=======< Entry Point >=================================================
C
#ifdef UNIX
	print *,'Dummy subroutine, tbcxwt was called.'
#endif
#ifdef MSP
C  -------------------------------------------------------------------
C (1) If IVERS = 0, search actual version # to put.
C  -------------------------------------------------------------------
C
CC      PRINT *,' TBCxWT=',IVERS
      NVERS = IVERS
      IF(NVERS.GT.0) GO TO 200
C
      READ(MCxDBS,KEY=TNAME(:MCxKYL),ERR=120)
     >     KEY(:MCxKEY)
      READ(KEY(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
      NVERS = MCxVSM - ITEMP + 1
      GO TO 200
120   CONTINUE
          NVERS = 1
C
C --------------------------------------------------------------------
C  (2) First output control block data.
C --------------------------------------------------------------------
C
200   CONTINUE
      PRINT *,' NVERS = ',NVERS
      NRET                = NVERS
      IDATE = NCDATE
      ITIME = NCTIME
      KEY                 = TNAME(:MCxKYL)
      ITEMP               = MCxVSM - NVERS
      WRITE(KEY(MCxKYL+1:MCxKEY-1),FMT=CMxVRS) ITEMP
      KEY(MCxKEY:MCxKEY)  = 'C'
      IW                  = NWCNTL + 2
      WRITE(MCxDBS,IOSTAT=IOS)
     .      KEY(:MCxKEY), IW, IDATE, ITIME, (ICNTL(K),K=1,NWCNTL)
C
      IEXIST = 0
      IF(IOS.EQ.0) THEN
        GO TO 300
      ELSEIF(IOS.EQ.159) THEN
         REWRITE(MCxDBS,ERR=220)
     .      KEY(:MCxKEY), IW, IDATE, ITIME, (ICNTL(K),K=1,NWCNTL)
        IEXIST = 1
        GO TO 300
      ELSE
        WRITE(MESAGE(1),210) IOS
210     FORMAT('Failed to write Control data to data base, IOS=',I6)
        MESAGE(2) = 'KEY ='//KEY(:MCxKEY)
        CALL TBxERR(800, 'TBCxWT', 2)
        STOP
      ENDIF
C
220   CONTINUE
      MESAGE(1) = 'Failed to write control data to the data base.'
      CALL TBxERR(800, 'TBCxWT', 1)
      STOP
C
C --------------------------------------------------------------------
C  (3) Write data block to the data base.
C --------------------------------------------------------------------
C
300   CONTINUE
C
      NBLK  = (NWORDS-1)/LNxBLK  + 1
      NW    = NWORDS
      IF( NWORDS .GT. MXWORK ) GO TO 500
 
C .. When NWORDS is less than SSWORK size, copy all data to SSWORK
 
      IBGN     = 1
      DO 410 L = 1, NREC
         NBGN  = NBEGIN(L)
         NWS   = NFINAL(L) - NBGN + 1
         CALL UVCOPY( NWS, IBUFF( NBGN ), IWORK( IBGN ))
         IBGN  = IBGN + NWS
410   CONTINUE
 
      IBGN  = 1
      DO 4000 IBLK = NBLK, 1, -1
        KEY(MCxKEY:MCxKEY) = DATKEY(IBLK)
        IF( NW .LT. LNxBLK ) THEN
           IEND = IBGN + NW - 1
           NW   = 0
        ELSE
           IEND = IBGN + LNxBLK - 1
           NW   = NW   - LNxBLK
        ENDIF
        IW  = IEND - IBGN + 3
 
        IF(IEXIST.EQ.0) THEN
          WRITE(MCxDBS,ERR=380) KEY(:MCxKEY), IW, IDATE, ITIME,
     >       (IWORK(K),K=IBGN,IEND)
        ELSE
        REWRITE(MCxDBS,IOSTAT=IOS) KEY(:MCxKEY), IW, IDATE, ITIME,
     >           ( IWORK(K),K=IBGN,IEND)
          IF( IOS .EQ. 158 ) THEN
            WRITE(MCxDBS,ERR=380) KEY(:MCxKEY), IW, IDATE, ITIME,
     >           ( IWORK(K),K=IBGN,IEND)
          ELSEIF( IOS .NE. 0 ) THEN
            GO TO 380
          ENDIF
        ENDIF
        IBGN = IEND + 1
4000  CONTINUE
      GO TO 800
 
C*  If NWORDS gt MXWORK, output data without using SSWORK
 
500   CONTINUE
      NPRINT = NPRINT + 1
      IF( NPRINT .LE. MXPRNT ) THEN
         WRITE(MESAGE(1),9501)
 9501    FORMAT(' Size of /SSWORK/ is too small')
         WRITE(MESAGE(2),9502) NWORDS
 9502    FORMAT(' Required size is ',I10)
         CALL TBxERR( 200,'TBWRIT', 2)
      ENDIF
 
      IBLK   = NBLK
      IFREC  = 1
      ILREC  = 1
C
C Start loop to output data.
C
510   CONTINUE
      IF( IBLK .LE. 0 ) GO TO 950
      IOUT   = 0
520   CONTINUE
      IOUT   = IOUT + NFINAL(ILREC) - NBEGIN(ILREC) + 1
      IF(IOUT .LT. LNxBLK ) THEN
        ILREC = ILREC + 1
        IF( ILREC .LE. NREC ) GO TO 520
      ENDIF
530   CONTINUE
      ISLSTB = NBEGIN(ILREC)
      ISLSTF = NFINAL(ILREC)
      IDIF   = 0
      IF( IOUT .GT. LNxBLK ) IDIF = IOUT - LNxBLK
      IOUT   = IOUT - IDIF
      NFINAL(ILREC) = NFINAL(ILREC) - IDIF
      IW     = IOUT + 2
 
      KEY(MCxKEY:MCxKEY) = DATKEY(IBLK)
      IF(IEXIST.EQ.0) THEN
        WRITE(MCxDBS,ERR=380) KEY(:MCxKEY), IW, IDATE, ITIME,
     >      ((IBUFF(K),K=NBEGIN(L),NFINAL(L)),L=IFREC,ILREC)
      ELSE
        REWRITE(MCxDBS,IOSTAT=IOS) KEY(:MCxKEY), IW, IDATE, ITIME,
     >      ((IBUFF(K),K=NBEGIN(L),NFINAL(L)),L=IFREC,ILREC)
        IF( IOS .EQ. 158 ) THEN
          WRITE(MCxDBS,ERR=380) KEY(:MCxKEY), IW, IDATE, ITIME,
     >      ((IBUFF(K),K=NBEGIN(L),NFINAL(L)),L=IFREC,ILREC)
        ELSEIF( IOS .NE. 0 ) THEN
          GO TO 380
        ENDIF
      ENDIF
      IF( IDIF .NE. 0 ) THEN
        NBEGIN(ILREC) = NFINAL(ILREC) + 1
        NFINAL(ILREC) = ISLSTF
        IFREC         = ILREC
      ELSE
        IFREC         = ILREC + 1
      ENDIF
      ILREC = IFREC
      IBLK  = IBLK - 1
      IF( IFREC .LE. NREC ) GO TO 510
 
C ... Put time stamp.
800   CONTINUE
      CALL TBCxTS
 
      RETURN
 
380   CONTINUE
      MESAGE(1) = 'Failed to write data block to the data base.'
      MESAGE(2) = 'KEY ='//KEY(:MCxKEY)
      CALL TBxERR(800, 'TBCxWT', 2)
 
950   CONTINUE
      MESAGE(1) = 'NBLK is less than 0, before output all records.'
      MESAGE(2) = 'KEY ='//KEY(:MCxKEY)
      CALL TBxERR(800, 'TBCxWT', 2)

#endif
      RETURN
      END
