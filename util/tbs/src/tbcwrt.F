C *********************************************************************C
C*                                                                     *
C*===================================                                  *
C* Subroutine TBCWRT(ID, IVERS, IRET, NDATIM )                         *
C*===================================                                  *
C*(Purpose)                                                            *
C*   Write Data of record :ID to the data base file.                   *
C*(Input)                                                              *
C*   ID       : ID_Number of the record.                               *
C*   IVERS    : Version number.                                        *
C*          = 0 to save as latest version data.                        *
C*          > 0 to save as given version number data.                  *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*        > 0 : Actual version number.                                 *
C*        =-1 : Record name not exist.                                 *
C*        =-2 : Record name is too long.                               *
C*        =-3 : Write error.                                           *
C*   NDATIM(2) : Date and Time of record.                              *
C*(Used COMMON)                                                        *
#include "ctlexp.inc"
#include "bnkexp.inc"
#include "bufexp.inc"
C*(Author)                                                             *
C*   A. Miyamoto  Mar. 18 '87  Modified from TBWRIT.                   *
C*                                                                     *
C **********************************************************************
 
#ifdef  MSP
      SUBROUTINE TBCWRT( ID, IVERS, IRET, /NDATIM/ )
#endif
#ifdef  UNIX
      SUBROUTINE TBCWRT( ID, IVERS, IRET )
#endif

      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsctl.inc"
#include "tbsmes.inc"
#include "sswork.inc"
#include "tbssum.inc"
#include "tbsiob.inc"
#include "tbcctl.inc"
#include "tbcdat.inc"
 
      CHARACTER*64 TNAME
      INTEGER*4    NDATIM(2)

#ifdef UNIX
	character*256 dbsfile,systemcmd
	character*40  tempfile
	logical incstat
	integer getpid
#endif

C
#ifdef  UNIX
      NUMARG = 3
      GO TO 9900
      ENTRY TCWRTx(ID, IVERS, IRET, NDATIM )
      NUMARG = 4
9900  CONTINUE
#endif
#ifdef  MSP
       NUMARG = NARG(0)
#endif

C  -------------------------------------------------------------------
C                 Test whether Record ID exists in any DSB.
C  -------------------------------------------------------------------
CTIME                               CALL CLOCK( TIM0, 0, 1 )
C
         IF( NUMARG  .GE. 4 ) THEN
             NCDATE = NDATIM(1)
             NCTIME = NDATIM(2)
         ELSE
            CALL UIDATE( NCDATE )
#ifndef HIUXF77
            CALL   TIME( NCTIME )
#else
            call clock(2, nctime)
#endif
         ENDIF
C                          =====
         CALL TBxFRD( ID,  IPSUB )
 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8000) TIME
CTIME 8000                   FORMAT(1X,F10.3,' sec to find Record ID.')
 
CTIME                               CALL CLOCK( TIM0, 0, 1 )
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBWRIT', 1)
 
           IRET     = -1
           RETURN
         ENDIF
 
         IASH   = IDSUB(11,IPSUB)
         IF( IASH .GT. 0 ) THEN
             ISSH  = IDSUB(10,IPSUB)
             TNAME = SHRNAM( IASH, ISSH)
         ELSE
             TNAME = SUBNAM(IPSUB)
         ENDIF
 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8010) TIME
CTIME 8010               FORMAT(1X,F10.3,' sec to find Record name.')
 
CTIME                               CALL CLOCK( TIM0, 0, 1 )
C
C  -------------------------------------------------------------------
C     Confirm that the length of the record name is within Valid
C    key length.
C  -------------------------------------------------------------------
C
         DO 200 I  = 64, 1, -1
           IF(TNAME(I:I).NE.' ') GO TO 210
200      CONTINUE
210      CONTINUE
         IF(I.GT.MCxKYL) THEN
           MESAGE(1) = 'Record name '//TNAME(:I)
           MESAGE(2) = 'is longer than valid key length of the data '
     >               //'base file.'
           CALL TBxERR(800, 'TBCWRT', 2)
           STOP
         ENDIF
C
C In case of UNIX system, output data using TBWRIT
#ifdef UNIX
	inquire(file=cmxdbs(:lnblnk(cmxdbs)),exist=incstat)
	if( .not.incstat ) then
          mesage(1)='Directory for constants data base does not exists.'
	  mesage(2)='Directory was'
	  mesage(3)=cmxdbs(:lnblnk(cmxdbs))
           CALL TBxERR(800, 'TBCWRT', 3)
	endif
	dbsfile=cmxdbs(:lnblnk(cmxdbs))//'/'//tname(:mcxkyl)
	inquire(file=dbsfile,exist=incstat)
	if( .not.incstat ) then
	  systemcmd='mkdir '//dbsfile(:lnblnk(dbsfile))
	  call system(systemcmd)
	  mesage(1)='DBS file '//tname(:mcxkyl)
	  mesage(2)='was created in the directory'
	  mesage(3)=cmxdbs(:lnblnk(cmxdbs))
          CALL TBxERR(100, 'TBCWRT', 3)
	  nvers = 1
	else
#ifdef HIUXF77
           call clock(2,itim)
#else
	  itim = getpid()
#endif
	  write(tempfile,4100) itim
4100      format('/tmp/tbs.ls.',i8.8)
	  write(systemcmd,4110) dbsfile(:lnblnk(dbsfile)),tempfile
4110      format('cd ',a,' && ls 0* | tail -1 > ',a)
	  call  system(systemcmd)
	  open(mcxdbs,file=tempfile)
	  read(mcxdbs,*) nvers
	  nvers = nvers+1
	  close(mcxdbs,status='DELETE')
	endif
	if( ivers.eq.0 ) ivers = nvers
	write(tempfile,4120) ivers
4120    format(i5.5)
	dbsfile=dbsfile(:lnblnk(dbsfile))//'/'//tempfile
	open(mcxdbs,file=dbsfile,form='Unformatted',
     >	status='new',iostat=ios)
	if(ios.ne.0) then
	  mesage(1) = 'Unable to open DBS file, File name is'
	  mesage(2) = dbsfile
	  call tbxerr(800, 'TBCWRT', 2)
	endif
	call tbwrit(mcxdbs, id, iret )
	close(mcxdbs)
	iret = nvers
	return
#endif
#ifdef MSP

C
C  -------------------------------------------------------------------
C     Get General information of the Control_Block from the COMMON
C    /TBSBNK/.
C  -------------------------------------------------------------------
C  ---------------- Get General Information --------------------------
C
          ICNTL( 3) = NWSUB(2,IPSUB)
          NWS       = 16
C                                  =======
          CALL TBxINT( TNAME, NWS, ICNTL(4))
C
          IBOFT     = IDSUB(2,IPSUB)
          MXBANK    = IDSUB(3,IPSUB)
          NOBANK    = IDSUB(4,IPSUB)
          IEOFT     = IDSUB(5,IPSUB)
          MXELMT    = IDSUB(6,IPSUB)
          NOELMT    = IDSUB(7,IPSUB)
          HCNTL(39) = NOBANK
          HCNTL(40) = MXBANK
          HCNTL(41) = NOELMT
          HCNTL(42) = MXELMT
C
C  -------- Get Offsets of Sort,Bank_Name,Bank_Key,Link ------------
C  --------    Element_Key_Table and Address_Table      ------------
C  -------- and pack Bank_names                         ------------
 
          IOFSRT    = 50
          IOFNAM    = (IOFSRT + NOBANK + 1)/2
          IOFBKN    = IOFNAM
          IF( NOBANK .GT. 0 ) THEN
             DO  100     NB  = 1, NOBANK
               IPBNK         = NB + IBOFT
               CALL TBxLEN( BNKNAM(IPBNK), NWS )
               IOFBKN        = IOFBKN + 1
               ICNTL(IOFBKN) = NWS
               IPNT          = IOFBKN + 1
C                                              ============
               CALL TBxINT( BNKNAM(IPBNK), NWS, ICNTL(IPNT))
               IOFBKN        = IOFBKN + NWS
  100        CONTINUE
          ENDIF
          IOFNWB    = IOFBKN
          IOFKEY    = 2*(IOFBKN + NOBANK*2)
          IOFLNK    = IOFKEY + 2*NOBANK
          IOFELM    = (IOFLNK + 2*NOELMT)/2
          LINKAD    = 1
          IOFADT    = IOFELM + 2*NOELMT
C
          HCNTL(43) = IOFSRT
          HCNTL(44) = IOFNAM
          HCNTL(45) = IOFNWB
          HCNTL(46) = IOFKEY
          HCNTL(47) = IOFLNK
          HCNTL(48) = IOFELM
          HCNTL(49) = IOFADT
C
C  -------------------------------------------------------------------
C     Make the Control_Block and the output ordering table NBEGIN(i)
C    and NFINAL(i).
C    Do the following steps for NOBANK times.
C  -------------------------------------------------------------------
C
         NWORDS    = 0
         NREC     = 0
C
C  --- Copy Sort_Table to the Control_Block at a time. (only once) --
C
         IF( NOBANK .GT. 0 ) THEN
           DO  500     NB  = 1, NOBANK
             IOFSRT        = IOFSRT + 1
             HCNTL(IOFSRT) = ISORT( IBOFT+NB ) - IBOFT
  500      CONTINUE
 
C             ******
           DO  1000    NB  = 1, NOBANK
C             ******
 
                IPBNK         = NB + IBOFT
 
C  --- Copy Bank_Key to the Control_Block  --------------------------
 
                IOFNWB        = IOFNWB + 1
                ICNTL(IOFNWB) = NWBNK(1,IPBNK)
                IOFNWB        = IOFNWB + 1
                ICNTL(IOFNWB) = NWBNK(2,IPBNK)
C
                IPNT          = KEYBNK(1,IPBNK)
C
                IOFKEY        = IOFKEY + 2
                NELMNT        = KEYBNK(2,IPBNK)
                HCNTL(IOFKEY) = NELMNT
C                                                             /// DEBUG
C                                 WRITE(6,9000) NB,NELMNT,BNKNAM(IPBNK)
C9000                     FORMAT(1X,'==== Loop for No. of Banks =====',
C    -                      /1X,'TBWRIT NB,NELMNT,BNKNAM =',2I6,2X,A32)
C
                IF( NELMNT .LE. 0 ) THEN
                    HCNTL(IOFKEY-1) = -1
C
                ELSE
                    HCNTL(IOFKEY-1) = LINKAD
C
C   ------- Copy Link_Table, Address_Table and Ordering Data.  -------
C   -------    Do the following steps for NELMNT times.        -------
C
                    DO    900   NE  = 1, NELMNT
C
C                                  Copy Link_Table to the Control_Block
C
                       IOFLNK        = IOFLNK + 1
                       LINKAD        = LINKAD + 1
                       HCNTL(IOFLNK) = LINK(1,IPNT)
                       NEXT          = LINK(2,IPNT)
C
                       IOFLNK        = IOFLNK + 1
                     IF( NE .EQ. NELMNT ) THEN
                       HCNTL(IOFLNK)= -1
                     ELSE
                       HCNTL(IOFLNK)= LINKAD
                     ENDIF
C
C                        Get Address and Parameters of the Bank_element.
C
                       HPDATA       = IADTBL(1,IPNT) + NWSUB(1,IPSUB)
                       HWDSB        = NWELM(2,IPNT)
                       HPEND        = HPDATA + HWDSB - 1
C
                       NREC         = NREC + 1
                       NBEGIN(NREC) = (HPDATA+1)/2
                       NFINAL(NREC) = (HPEND+1)/2
C
C                      Change Address of the Bank_Element and store it.
C
                       IOFELM       = IOFELM + 1
                       ICNTL(IOFELM)= 2*( (HWDSB+1)/2 )
                       IOFELM       = IOFELM + 1
                       ICNTL(IOFELM)= HWDSB
                       IOFADT       = IOFADT + 1
                       ICNTL(IOFADT)= 2*NWORDS + 1
C
C*    4. Preparation for the next Bank_Element.
C
                       IPNT         = NEXT
                       NWORDS       = NWORDS + (HWDSB+1)/2
C
  900             CONTINUE
                ENDIF
C*****
 1000      CONTINUE
C*****
         ENDIF
C
C  ------------------------------------------------------------------
C            Write the Control_Block on the Unit LUN.
C  ------------------------------------------------------------------
C
         ICNTL(1)         = NWORDS*2
         ICNTL(2)         = 403
         NWCNTL           = IOFADT
 
CC       PRINT *,' In TBCWRT..NWCNTL, NWORDS=',NWCNTL, NWORDS
 
         CALL TBCxWT(TNAME, IVERS, NWORDS, IRET)
 
 
         NTWRIT        = NTWRIT + 1
         NOWRIT(IPSUB) = NOWRIT(IPSUB) + 1
 
CCC      IRET    = NWCNTL + NWORDS
         RETURN
#endif 
         END
