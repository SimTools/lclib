#ifdef HIUXF77
	integer*4 function iargc(nret)
	character*100 tmp
	narg = 0
	do i = 1, 80
	  call getarg(i, tmp)
	  ltmp = lnblnk(tmp)
	  if( ltmp .le.0 ) go to 200
	  narg = narg + 1
	end do
200	continue
	iargc = narg - 1
	return
	end
c
	subroutine system(string)
	character*(*) string
	call hf_sh(string, nret)
	return
	end
c
c	integer*4 function fnum(iunit)
c	print *,'Function FNUM is not supported yet for HI-UX F77.'
c	stop
c	end
#endif
C*---------------------------------------------------------------*
C*
C* ------------------
C*  List information of TBS data.
C* ------------------
C*
C*(Function)
C*    This program is to list contents of TBS data.
C*
C*(Author)
C*    A. Miyamoto    Jan-30-1986  Original version.
C*
C*---------------------------------------------------------------*
C
C
      EXTERNAL TBVOPN !$pragma C ( TBVOPN )

      PARAMETER    ( NSIZE = 400000 )
      PARAMETER      (IWKSIZ = 8000 )
      COMMON /SSWORK/ IWKBUF(IWKSIZ)
      CHARACTER*64 RECNAM,BNKNAM
      PARAMETER   (MXxBNK=200)
      CHARACTER*64 TMPBNK(MXxBNK)
CC      CHARACTER*44 INDSN,LSTDSN,TBSDSN
      CHARACTER*144 INDSN,LSTDSN,TBSDSN
      CHARACTER*10 OPTCOD
      CHARACTER*80 RECOPT, BLANK
      INTEGER*4    ISORT(MXxBNK), IRSORT(MXxBNK)
      CHARACTER*4096 WORKS
C
C=EXPAND 'T#SM.TBS.FORT/TBSFMT.inc'
#include "tbsfmt.inc"
C
      DATA      BLANK/' '/
C
      character*80 cmd
C
C---------------------- Parameter for the banks.
C
      COMMON /TBLOCK/ NUMDSB, IDDSB(100)
      CHARACTER*64    DSBNAM(100)
      COMMON /TBLOCC/ DSBNAM
C
      COMMON /CHKCTL/  IDSB
C
      EXTERNAL   TBLSUB, TBLBNM, TBLLNK, XBLIST, XBLSPC

      character*80 command
      integer*4    ipil(3,100,10)
C
C=========< Entry Point >===============================================
C
C (1) Initialize valiable and
C
      NRWFMT(1)=0
      NRWFMT(2)=0
      NRWFMT(3)=0
      NRWFMT(4)=0

C
C 1.1  Get parameter
C
      noarg = iargc()
      if(noarg.lt.1) then
         print *,'Command usage is '
         print *,'   tbschk [-vbs] file_name '
         print *,' -vbs to read FACOM VBS format data.'
         stop
      elseif( noarg.eq.1 ) then
#ifdef  HP
        iflen = igetarg(1, tbsdsn, 144)
        open(1,file=tbsdsn,IOSTAT=inst,readonly,form='unformatted')
        print *,' tbsdsn =',tbsdsn
#endif
#ifdef SUN
        CALL getarg(1, tbsdsn)
        print *,' tbsdsn =',tbsdsn
        open(1,file=tbsdsn,IOSTAT=inst,form='unformatted')
#endif
#ifdef HIUXF77
        CALL getarg(2, tbsdsn)
        print *,' tbsdsn =',tbsdsn
        open(1,file=tbsdsn,IOSTAT=inst,form='unformatted')
#endif
#ifdef MACHTEN_PPC
        CALL getarg(1, tbsdsn)
        print *,' tbsdsn =',tbsdsn
        open(1,file=tbsdsn,IOSTAT=inst,form='unformatted')
#endif

         if(inst.ne.0) then
            print *,'Failed to open ',tbsdsn
            stop
         endif
      else
         do 20 i = 1, noarg
#ifdef SUN
            call getarg(i, command)
#endif
#ifdef HIUXF77
            call getarg(i+1, command)
#endif
#ifdef MACHTEN_PPC
            call getarg(i, command)
#endif
#ifdef HP
            iflen=igetarg(i, command,80)
#endif
            if( command(1:4).eq.'-vbs' ) then
               NRWFMT(1) = 1
            else
             
               tbsdsn=command
               IF( NRWFMT(1).EQ.0 ) THEN
#if defined(SUN) || defined(MACHTEN_PPC)
       open(1,file=tbsdsn,IOSTAT=inst,form='unformatted')
#endif
#ifdef HIUXF77
       open(1,file=tbsdsn,IOSTAT=inst,form='unformatted')
#endif
#ifdef HP
       open(1,file=tbsdsn,IOSTAT=inst,readonly,form='unformatted')
#endif
               ELSE
                  CALL TBVOPN(tbsdsn(:LNBLNK(tbsdsn)))
               endif

            endif
 20      continue
      endif

      if( NRWFMT(1).NE. 0 ) CALL TBUXIN(3, 0)

      RECOPT=' '
C
      IF(RECOPT.NE.BLANK) THEN
        IF(RECOPT(1:1).NE.'*') THEN
          NUMDSB         = NUMDSB + 1
          IDDSB(NUMDSB)  = NUMDSB
          DSBNAM(NUMDSB) = RECOPT
        ELSE
          PRINT *,'Enter record name to be TBALOCed.'
          PRINT *,'Enter X to End input.'
150       CONTINUE
c          CALL PROMPT(2,'Record name?>',IRT)
          write(6,*) ' Record name ?'
          READ(5,'(A64)',END=160) TMPBNK(1)
          IF(TMPBNK(1)(1:3).EQ.'X  ') GO TO 160
          NUMDSB         = NUMDSB + 1
          IDDSB(NUMDSB)  = NUMDSB
          DSBNAM(NUMDSB) = TMPBNK(1)
          GO TO 150
160       CONTINUE
        ENDIF
      ENDIF
C
C
      NREC = 0
      LSTDSN = 'tmp.tbschk.list'
      CALL TBTBSI(IRET)
      IF(IRET.LT.0) THEN
        PRINT *,'Error to initialize TBS'
        STOP
      ENDIF


      call tinit()
      call tscreensz(NLINSW, NCOLSW)
      if( NLINSW .LT.15 .OR. NCOLSW .LT.80 ) THEN
          call tend(0)
          print *,'Window sie is too small.'
          stop
      endif
      NLINE1 = 4
      NLINEL = NLINSW - 4  

190   CONTINUE
      call tclear()
      call tmove(0,0)
      call tputstr('TBSCHK : File ')
      call tmove(0,15)
      call tputstr(tbsdsn(:lnblnk(tbsdsn)))
C
C (2) Read data.
C
      INDSN = TBSDSN
C
      ICONS = 0
      CALL TBREAD(1, IDSB)
      NREC = NREC + 1

      IF(IDSB.LT.0) GO TO 900
      RECNAM = DSBNAM(IDSB)
      OPTCOD = ' '

      CALL TBQBNK( IDSB, IWKBUF, NOBNK, TMPBNK )
      CALL USORTC( NOBNK, 1, 64, TMPBNK, ISORT)
      
      npg  = 1
      call uvzero(3*100*10,ipil)
      
      DO 200 I = 1, NOBNK
        IP = ISORT(I)
        IRSORT(I)=IP
	BNKNAM = TMPBNK(IP)
        CALL TBNOEL(IDSB, BNKNAM, NOELM, IWKBUF)
	npg = (i-1)/(nlinel-4) + 1
	ipscrn = mod(i-1,nlinel-4) + 1
	ipil(1,ipscrn,npg) = ip
	ipil(2,ipscrn,npg) = noelm
 200  CONTINUE
      ipg    = 1
      ipscrn = 1
C
 180  CONTINUE
      write(works,'(''NREC='',I4,'' IDSB='',I3)') NREC, IDSB
      lw = lnblnk(works)
      works(lw+1:) = '  Record name='//RECNAM
      call tmove(1, 0)
      call tputstr(works(:lnblnk(works)))
      call tmove(2, 0)
      works = 'Elm#  Bank-name'
      call tputstr(works(:lnblnk(works)))
      call tmove(3, 0)
      works = '----+-------------------------------------------------'
      call tputstr(works(:lnblnk(works)))

C
C (3) Updata table.
C
      idbnk = ipil(1,ipscrn,ipg)
      
      do 300 i = 1, nlinel-4
	ip    = ipil(1,i,ipg)
	if ( ip.le.0 )				go to 310
        noelm = ipil(2,i,ipg)
        il    = i + 3
        call tmove(il, 0)
        write(works,'(i4)') noelm
        call tputstr(works(:6))
        if ( i.eq.ipscrn ) then
          call tattrreverse()
          call tputstr(tmpbnk(ip)(:lnblnk(tmpbnk(ip))))
          call tattrnormal()
        else
          call tputstr(tmpbnk(ip)(:lnblnk(tmpbnk(ip))))
        endif
300   continue
310   continue

      CALL TMOVE(NLINEL,0)
      CALL TPUTSTR('You can use any of the following commands:  ')
      CALL TPUTSTR('L)list element data, N)next record, ')
      CALL TPUTSTR('B)previous record, S)skip n records, Q)quit')
C
C (4) Display Table.
C
400   CONTINUE
      call trefresh()
      OPTCOD = ' '
      call tgetcode(icode,OPTCOD(1:1))
      call ucuper(1, OPTCOD)
290   continue
C
C (5) Switch according to the Option code.
C
C     ------------------------
      if ( icode .eq. 3 ) then
C     ------------------------
C
	  call tmove(ipscrn+3,6)
	  call tattrnormal()
	  idbnk = ipil(1,ipscrn,ipg)
	  call tputstr(tmpbnk(idbnk)(:lnblnk(tmpbnk(idbnk))))
	  if ( ipscrn.gt.1 ) then
	     ipscrn = ipscrn - 1
	  else if ( ipg.gt.1 .and. ipscrn.eq.1 ) then
	     ipg    = ipg -1
	     ipscrn = nlinel - 4
	     go to 780
	  endif
	  call tattrreverse()
	  call tmove(ipscrn+3,6)
	  idbnk = ipil(1,ipscrn,ipg)
	  call tputstr(tmpbnk(idbnk)(:lnblnk(tmpbnk(idbnk))))
	  call tattrnormal()
          go to 180
      elseif( icode .eq. 2 ) then
	  if ( ((ipg-1)*(nlinel-4)+ipscrn).lt.nobnk ) then
	     call tmove(ipscrn+3,6)
	     call tattrnormal()
	     idbnk = ipil(1,ipscrn,ipg)
	     call tputstr(tmpbnk(idbnk)(:lnblnk(tmpbnk(idbnk))))
	     if ( ipscrn.lt.(nlinel-4) ) then
     	        ipscrn = ipscrn + 1
     	     else
     	        ipg    = ipg + 1
     	        ipscrn = 1
     	        go to 780
     	     endif
	     call tattrreverse()
	     call tmove(ipscrn+3,6)
	     idbnk = ipil(1,ipscrn,ipg)
	     call tputstr(tmpbnk(idbnk)(:lnblnk(tmpbnk(idbnk))))
	     call tattrnormal()
     	  endif	  
          go to 180
      elseif ( icode .eq.0 .and.OPTCOD(1:1).EQ.'Q') THEN   
	  call tend()
	  go to 900
      elseif( icode.eq.0.and.OPTCOD(1:1).EQ.'N') THEN
          GO TO 190
          
      ELSEIF(ICODE.EQ.0.AND.OPTCOD(1:1).EQ.'B') THEN
		if ( nrec.gt.1 ) then
#ifdef MACHTEN_PPC
		   close(1)
                   open(1,file=tbsdsn,IOSTAT=inst,form='unformatted')
#else
		   rewind 1
#endif
		   iskip = nrec - 1
		   nrec  = 0
		   if ( iskip.gt.1 ) then
		      do 553 i = 1, iskip-1
		         call tbread(1,idsb)
		         nrec = nrec + 1
 553		      continue
 		   endif
		   go to 190
		else
		   go to 180
		endif

      ELSEIF(ICODE.EQ.0.AND.OPTCOD(1:1).EQ.'S') THEN

		works = 'No. records to skip ? '
		call tscrollok()
		call tmove(nlinel+3,0)
          	call tputstr(works(:lnblnk(works)))
		call tmove(nlinel+3,lnblnk(works)+1)
		
		call tgetstr(optcod)
		optcod = optcod(:(lnblnk(optcod)-1))
#ifdef MACHTEN_PPC
		read(optcod(:lnblnk(optcod)),'(I8)') iskip
#else
		read(optcod(:lnblnk(optcod)),'(I)') iskip
#endif
		if ( iskip.lt.0 ) then
#ifdef MACHTEN_PPC
		   close(1)
                   open(1,file=tbsdsn,IOSTAT=inst,form='unformatted')
#else
		   rewind 1
#endif
		   iskip = max( nrec + iskip, 1 )
		   nrec  = 0
		endif
		if ( iskip.gt.1 ) then
		   do 555 i = 1, iskip-1
		      call tbread(1,idsb)
		      if ( idsb.lt.0 ) 		go to 800
		      nrec = nrec + 1
 555		   continue
 		endif      
		call tscrollnotok()
		go to 190
C
C  Display Element Data.
C
C
C  List element.
C
      ELSE IF ( OPTCOD(1:1).EQ.'L' ) THEN
         idbnk = ipil(1,ipscrn,ipg)
         BNKNAM = TMPBNK(IDBNK)
         CALL TBNOEL(IDSB,BNKNAM,IELM,IWKBUF)

		works = 'Following elements exist in '//BNKNAM
		call tmove(nlinel+2,0)
          	call tputstr(works(:lnblnk(works)))

		write(works,*) (IWKBUF(K),K=1,IELM)
		call tmove(nlinel+3,0)
		call tscrollok()
          	call tputstr(works(:lnblnk(works)))

		works = 'Element number ? '
		call tmove(nlinel+3,0)
		call tscroll()
          	call tputstr(works(:lnblnk(works)))
		call tmove(nlinel+3,lnblnk(works)+1)
		
		call tgetstr(optcod)
		optcod = optcod(:(lnblnk(optcod)-1))
#ifdef MACHTEN_PPC
		read(optcod(:lnblnk(optcod)),'(I8)') ielno
#else
		read(optcod(:lnblnk(optcod)),'(I)') ielno
#endif
		call tscrollnotok()
        
	open(77,file=LSTDSN,form='formatted')
        CALL TBLUNI(77)
        CALL TBLEL0(IDSB,BNKNAM,IELNO,0)
        CLOSE(77)
        CALL TBLUNI(6)

#ifndef HIUXF77
         CALL GETENV('TBSCHK_EDITOR', WORKS)
         IF( WORKS(1:1).EQ.' ') CALL GETENV('EDITOR',WORKS)
#else
         CALL GETENV('TBSCHK_EDITOR',13, WORKS,160)
         IF( WORKS(1:1).EQ.' ') CALL GETENV('EDITOR',6,WORKS,160)
#endif
         IF( WORKS(1:1).EQ.' ') WORKS='kterm -e less'

        cmd = WORKS(:LNBLNK(WORKS))//' '//LSTDSN
        call system(cmd)
        cmd = 'rm '//LSTDSN
        call system(cmd)
C
C     -----
      ENDIF
C     ----- 
C      
780   call tclear()
      call tmove(0,0)
      call tputstr('TBSCHK : File ')
      call tmove(0,15)
      call tputstr(tbsdsn(:lnblnk(tbsdsn)))
      go to 180
C
800   CONTINUE
      PRINT *,'Read End-of-File.'
      GO TO 310
C
900   CONTINUE
      IF(ICONS.EQ.1) GO TO 190
      close(1)
      STOP
      END
C
C
CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine TBTBSI(NRET)                                             *
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Initialize TBS, Bank System.                                  *
C*  (Relation)                                                         *
C*       Called by                                                     *
C*           PRJOBI : Production job initialization.                   *
C*       Call the following subroutines                                *
C*           TBSIZE : TBS Initialize.                                  *
C*           TBALOC : Allocate DSB.                                    *
C*  (Author)                                                           *
C*       A. Miyamoto    26-Aug-1985                                    *
C*       A. Miyamoto    29-Jan-1986  Use TBSHAR option.                *
C*                                   All DSB is shared, Except ENVMONcn*
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE TBTBSI(NRET)
C
C---------------------- Declair Buffer for TBS.
C
      PARAMETER    ( NSIZE = 400000 )
      COMMON /TBSBUF/ IBUFF( NSIZE )
      COMMON /SSWORK/ KBUFF( 50000 )
C
C---------------------- Parameter for the banks.
C
      COMMON /TBLOCK/ NUMDSB, IDDSB(100)
      CHARACTER*64    DSBNAM(100)
      COMMON /TBLOCC/ DSBNAM
      CHARACTER*80   MESAGE
C
C==================<<  Entry Point  >>================================
C
C
C---------------------- TBS Initialize.
C
      CALL TBSIZEx(NSIZE, 50000)
      CALL TBWARN( 11 )
      NRET = 0
C
C---------------------- Allocate DSB.
C
      CALL TBALOC('Raw_Data_Buffer', 100, 80, 2500, 300000, IRET)
      IF(IRET.LT.0) THEN
         WRITE(MESAGE,'(''Error to TBALOC "Raw_Data_Buffe"...IRET='',
     >                    I5,)') IRET
         GO TO 9000
      ENDIF
C
C   Allocate Shared DSB.
C
      DO 100 I = 1, NUMDSB
         CALL TBSHAR(100, DSBNAM(I), IDDSB(I), IRET)
         IF(IRET.LT.0) THEN
           WRITE(MESAGE,'(''Error to TBSHAR'',A,'' .. IRET='',I5)')
     >                      DSBNAM(I), IRET
           GO TO 9000
         ENDIF
100   CONTINUE
      RETURN
C
C (9) Error stop.
C
9000  CONTINUE
      PRINT *,' Error in TBTBSI '
      PRINT *,MESAGE
      NRET = -9999
      RETURN
      END
C
CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* BLOCK DATA
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Block data to define records to allcate.                      *
C*                                   All DSB is shared, Except ENVMONcn*
C*                                                                     *
CC**********************************************************************
C
C
      BLOCK DATA
C
c==EXPAND 'T#PR.P#STER.FORT(@PRFELD)'
C
      COMMON /TBLOCK/ NUMDSB, IDDSB(100)
      CHARACTER*64    DSBNAM(100)
      COMMON /TBLOCC/ DSBNAM
C     
      DATA  (IDDSB(I),DSBNAM(I),I=1,18)/
C      DATA  (IDDSB(I),DSBNAM(I),I=1,8)/
     >   1, 'Event',
     >   2, 'Begin_Run',
     >   3, 'System_Configuration',
     >   4, 'Environment_Monitor',
     >   5, 'Environment_Monitor_Constant',
     >   6, 'Luminosity_Monitor',
     >   7, 'Status_Information',
     >   8, 'End_Run'          ,
C
     >   9, 'Reduction:IDC_Constants',
     >  10, 'Reduction:TPC_Constants',
     >  11, 'Reduction:BTOF_Constants',
     >  12, 'Reduction:BDC_Constants',
     >  13, 'Reduction:BCL_Constants',
     >  14, 'Reduction:BMU_Constants',
     >  15, 'Reduction:EDC_Constants',
     >  16, 'Reduction:ECL_Constants',
     >  17, 'Reduction:LUM_Constants',
     >  18, 'Reduction:Steer_Constants' /
C
      DATA  (IDDSB(I),DSBNAM(I),I=19,30)/
     >  19, 'Production:IDC_Constants',
     >  20, 'Production:TPC_Constants',
     >  21, 'Production:BTOF_Constants',
     >  22, 'Production:BDC_Constants',
     >  23, 'Production:BCL_Constants',
     >  24, 'Production:BMU_Constants',
     >  25, 'Production:EDC_Constants',
     >  26, 'Production:ECL_Constants',
     >  27, 'Production:LUM_Constants',
     >  28, 'Production:Steer_Constants',
     >  29, 'BCL;Frozen_Shower',
     >  30, 'Format_Error'/
C
      DATA  (IDDSB(I),DSBNAM(I),I=31,42)/
     >  31, 'Production:VTX_Constants',
     >  32, 'Production:TCH_Constants',
     >  33, 'Production:FCL_Constants',
     >  34, 'Production:RCL_Constants',
     >  35, 'Reduction:VTX_Constants',
     >  36, 'Reduction:TCH_Constants',
     >  37, 'Reduction:FCL_Constants',
     >  38, 'Reduction:RCL_Constants',
     >  39, 'Simulator:Steer_Constants',
     >  40, 'Simulator:VTX_Constants',
     >  41, 'Simulator:TCH_Constants',
     >  42, 'Reduction:FLT_Constants'/
C
      DATA   NUMDSB/42/
C      DATA   NUMDSB/8/
C
      END
CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine TBEXEC(SUBNAM, DSN)                                      *
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Execute TBS list routines.                                    *
C*  (Input)                                                            *
C*       SUBNAM  : Subroutine name executed.                           *
C*       DSN     : Specify data set name to output on disk file.       *
C*                 '*' for terminal.                                   *
C*  (Author)                                                           *
C*       A. Miyamoto    17-Apr-1987  Original version.                 *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE TBEXEC(SUBNAM, DSN)
C
      CHARACTER*(*) DSN
      EXTERNAL      SUBNAM
C
C---------------------- Parameter for the banks.
C
      IF(DSN(1:1).EQ.'*') THEN
          CALL SUBNAM
      ELSE
c        CALL UALCPS(77, DSN, 'RENEW','VBA', 50, IRET)
         open(77,file=DSN,form='formatted')
        CALL TBLUNI(77)
        CALL SUBNAM
        CLOSE(77)
c        CALL PFDLNK('EDIT', DSN(6:))
        CALL TBLUNI(6)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE XBLSPC
      CALL TBLSPC(-1, ' ')
      RETURN
      END
C
      SUBROUTINE XBLIST
      COMMON /CHKCTL/  IDSB
      CALL TBLIST( IDSB )

      RETURN

      END

c
      subroutine tputstr(string)
      character*(*) string
      character char

      lstr = len(string)
      do 100 i = 1, lstr
         char = string(i:i)
         call tputc(char)
 100  continue
      return
      end
