c***********************************************************************
c
c      subroutine prgtif(type, nexp, nfil, vol, npos, fname)
c
c (Function)
c     get tape file information from the data base.
c (Input)
c     type(c*8) : file type, beamcosm, random, etc.
c     nexp : exp# 
c     nfil : file number.
c (Output)
c     vol(c*6) : tape volume name.
c     npos     : file position.
c       = -1 when tape information is not available.
c     fname(c*24) : tape file name.
c (Author)
c     A. Miyamoto  7-Apr-1995  Original version.
c
c***********************************************************************

      subroutine prgtif(type, nexp, nfil, vol, npos, fname, hostname)	
      character*(*) type
      character*8  rtype, hostname
      character*6 vol
      character*24 fname
      integer nexp, nfil, npos

      character*248 dbsname
      character*240 dbspath
      data dbspath
     > /'/home/tophp1/topazlib/dbs/tapeindex/index'/
      character*240 tapemap
      data tapemap
     >  /'/home/tophp1/topazlib/dbs/tapeindex/control/tapemap'/
      data iu/89/
      character*80 work
      character*8  wtype
c
c ====< Entry Point > ==============
c


	wtype = type
	ltyp = lnblnk(wtype)
	call uclowr(ltyp, wtype)
c
      dbsname=dbspath(:lnblnk(dbspath))//'/'//wtype(:ltyp)
      lname = lnblnk(dbsname)
      open(iu,file=dbsname(1:lname),status='old',iostat=ios)
      if( ios .ne. 0 ) then
        print *,'Error to open data base file in prgtif. File is'
        print *,dbsname(:lnblnk(dbsname))
        print *,' ios =',ios
        stop
      endif

110   continue
      read(iu,'(a)',end=900) work
      if( work(1:1).eq.'#') go to 110
      read(work,120) rtype, nexpr, nfilr, vol, npos, fname
120   format(a8,i4,i5,2x,a6,i4,2x,a24)      
      if( rtype(:ltyp).eq.wtype(:ltyp) 
     >    .and. nexpr.eq.nexp .and. nfil.eq.nfilr ) then
        close(iu)
        go to 300
      endif
      go to 110
C
C  Decide where to submit server job.
C
 300  continue
      open(iu,file=tapemap(:lnblnk(tapemap)),status='old',iostat=ios)
      if( ios.ne.0 ) then
        print *,'Unable to open a file ',tapemap(:lnblnk(tapemap))
        print *,'Fatal error in PRGTIF.'
        stop
      endif

 310  continue
      read(iu,'(a)',end=390) work
C      print *,' work =',work,' col =',vol
      if( index(work,vol).eq.0 ) go to 310
      if( work(1:6).eq.'robot9') then
        hostname='kektrf09'
      else
        hostname='kektrf08'
      endif
      close(iu)
      return

 390  continue
      close(iu)
      print *,'Tape ',vol,' does not defined in the tape map.'
      print *,' Map file is ',tapemap(:lnblnk(tapemap))
      npos = -2
      return


900   continue
      close(iu)
      print *,'in prgtif .. no information available for ',
     >  wtype(:ltyp)
      npos = -1
      return
      end
