C*************************************************************
C*
C*  TBSDS_SERVER_MAIN
C*(Function)
C*  Main server program for TBS Data Server system.
C*
C*(Author)
C*  Akiya Miyamoto  25-March-1995.
C*
C**************************************************************
#include "tbsfmt.inc"
#include "tbsds_definitions.inc"
c
c---------------------- declair buffer for tbs.
c
      parameter    ( nsize = 400000 )
      common /tbsbuf/ ibuff( nsize )
      common /sswork/ kbuff( 0:50000 )

      record/tbsds_rec_command/ cmd

      character*8 hostname/'topsun1'/
      integer toserver/10000/, fromserver/10001/
      character*1 null/x'00'/

C
C==================<<  Entry Point  >>================================
C
C
C---------------------- TBS Initialize.
C
      nrwfmt(1)=0
      nrwfmt(2)=0
      nrwfmt(3)=0
      nrwfmt(4)=0
      call tbsizex(nsize, 50000)
      call tbwarn( 11 )
c
      ist = hostnm(hostname)
      print *,' tbsds_server_main running on the host ',hostname
      call tbsds_init_server(
     >   hostname(:lnblnk(hostname))//null, toserver, fromserver,
     >   iret)
      if( iret .lt. 0 ) then
	print *,'Unable to setup server socket.'
	stop
      endif
	

c
c (2) start loop to listen socket.
c
200	continue
	if( iend.eq.-1 ) go to 900

	call tbsds_receive(8000, nw, cmd)

	print *,' server received command : nw =',nw,
     >  ' length=',cmd.length,' id=',cmd.id,
     >  ' nret=',cmd.nret
	
	if ( nw .le. 0 ) go to 900
	if( cmd.id.eq.id_quit_server) then
	  go to 900

	else
	  call tbsds_process_request(cmd, nret)
	  print *,' send to client..nret =',nret
	
	endif

	if( nret.eq.0 ) go to 200

900    continue
	call tbsds_end
	
	stop
	end

C
	subroutine tbsds_process_request(cmd, nret)
C*
C*(Function)
C*  Perform proper action according to the request.
C*

#include "tbsfmt.inc"
#include "tbsds_definitions.inc"
      record/tbsds_rec_command/ cmd
c
c---------------------- declair buffer for tbs.
c
      parameter    ( nsize = 400000 )
      common /tbsbuf/ ibuff( nsize )
      common /sswork/ kbuff( 0:50000 )
      character*256   inputbuff

      structure /ascdata/
	integer lentot
	union
	  map
	    character*32000 str
	  end map
	  map
	    integer ndata(8000)
	  end map
	end union
      end structure
      record /ascdata/ asc

      logical tbxred

      real e, etime, t(2)

      character*256 work
      character*64 logfile
      integer getpid, system
      data id_unit/89/

      character*80 cbuf(100)
      character*1 NULL/x'00'/

      character*24 filvol, fildev

      
      logical tape_is_open
      common /tbsds_tapestat/ tape_is_open
C
C ======< Entry Point >===========================================
C

	if( cmd.id .eq. id_exec_shell_command) then
	  print *,' command =',cmd.command(:lnblnk(cmd.command))
	  is=system(cmd.command(:lnblnk(cmd.command)))
          cmd.nret = is
	  print *,' call tbsds_server_send commnad.'
	  call tbsds_send(3, cmd, iret)
c
c Mount tape request
c
	elseif( cmd.id .eq. id_mount_tape) then
c          ipid = getpid()
c          write(logfile,'(''/tmp/tbssm.'',i6.6)') ipid
c          work = 'dir_mount '//cmd.tapename//' >&! '//logfile
c          work = 'dir_mount '//cmd.tapename//' 1 '
c          is=system(work)
c	  if( is.ne.0 ) then
c            cmd.nret=is
c            cmd.command=logfile
c            cmd.length=128 	         
c            call tbsds_send(128, cmd, iret)
c	    return
c	  else
	    lnv = lnblnk(cmd.tapename)
	    if( cmd.tapename(:lnv) .eq. 'dm2105' ) then
	      filvol = cmd.tapename(:lnv)//'.toptape@kektrf08'
	    else
	      filvol = cmd.tapename(:lnv)//'.toptape@kektrf09'
	    endif
	    fildev='/dev/nrst0'
	    call tbtmnt(filvol(:lnblnk(filvol))//NULL,
     >                  fildev(:lnblnk(fildev))//NULL, iret)
c	  endif
          cmd.nret=iret
          cmd.command=logfile
          cmd.length=128 	         
          call tbsds_send(128, cmd, iret)
c
c Dismount tape request
c
	elseif( cmd.id .eq. id_dismount_tape) then
	  print *,' Dismount tape is requested.'
	  if( tape_is_open) then
	    call tbtcls(iret)
	    print *,'Tape is closed with iret=',iret
	    tape_is_open=.false.
	  endif
          ipid = getpid()
	  call tbtdsm(filvol(:lnblnk(filvol))//NULL, kret )	  
c          write(logfile,'(''/tmp/tbssm.'',i6.6)') ipid
c          work = 'dir_dismount robot9 >&! '//logfile
c          work = 'dir_dismount robot9 '
c          is=system(work)
          cmd.nret=kret
          cmd.command=logfile
          cmd.length=128 	         
          call tbsds_send(128, cmd, iret)
C
C
C Allocate TBS records.
C
	elseif( cmd.id .eq. id_define_records ) then
	  if(cmd.tbaloc.idshare .le.0 ) then
	    call tbshar(cmd.tbaloc.idshare, 
     >           cmd.tbaloc.recname, cmd.tbaloc.idrec, nret)
	    cmd.nret = nret
            print *,cmd.tbaloc.recname(:lnblnk(cmd.tbaloc.recname)),
     >            ' is shared with id ',cmd.tbaloc.idrec,' record.'
	    print *,' nret =',nret
	  else
	    call tbaloc(cmd.tbaloc.recname, cmd.tbaloc.idrec,
     >           cmd.tbaloc.maxbank, cmd.tbaloc.maxelem,
     >           cmd.tbaloc.maxsize, nret)
	    cmd.nret = nret
            print *,cmd.tbaloc.recname(:lnblnk(cmd.tbaloc.recname)),
     >            ' is allocated.  nret =',nret
	  endif  

C
C Open D-1 tape file for data transfer
C
	elseif( cmd.id .eq. id_open_tapefile ) then
          print *,'file name is ',cmd.filename
          print *,'lpos =',cmd.lpos
          nrwfmt(1) = 3
	  call tbtopn(cmd.filename(:lnblnk(cmd.filename))//NULL,
     >	              cmd.lpos, 'READ'//NULL)
	  print *,'Tape file is opened.'
	  id_unit=nsize
	  cmd.nret = ios
	  call tbsds_send(3, cmd, iret )
	  tape_is_open=.true.
	  print *,'tape_is_open is',tape_is_open
C
C Open TBS file for data transfer
C
	elseif( cmd.id .eq. id_open_tbsfile ) then
	  ln=lnblnk(cmd.tbsfile_name)
          nfmt=mod(cmd.tbsfile_format,20)
          nrwfmt(1) = nfmt
	  ios = -1
          if( nfmt.eq.0 .or. nfmt.eq.4 ) then
  	    open(id_unit,file=cmd.tbsfile_name,form='unformatted',
     >         status='old',iostat=ios)
	   elseif( nfmt.eq.1 ) then
	     print *,' Going to call tbvopn..'
	     call tbvopn(cmd.tbsfile_name(1:ln)//null)
	     ios = 0
          endif  
	  print *,'open TBS file ',cmd.tbsfile_name(:ln), 
     >    ' with nrwfmt=',nfmt,' iret=',ios
	  if( nfmt.ne.0 .and. nfmt .ne.4 ) id_unit=nsize
	  cmd.nret = ios
	  call tbsds_send(3, cmd, iret )

C
C Open ASC file for data transfer
C
	elseif( cmd.id .eq. id_open_ascfile ) then
	  open(id_unit, file=cmd.ascfile_name, form='formatted',
     >         status='old',iostat=ios)
	  cmd.nret = ios
	  call tbsds_send(3, cmd, iret )

C
C data transfer of TBS File data
C
	elseif( cmd.id .eq. id_send_tbsfile ) then
          print *,'Start tbsds_send tbs file.'
	  cmd.id  = 0
	  cmd.nret = 0
	  numsend = 0
	  ntotsize = 0
1000      continue
	  nw = nsize
	  do 1010 i = 1, 2*cmd.tbsfile_numrecord
 	  if( tbxred( id_unit, nw, ibuff(1) ) ) then
	     call tbsds_send( nw, ibuff, iret )
	     if( iret .lt. 0 ) then
               print *,'Error while sending data .'
	       call tbsds_send(1,0,iret)
	       return
	     endif
	     numsend = numsend + 1
	     ntotsize = ntotsize+nw
          else
	     call tbsds_send(1, 0, iret)
  	     if( nrwfmt(1).eq.0 .or. nrwfmt(1).eq.4 ) then
  	       close(id_unit)
	     elseif( nrwfmt(1).eq.3 .and. tape_is_open  ) then
	       call tbtcls(iret)
	      tape_is_open=.false.
	     endif
  	     ntotbyte = ntotsize*4
	     print *,'Complete transfer ',numsend,' records by eof.',
     >               ntotbyte,' bytes.'
	     return
          endif
1010      continue
	  ntotbyte = ntotsize*4
	  print *,'Complete transfer ',numsend,' records ',
     >            ntotbyte,' bytes ',
     >            'at the normal end.'
	  return	
C
C Send ASCII file 
C
	elseif( cmd.id .eq. id_send_ascfile ) then
	  cmd.id  = 0
	  cmd.nret = 0
          numsend = 0
2000      continue
	  asc.lentot = 0
	  ip  = 1
2100      continue
	  read(id_unit,'(A)',end=2200) inputbuff
	  linp = lnblnk(inputbuff)
c 	  print *,inputbuff(:linp)
	  asc.ndata(ip) = linp
	  ipstr = ip*4 + 1
c	  print *,' linp =',linp,' ipstr =',ipstr,' ip =',ip
	  asc.str(ipstr:) = inputbuff
	  ip = ip + ((linp-1)/4 + 2)
c	  print *,' ip =',ip,' linp =',linp
C	  if( ip .lt. 4000 ) go to 2100
	  if( ip .lt. 7900 ) go to 2100
C	  if( ip .lt. 790 ) go to 2100
C	  if( ip .lt. 1200 ) go to 2100
C	  if( ip .lt. 300 ) go to 2100

	  asc.lentot = ip
	  print *,' asc.lentot =',asc.lentot,' ip =',ip
	  call tbsds_send(ip, asc, iret )
c	  call system("sleep 2")
	  go to 2000
2200      continue
	  if( ip.gt. 1 ) then
	    asc.lentot = ip
  	    print *,' asc.lentot =',asc.lentot,' ip =',ip
  	    call tbsds_send(ip, asc, iret )
c 	  call system("sleep 2")
	  endif
	  nw = 1
	  ibuff(1) = 0
	  call tbsds_send(nw, ibuff, iret )
c  	  call system("sleep 2")
	  print *,' Send server end message.'
C
C Test transfer speed.
C
	elseif( cmd.id .eq. id_speed_test ) then
	  e=etime(t)
	  print *,'Elapsed e=',e,' user:',t(1),' system:',t(2)
	  asc.lentot = 8000
	  do i = 1, 8000
	    asc.ndata(i) = i
	  end do
	  do 3000 i = 1, 2
	    print *,' at i=',i
	    call tbsds_send(asc.lentot, asc, iret)
	    if( iret .lt. 0 ) then 
	      print *,' Received quit request '
	      go to 3100
	    endif
3000	  continue
	  e=time(t)
	  print *,'Elapsed e=',e,' user:',t(1),' system:',t(2)
3100      continue
	
	  call tbsds_send(1, 0, iret)
	  


        endif

	return
	end


