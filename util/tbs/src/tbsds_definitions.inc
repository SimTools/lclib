C
C  Command Code definition.
C
C2345670
	parameter (ID_mount_tape=1,
     >	ID_dismount_tape=2,
     >	ID_fs_bs_f_tape=3,
     >	ID_rewind_tape=5,
     >	ID_sense_tape=6,
     >	ID_open_tbsfile=11,
     >	ID_open_tapefile=12,
     >	ID_close_tbsfile=13,
     >	ID_close_tapefile=14,
     >	ID_send_tbsfile=21,
     >	ID_define_records=22,
     >	ID_define_format=23,
     >	ID_get_iostat=24,
     >	ID_open_ascfile=31,
     >	ID_send_ascfile=32,
     >	ID_get_disk_iostat=33,
     >	ID_speed_test=96,
     >	ID_exec_shell_command=97,
     >	ID_are_you_there=98,
     >	ID_quit_server=99)
C
	structure /tbsds_tape_status/
	  character*16 serverlname
	  character*16 clienthost
	  character*16 clientlname
	  integer ltimestart
	  logical istape
	  character*8 tapename
	  integer lpos
	  logical isopen
	  character*32 filename
	end structure 

	structure /tbsds_tballoc/
	  character*64 recname
	  integer*4 idshare
	  integer*4 idrec
	  integer*4 maxbank
	  integer*4 maxelem
	  integer*4 maxsize
	end structure
	  
C
C  Data structure for comunication between server and client.
C
	structure /tbsds_rec_command/ 
	  integer length
	  integer id
	  integer nret
 	  union
	      map
	         character*8 tapename  ! for mount_tape and dismount_tape
	      end map
C for fsf and bsf
	      map
	         integer numfiles
	      end map
C for sense tape
	      map
	         record /tbsds_tape_status/ status
	      end map
C for open_tapefile
	      map
	        character*32 filename
	        integer lpos
	        logical mountbefore
	        logical rewindafter
	      end map
C for open_tbsfile
	      map
	        integer tbsfile_format
	        character*256 tbsfile_name
	      end map
C for send_tbsfile
	      map
	        integer tbsfile_numrecord
	      end map
C for exec_shell_Command
	  map
	        character*256 command
	  end map
C
C for open_ascfile
	  map
	       character*256 ascfile_name
	  end map
C
C for tballoc records.
C
	  map
	     record/tbsds_tballoc/ tbaloc
	  end map

	  end union

	end structure




