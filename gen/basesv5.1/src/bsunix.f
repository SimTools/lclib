      SUBROUTINE BSUNIX( ID )
*
      IMPLICIT REAL*8 ( A-H, O-Z )
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE0/ IFLAG,IBASES
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      COMMON/ NINFO  / NODEID, NUMNOD
      INTEGER UNIX
      COMMON/ BSCNTL / INTV, IPNT
C
      IF( ID .EQ. 1 ) THEN
*#ifdef INTEL
*      id_node  = mynode()
*      pid      = mypid()
*      no_node  = numnodes()
*      all_node = -1
*#elif AP
*      call cgcid(id_node)
*      call cgtid(pid)
*      call cgncl(no_node)
*#elif NCUBE
*      id_node  = mynode()
*      pid      = mypid()
*      no_node  = nnodes()
*#else
C**** for single CPU machine.
       NODEID   = 0
       NUMNOD   = 1
*#endif
C
C
*#ifdef INTEL
*      if( id_node .eq. 0 ) then
C
          READ( 5, * ) NLOOP, MLOOP
*          LOOPX(1) = NLOOP
*          LOOPX(2) = MLOOP
*
*          call csend( type_time,LOOPX, 2*4, all_node, pid)
*
*      else
*
*          call crecv( type_time, LOOPX, 2*4 )
*
*          NLOOP = LOOPX(1)
*          MLOOP = LOOPX(2)
*
*      endif
*#elif AP
*      NLOOP = 1
*      MLOOP = 1
*#elif NCUBE
*      call nglobal()
*      READ( 5, * ) NLOOP, MLOOP
*      call nlocal()
*#else
*      READ( 5, * ) NLOOP, MLOOP
*#endif
*
*
*#ifdef INTEL
*      if( id_node .eq. 0 ) then
          READ( 5, * ) NPRINT
*          call csend( type_time,NPRINT, 1*4 , all_node, pid)
*      else
*
*          call crecv( type_time,NPRINT, 1*4 )
*      endif
*#elif AP
*      NPRINT = 4
*#elif NCUBE
*      call nglobal()
*      READ( 5, * ) NPRINT
*      call nlocal()
*#else
*      READ( 5, * ) NPRINT
*#endif
*
*
*#ifdef INTEL
*      if( id_node .eq. 0 ) then
*
          READ( 5, * ) IFLAG
*          call csend( type_time,IFLAG, 1*4 , all_node, pid)
*      else
*
*          call crecv( type_time, IFLAG, 1*4 )
*
*      endif
*#elif AP
*      IFLAG = 0
*#elif NCUBE
*      call nglobal()
*      READ( 5, * ) IFLAG
*      call nlocal()
*#else
*      READ( 5, * ) IFLAG
*#endif
*
*
*#ifdef INTEL
*      if( id_node .eq. 0 ) then
*
          READ( 5, * ) SETIM
*
*          call csend( type_time,SETIM, 1*4, all_node, pid)
*
*      else
*
*          call crecv( type_time, SETIM, 1*4 )
*
*      endif
*#elif AP
*      SETIM = 1000000.
*#elif NCUBE
*      call nglobal()
*      READ( 5, * ) SETIM
*      call nlocal()
*#else
*      READ( 5, * ) SETIM
*#endif
*
      ELSE
      ENDIF
*
      RETURN
      END
