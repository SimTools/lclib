      parameter ( mxsize = 1000 )
      integer*4 iwork(mxsize), ibuf(mxsize), jwork(mxsize)
      lu = 20
      open(lu,file='temp.dat',status='unknown')
      print *, 'Now calls tb_set_buffer'
      call tb_set_buffer (lu,mxsize,ibuf)
      nw = 10
      do 100 i = 1, nw
         iwork(i) = i
100   continue
      print *, 'Now calls tb_write_buffer'
      call tb_write_buffer(1,nw)
      call tb_write_buffer(nw,iwork)

      print *, 'Now calls tb_flush_buffer'
      call tb_flush_buffer(ntot)
      print *, 'tb_flush_buffer wrote ', ntot, ' words.'
      close(lu)

      open(lu,file='temp.dat',status='unknown')
      call tb_read_buffer(lu,nw,jwork)
      print *, 'nw = ', nw
      print *, 'jwork = ', (jwork(i),i=1,nw)
            
      print *, 'Now calls test2'
c      open(lu,file='temp.dat',status='unknown')
c      write(lu,'(A)') 'What'
c      call test2(lu)
      close(lu)
      end

