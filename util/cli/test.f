      program main
c     
      character * 16 s
      s = 'this is a test'
      print *, s
      call strupc(16,s)
      print *, s
      stop
      end
