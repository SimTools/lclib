c+
c File : lun_to_fileno.f
c Description : Convert lun to file number
c Note : This routine works ONLY on SUN. 
c        AViiON -> use lun_to_fileno.c in libMISC.a
c
c Author : Ryosuke Itoh, TOPAZ, KEK
c Date : 8 - NOV - 1991
c-
c
      Subroutine lun_to_fileno ( lun, fn )
c
c Arg
c
      Integer * 4      lun
      Integer * 4      fn
c
c Function
c
      Integer * 4      getfilep
      External         getfilep
c
c Main
c
      call fd_to_fileno ( getfilep(lun), fn )
      return
      end
