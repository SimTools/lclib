c+
c File : lun_to_fileno.f
c Description : Convert lun to file number
c Note : This routine works ONLY on HP.
c        AViiON -> use lun_to_fileno.c in libMISC.a
c
c Author : Ryosuke Itoh, TOPAZ, KEK
c Date : 8 - NOV - 1991
c-
c
      Subroutine lun_to_fileno_ ( lun, fn )
c
c Arg
c
      Integer * 4      lun
      Integer * 4      fn
c
c Function
c
      Integer * 4      FNUM
c
c Main
c
c#ifndef HIUXF77
c      fn = FNUM ( lun )
c#else
c      print *,' Fnum in tbs/lun_to_fileno for HIUX is not',
c     > ' supported yet.'
c#endif
      fn = FNUM ( lun )
      return
      end
