      integer*4 itime
      character*8 hizuke, jikan
      
      integer*4 time
      external time, uitime
      
      call date(hizuke)
      call utime(jikan)
      call uitime(itime)

      print *, 'hizuke = ', hizuke
      print *, 'jikan  = ', jikan
      print *, 'itime  = ', itime
      print *, 'jtime  = ', time()
      end

