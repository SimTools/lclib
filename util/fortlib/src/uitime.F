      SUBROUTINE UITIME(ITIME)
C 
C This routine returns sec. from the midnight 
C prepared to provide similar function as sub. TIME in 
C FACOM MSP Fortran77
C

#if defined(HP) && !defined(MACHTEN_PPC)
      INTEGER*4   N, TIME, TA(9)
      N = TIME()
      CALL LTIME(N, TA)
      ITIME=(TA(3)*3600+TA(2)*60+TA(1))*1000
#endif
#ifdef HIUXF77
      CALL CLOCK(2, ITIME)
#endif
      RETURN
      END
