C*********************************************************************
C* 
C*    ----------------======
C*    Subroutine UTIME(JIKAN)
C*    ----------------======
C* 
C*(Function)
C*    Returns a time of a day by character format as follows.
C* 
C*       JIKAN = hh:mm:ss
C* 
C*    Using the subroutine TIME in Facom fortran library.
C* 
C*(Output)
C*    JIKAN   :  8 byte character to return time.
C* 
C*(Author)
C*    A. Miyamoto        15-Nov-1985  Original version.
C* 
C*********************************************************************
C* 
      SUBROUTINE UTIME(JIKAN)
C  
      CHARACTER*8 JIKAN
C  
#ifdef  MSP
       CALL TIME(ITIME)
       ITIME = ITIME/1000
       IHOUR = ITIME/3600
       IMIN  = MOD(ITIME,3600)/60
       ISEC  = ITIME - 3600*IHOUR - 60*IMIN
      WRITE(JIKAN,100) IHOUR, IMIN, ISEC
100   FORMAT(I2,':',I2.2,':',I2.2)
#endif
C
#ifdef UNIX
#if !defined(HIUXF77) && !defined(MACHTEN_PPC)
       INTEGER*4  STIME, TARRAY(9), TIME
C
       STIME = TIME()
       CALL LTIME(STIME, TARRAY)
       IHOUR = TARRAY(3)
       IMIN  = TARRAY(2)
       ISEC  = TARRAY(1)
       WRITE(JIKAN,100) IHOUR, IMIN, ISEC
100    FORMAT(I2,':',I2.2,':',I2.2)
#else
      CHARACTER*12 WORK
      CALL CLOCK(1, WORK)
      JIKAN = WORK(1:2)//':'//WORK(4:5)//':'//WORK(7:8)
#endif
#endif

C  
      RETURN
      END
