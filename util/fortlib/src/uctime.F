C*********************************************************************
C* 
C*    ------------------------======
C*    Subroutine UCTIME(ITIME, TIME)
C*    ------------------------======
C* 
C*(Function)
C*    Converts TOPAZ time format into Character*8 format.
C*        msec   ---> hh:mm:ss
C* 
C*(Input)
C*    ITIME   :  Integer*4 variable of time with format hhmmss.
C*               ITIME is time counted from 0:00 in msec unit.
C* 
C*(Output)
C*    TIME    :  Character*8 date with format hh:mm:ss
C* 
C*(Author)
C*    A. Miyamoto        19-Dec-1985  Original version.
C* 
C*********************************************************************
C* 
      SUBROUTINE UCTIME(ITIME, TIME)
C  
      CHARACTER*8 TIME
C  
      JTIME = ITIME/1000
      IHOUR = JTIME/3600
      IMIN  = MOD(JTIME,3600)/60
      ISEC  = JTIME - 3600*IHOUR - 60*IMIN
      WRITE( TIME,100) IHOUR, IMIN, ISEC
100   FORMAT(I2.2,':',I2.2,':',I2.2)
C  
      RETURN
      END
