C*********************************************************************
C* 
C*    ------------------------======
C*    Subroutine UCDATE(IDATE, DATE)
C*    ------------------------======
C* 
C*(Function)
C*    Converts TOPAZ Date format into Character*8 format.
C*        yymmdd ---> yy-mm-dd
C* 
C*(Input)
C*    IDATE   :  Integer*4 variable of Date with format yymmdd.
C* 
C*(Output)
C*    DATE    :  Character*8 date with format yy-mm-dd
C* 
C*(Author)
C*    A. Miyamoto        19-Dec-1985  Original version.
C* 
C*********************************************************************
C* 
      SUBROUTINE UCDATE(IDATE, DATE)
C  
      CHARACTER*8 DATE
C  
      IY = IDATE/10000
      IM = (IDATE - IY*10000)/100
      ID =  IDATE - IY*10000 - IM*100
      WRITE( DATE, 100) IY, IM, ID
100   FORMAT(I2,'-',I2.2,'-',I2.2)
      RETURN
      END
