C***********************************************************************
C* 
C*  --------------------------------======
C*  Subroutine UDAYDF( IDATE, KDATE, NDAY)
C*  --------------------------------======
C* 
C*(Function)
C*   Calculate day difference between IDATE and KDATE.
C* 
C*        NDAY  = IDATE - KDATE
C* 
C*(Input)
C*   IDATE   : Date (yymmnn)
C*   KDATE   : Date (yymmnn)
C* 
C*(Output)
C*   NDAY    : Difference
C*      ex.    871203 - 871203 = 0
C*             871203 - 871130 = 3
C* 
C*(Author)
C*   A. Miyamoto  3-Dec-1987  Original version.
C* 
C***********************************************************************
C* 
      SUBROUTINE UDAYDF( IDATE, KDATE, NDAY )
C* 
      INTEGER*4   NDYPED(12, 2)
      DATA        NDYPED/   0,  31,  59,  90, 120, 151,
     >                    181, 212, 243, 273, 304, 334,
     >                      0,  31,  60,  91, 121, 152,
     >                    182, 213, 244, 274, 305, 335/
      INTEGER*4       LDATE(2), NSEQDY(2)
C  
C ====< Entry Point >===================================================
C  
C ----------------------------------------------------------------------
C (1) Convert date to the sequential day from 1-Jan-1988
C ----------------------------------------------------------------------
C  
      LDATE(1) = IDATE
      LDATE(2) = KDATE
      DO 100 I = 1, 2
        IY     =  LDATE(I)/10000
        ID     =  MOD(LDATE(I), 100)
        IM     = (LDATE(I) - IY*10000)/100
        NSQY   =  IY-88
        IF( NSQY .GE. -3 ) THEN
          NSQYAD =  (NSQY+3)/4
        ELSE
          NSQYAD =  NSQY/4
        ENDIF
        NSEQDY(I) = 365*NSQY + NSQYAD
        IURUU    = 1
        IF( MOD(IY,4) .EQ. 0 ) IURUU = 2
        NSEQDY(I) = NSEQDY(I) + NDYPED(IM, IURUU) + ID
100   CONTINUE
C  
      NDAY = NSEQDY(1) - NSEQDY(2)
      RETURN
      END
