C***********************************************************************
C* 
C*  --------------------------=====
C*  Subroutine UDAYFC( NYDAY, CDATE )
C*  --------------------------=====
C* 
C*(Function)
C*   Convert FACOM format date to TOPAZ standard Date.
C* 
C*   ex.  88035 --> 88-02-04
C* 
C*(Input)
C*   NYDAY   : FACOM system standard date (I*4)
C* 
C*(Output)
C*   CDATE    : TOPAZ format date( Char*8)
C* 
C*(Author)
C*   A. Miyamoto  27-Sep-1988  Original version.
C* 
C***********************************************************************
C* 
      SUBROUTINE UDAYFC( NYDAY, CDATE )
C* 
      CHARACTER*8 CDATE
      INTEGER*4   NDYPED(12, 2)
      DATA        NDYPED/   0,  31,  59,  90, 120, 151,
     >                    181, 212, 243, 273, 304, 334,
     >                      0,  31,  60,  91, 121, 152,
     >                    182, 213, 244, 274, 305, 335/
C  
C ====< Entry Point >===================================================
C  
C ----------------------------------------------------------------------
C (1) Convert date
C ----------------------------------------------------------------------
C  
      IY = NYDAY/1000
      ID = MOD( NYDAY, 1000 )
      IU = 1
      IF( MOD(IY,4) .EQ. 0 ) IU = 2
      DO 100 IM = 1, 12
        IF( ID .LE. NDYPED(IM,IU)) GO TO 200
100   CONTINUE
200   CONTINUE
      IM = IM - 1
      ID = ID - NDYPED(IM,IU)
      WRITE(CDATE,300) IY, IM, ID
300   FORMAT(I2.2,'-',I2.2,'-',I2.2)
      RETURN
      END
