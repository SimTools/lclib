C***********************************************************************
C*
C*===============================
C* Subroutine MCSDRT(LEVEL,DEBUG)
C*===============================
C*
C* (Function)
C*    Book random number seeds.
C* (Update Record)
C*   95/08/31  K.Fujii		Original version.
C*
C***********************************************************************

      SUBROUTINE MCSDRT(LEVEL,DEBUG)

      INTEGER*4  LEVEL, DEBUG
C--
C  Common blocks.
C--
      INCLUDE 'MCSDFG.inc'
C--
      PARAMETER ( LEN_SP = 45, LEN_TU = 100, LEN_LU = 106 )
C--
      COMMON /RANDM/  RDM(31), RM1, RM2, IA1, IC1, M1, IX1,
     .                                   IA2, IC2, M2, IX2,
     .					 IA3, IC3, M3, IX3
      REAL*4          RANDSP(LEN_SP)
      EQUIVALENCE    (RDM(1),RANDSP(1))
C--      
      COMMON /RASET1/ U(97), C, I97, J97
      REAL*4          RANDTU(LEN_TU)
      EQUIVALENCE    (U(1),RANDTU(1))
C--
      COMMON /LUDATR/ MRLU(6), RRLU(100)
      REAL*4          RANDLU(LEN_LU)
      EQUIVALENCE    (MRLU(1),RANDLU(1))
C--
C  Local variables.
C--
      INTEGER*4 IDATA(500)
C
C========< Entry Point >================================================
C
C--
C  Now book-keep MC seeds.
C--
      PRINT *, ' '
      PRINT *, 'Seed Booker - Book keeping MC seeds.'
      PRINT *, ' '
C--
C  Decode flag.
C--
      IWTSP = MOD(IWTSED,2)
      IWTHD = IWTSED/2
C--
C  Save SPRING seeds.
C--
      IF ( IWTSP.NE.0 ) THEN
         PRINT *, 'IWTSP = ', IWTSP, ': Output seeds to TBS.'
         PRINT *, ' SPRING: IX1, IX2, IX3 = ', IX1, IX2, IX3
         CALL TBPUT(2,'BASES:Results',6, LEN_SP, RANDSP, IRT)
         IF ( IRT.LT.0 ) THEN
	    PRINT *,'MCSDRT: failed to TBPUT /RANDM/, IRET=',IRT
	    RETURN
         ENDIF
      ENDIF
C--
C  Save Hadronization seeds in 'LUND73:Seed'.
C--
      IF ( IWTHD.NE.0 ) THEN
C--
         PRINT *, 'IWTHD = ', IWTHD, ': Output seeds to TBS.'
         CALL TBCRTE( 2, 'LUND73:Seed', 0, 0, IRT )
C--
         CALL RMARUT(IDATA(1),IDATA(2),IDATA(3))
         NW = 3
         CALL TBPUT(2,'LUND73:Seed',1,NW,IDATA,IRT)
         NW = LEN_TU
         CALL TBPUT(2,'LUND73:Seed',2,NW,RANDTU,IRT)
         PRINT *, ' TAUOLA: I97, J97, C   = ', I97, J97, C
C--
         NW = LEN_LU
         CALL TBPUT(2,'LUND73:Seed',3,NW,RANDLU,IRT)
         PRINT *, ' LUND73: I97, J97, C   = ', 
     .                      MRLU(4), MRLU(5), RRLU(98)
      ENDIF
C--
C  TBWRIT 'Begin_Run' record to LOUNIT.
C--
      PRINT *, ' '
      PRINT *, 'Output the random number status to LU = ', LOUNIT
      PRINT *, 'File name = ', SDFILE(:LNBLNK(SDFILE))
      PRINT *, ' '
      OPEN(LOUNIT,FILE=SDFILE(:LNBLNK(SDFILE)),FORM='UNFORMATTED')
      CALL TBWRIT(LOUNIT,2,IRT)
C--
C  That's it.
C--
      RETURN
      END
