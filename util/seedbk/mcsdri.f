C***********************************************************************
C*
C*===============================
C* Subroutine MCSDRI(LEVEL,DEBUG)
C*===============================
C*
C* (Function)
C*    Initialize random number seeds.
C* (Update Record)
C*   95/08/31  K.Fujii		Original version.
C*
C***********************************************************************

      SUBROUTINE MCSDRI(LEVEL,DEBUG)

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
C  Now initialize MC seeds.
C--
      PRINT *, ' '
      PRINT *, 'Seed Booker - Initializing MC seeds.'
      PRINT *, ' '
C--
C  Decode flag.
C--
      IRDSP = MOD(IRDSED,2)
      IRDHD = IRDSED/2
C--
C  Reset SPRING seeds.
C--
      IF ( IRDSP.EQ.0 ) THEN
         ISEED = 12345
         CALL DRNSET(ISEED)
         PRINT *, 'IRDSP = 0: Reset SPRING seeds to default.'
      ELSE
         PRINT *, 'IRDSP = ', IRDSP, ': Use seeds from TBS.'
         PRINT *, ' SPRING: IX1, IX2, IX3 = ', IX1, IX2, IX3
      ENDIF
C--
C  Initialize Hadroniztion seeds.
C--
      IF ( IRDHD.NE.0 ) THEN
         PRINT *, 'IRDHD = ', IRDHD, ': Use seeds from TBS.'
C--TAUOLA
         CALL TBGET(2,'LUND73:Seed',1,NW,IDATA,IRT)
         CALL RMARIN(IDATA(1),IDATA(2),IDATA(3))
         CALL TBGET(2,'LUND73:Seed',2,NW,RANDTU,IRT)
         PRINT *, ' TAUOLA: I97, J97, C   = ', I97, J97, C
C--LUND73
         CALL TBGET(2,'LUND73:Seed',3,NW,RANDLU,IRT)
         PRINT *, ' LUND73: I97, J97, C   = ', 
     .                      MRLU(4), MRLU(5), RRLU(98)
      ELSE
         PRINT *, 'IRDHD = 0: Use default seeds for Hadronizer.'
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
