C***********************************************************************
C*
C*==========================================----===
C* Subroutine MCSEED( IDREC, LEVEL, IDEBUG, IRET )
C*==========================================----===
C*
C* (Function)
C*    Save and monitor random number seeds.
C* (Update Record)
C*   95/08/31  K.Fujii		Original version.
C*
C***********************************************************************

      SUBROUTINE MCSEED(IDREC,LEVEL,IDEBUG,IRET)

      INTEGER*4  LEVEL, IDEBUG, IRET
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
      DATA IEVT /0/
C
C========< Entry Point >================================================
C
C--
C  Skip if non-event record.
C--
      IRET = 0
      IF ( IDREC.NE.1  ) RETURN
      IEVT = IEVT + 1
      IF ( IWTSED.EQ.0 .OR. ISVSED.EQ.0 ) RETURN
C--
C  Save Spring seeds in 'SPRING:Seed_Now'.
C--
      NW = LEN_SP
      CALL TBPUT(1,'SPRING:Seed_Now',1,NW,RANDSP,IRT)
C--
C  Save Hadronization seeds in 'LUND73:Seed_Now'.
C--
      CALL RMARUT(IDATA(1),IDATA(2),IDATA(3))
      NW = 3
      CALL TBPUT(1,'LUND73:Seed_Now',1,NW,IDATA,IRT)
      NW = LEN_TU
      CALL TBPUT(1,'LUND73:Seed_Now',2,NW,RANDTU,IRT)
C--
      NW = LEN_LU
      CALL TBPUT(1,'LUND73:Seed_Now',3,NW,RANDLU,IRT)
C--
C  Monitor MC seeds.
C--
      IF ( IDEBUG.LE.0 ) RETURN
C--
      PRINT *, ' '
      PRINT *, 'Seed booker - Monitoring MC seeds: IEVT = ', IEVT
      PRINT *, ' '
      PRINT *, ' SPRING: IX1, IX2, IX3 = ', IX1, IX2, IX3
      PRINT *, ' TAUOLA: I97, J97, C   = ', I97, J97, C
      PRINT *, ' LUND73: I97, J97, C   = ', MRLU(4), MRLU(5), RRLU(98)
C--
C  That's it.
C--
      RETURN
      END
