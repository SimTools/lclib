C***********************************************************************
C*
C*===================
C* Subroutine MCSDPR
C*===================
C*
C* (Function)
C*    Reads parameters for seed handling
C* (Update Record)
C*   95/08/31  K.Fujii		Original version.
C*
C***********************************************************************

      SUBROUTINE MCSDPR
C--
C  Common blocks.
C--
      INCLUDE 'MCSDFG.inc'
C--
C  Variables.
C--
      CHARACTER*6    CMD
C
C========< Entry Point >================================================
C
C--
C  Now initialize Seed Booker.
C--
      CALL CMxCLR
      PRINT *, 'Seed Booker - Reading MC seeds.'
      PRINT *,' '
C--
C  Set default parameter values.
C  The seed information is read from or write to a disk file
C  as a part of Begin_Run record at the run end according to
C  flags:
C     LOUNIT        = seed output unit.
C     IRDSED/IWTSED = ISP + 2*IHD for READ/WRITE control
C  where
C     ISP/IHD = (0:default, 1:seeds from banks) for SP/HD step.
C  Current status of random number seeds can be saved on an
C  event by event basis if requested:
C     ISVSED = (0:do not, 1:do) save SPRING/LUND73:Seed_Now.
C--
      LOUNIT = 32
      IRDSED = 0
      IWTSED = 3
      ISVSED = 0 
      SDFILE = 'ft32_BR.tbs'
C--
C  Read in input paramters.
C--
100   PRINT *, 'Parameter keys: LUSD RDSD WTSD SVSD SEDF'
      CMD = '?'
C--
      CALL TXTRD ( ' Enter param. key or EXIT ', CMD )
      CALL STRUPC( 40, CMD )
C--
C  Branch on CMD.
C--
      IF ( CMD(1:4).EQ.'EXIT' ) THEN
	 PRINT *,'End of parameter setting in MCSDPR.'
						GO TO 900
      ELSE IF ( CMD(1:4).EQ.'LUSD' ) THEN
         CALL INTRD('LUSD: ', LOUNIT )
         print *, 'LUSD   = ', LOUNIT
      ELSE IF ( CMD(1:4).EQ.'RDSD' ) THEN
         CALL INTRD('RDSD: ', IRDSED )
         print *, 'RDSD   = ', IRDSED
      ELSE IF ( CMD(1:4).EQ.'WTSD' ) THEN
         CALL INTRD('WTSD: ', IWTSED )
         print *, 'WTSD   = ', IWTSED
      ELSE IF ( CMD(1:4).EQ.'SVSD' ) THEN
         CALL INTRD('SVSD: ', ISVSED )
         print *, 'SVSD   = ', ISVSED
      ELSE IF ( CMD(1:4).EQ.'SEDF') THEN
	 SDFILE = '?'
	 CALL TXTRD('SEDF: ',SDFILE)
         PRINT *, ' SEDF=', SDFILE(:LNBLNK(SDFILE))
      ENDIF
                                                GO TO 100
200   CMD = '?'
      CALL TITRD('EXIT ?:', CMD )
C--
C  OK.
C--
900   CONTINUE
C--
C  That's it.
C--
      RETURN
      END
