C***********************************************************************
C*
C*===================
C* Subroutine FLNPAR
C*===================
C*
C* (Function)
C*    Reads parameters for smearing.
C* (Update Record)
C*   95/02/11  K.Ffujii		Temporary version for LUND7.3.
C*     				At presetn, only decay switch is
C*				implemented.
C*
C***********************************************************************

      SUBROUTINE FLNPAR
C--
C  LUND commons.
C--
      COMMON /LUDAT3/ MDCY(500,3), MDME(2000,2),
     .                BRAT(2000), KFDP(2000,5)
      INTEGER*4       MDCY, MDME, KFDP
      REAL   *4       BRAT
C--
C  Variables.
C--
      CHARACTER*6    CMD
C
C========< Entry Point >================================================
C
C--
C  Now initialize LUHADR.
C--
      CALL CMxCLR
      PRINT *, 'LUND73 - Change LUND and TAUOLA parameters'
      PRINT *,' '
C--
C  Set default parameter values.
C--
      JAK1   = 0
      JAK2   = 0
      ITDKRC = 1
      KEYA1  = 1
      IHLON  = 1
      XK0DEC = 0.001
C--      
      MDCY(23,1) = 0
      MDCY(24,1) = 0
      MDCY(33,1) = 0
C--
C  Read in input paramters.
C--
100   PRINT *, 'Parameter keys: JAK1 JAK2 ITDKRC KEYA1 IHLON XK0DEC ',
     .                         'MDCY'
      CMD = '?'
C--
      CALL TXTRD ( ' Enter param. key or EXIT ', CMD )
      CALL STRUPC( 40, CMD )
C--
C  Branch on CMD.
C--
      IF ( CMD(1:4).EQ.'EXIT' ) THEN
	 PRINT *,'End of parameter setting in FLNPAR.'
						GO TO 900
      ELSE IF ( CMD(1:4).EQ.'JAK1' ) THEN
         CALL INTRD('JAK1: ', JAK1 )
         print *, 'JAK1   = ', JAK1
      ELSE IF ( CMD(1:4).EQ.'JAK2' ) THEN
         CALL INTRD('JAK2: ', JAK2 )
         print *, 'JAK2   = ', JAK2
      ELSE IF ( CMD(1:6).EQ.'ITDKRC' ) THEN
         CALL INTRD('ITDKRC: ', ITDKRC )
         print *, 'ITDKRC = ', ITDKRC
      ELSE IF ( CMD(1:5).EQ.'KEYA1' ) THEN
         CALL INTRD('KEYA1: ', KEYA1 )
         print *, 'KEYA1  = ', KEYA1
      ELSE IF ( CMD(1:5).EQ.'IHLON' ) THEN
         CALL INTRD('IHLON: ', IHLON )
         print *, 'IHLON  = ', IHLON
      ELSE IF ( CMD(1:6).EQ.'XK0DEC' ) THEN
         CALL FLTRD('XK0DEC: ', XK0DEC )
         print *, 'XK0DEC = ', XK0DEC
      ELSE IF ( CMD(1:4).EQ. 'MDCY' ) THEN
         CALL INTRD ( 'Enter I: ', IE )
         CALL INTRD ( 'Enter J: ', JE )
         CALL INTRD ( 'MDCY(I,J): ', MDCY(IE,JE) )
         print *, 'MDCY(',IE,',',JE,') = ', MDCY(IE,JE)
      ENDIF
                                                GO TO 100
200   CMD = '?'
      CALL TITRD('EXIT ?:', CMD )
C--
C  Initialize TAUOLA.
C--
900   INUT   = 5
      IOUT   = 6
C--
      PRINT *, '>>> FLNPAR now invokes TAUINT >>>'
C--
      CALL TAUINT(INUT,IOUT,JAK1,JAK2,ITDKRC,KEYA1,XK0DEC)
      CALL LUTUHL(1,IHLON)
C--
C  That's it.
C--
      RETURN
      END
