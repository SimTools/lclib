CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine BS51CM                                                  *C
C*====================                                                *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Read parameter for BASES/SPRING job.                             *C
C*   This routine should return to caller when read 'EXIT'.           *C
C*   This routine is called prior to the job initialization by        *C
C* steering routine when the string 'SPRING' is read from FT05F001.   *C
C*   This example reads parton ID, set to common /SPDATA/ for the     *C
C* use in sub. SPEVNT.                                                *C
C*                                                                    *C
C*(Author)                                                            *C
C*   Akiya Miyamoto  20-May-1990   Original version.                  *C
C*                   19-Oct-1999   SPGPAR renamed to BS51CM           *C
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE BS51CM
C ==========================
C
      INCLUDE 'BS51PR.inc'
      INCLUDE 'BS51DF.inc'
      INCLUDE 'WWZPRM.inc'

      CHARACTER*20   CMD
C
C =====< Entry Point >==================================================
C
C --------------------------------------------
C  Example of read parameters using COM/CLI
C --------------------------------------------
C

      PRINT *, 'BASES_V5.1 - Change parameters for BASES V5.1 ',
     >         ' generator .'
      PRINT *,' '
100   CONTINUE
      PRINT *, 'Parameter name : GETB PUTB DOSP PHST PINF PLOG ',
     >         'INF OUTF MXTRY'
      CALL TXTRD ( 'Enter parameter name or EXIT ', CMD )
      CALL STRUPC( 20, CMD )

      IF( CMD(1:4).EQ.'EXIT') THEN
	PRINT *,'End of parameter setting (in BS51CM).'
	GOTO 900
	
      ELSEIF( CMD(1:4).EQ.'GETB') THEN
	CALL INTRD('GETB: ',NGETBS)

      ELSEIF( CMD(1:4).EQ.'PUTB') THEN
	CALL INTRD('PUTB: ',NPUTBS)

      ELSEIF( CMD(1:4).EQ.'DOSP') THEN
	CALL INTRD('DOSP: ',NDOSPR)

      ELSEIF( CMD(1:4).EQ.'PHST' ) THEN
	CALL INTRD('PHST: ',NPRHST)
	
      ELSEIF( CMD(1:4).EQ.'PINF') THEN
	CALL INTRD('PINF: ', NPRINF)

      ELSEIF( CMD(1:4).EQ.'PLOG') THEN
	CALL INTRD('PLOG: ',NBSINF)

      ELSEIF( CMD(1:3).EQ.'INF') THEN
	BSINF = ' '
	CALL TXTRD('Input File: ',BSINF)
	
      ELSEIF( CMD(1:4).EQ.'OUTF') THEN
	BSOUTF = ' '
	CALL TXTRD('Output File: ',BSOUTF)

      ELSEIF( CMD(1:5).EQ.'MXTRY') THEN
	CALL INTRD('MXTRY: ',MXTRY)

C
C Parameters specific for this generator.
C
      ELSEIF( CMD(1:5).EQ.'EBEAM') THEN
        EBI = EBEAM
        CALL FLTRD('Ebeam:',EBI)
        EBEAM = EBI

      ELSEIF( CMD(1:5).EQ.'NBSON') THEN
         CALL INTRD('NBSON:',NBSON)

      ELSEIF( CMD(1:6).EQ.'NBTYPE') THEN
         CALL INTRD('NBTYPE:',NBTYPE)

      ELSEIF( CMD(1:5).EQ.'ITMX1') THEN
         CALL INTRD('ITMX1:',JTMX1)

      ELSEIF( CMD(1:5).EQ.'ITMX2') THEN
         CALL INTRD('ITMX2:',JTMX2)

      ELSEIF( CMD(1:5).EQ.'NCALL') THEN
         CALL INTRD('NCALL:',JCALL)

      ELSEIF( CMD(1:4).EQ.'ACC1') THEN
         CALL FLTRD('ACC1:',BCC1)

      ELSEIF( CMD(1:4).EQ.'ACC2') THEN
         CALL FLTRD('ACC2:',BCC2)

C
      ELSE
	PRINT *,' '
	PRINT *,'Command parameters for BASES_V5.1:'
	PRINT *,'GETB: 0 = Do BASES calculation at begin run.'
	PRINT *,'      1 = Get BASES data from TBS bank.'
        PRINT *,'      2 = Get BASES data from BASES ',
     >          'standard format file.' 
	PRINT *,'PUTB: 0 = Does not output BASES result.'
	PRINT *,'      1 = Write BASES results into TBS bank.'
	PRINT *,'      2 = Write BASES results with BASES ',
     >          ' standard file format.'
	PRINT *,'DOSP: 1(0) = Do (not do) SPRING.'
	PRINT *,'PHST: 1(0) = Print (not print) BASES/SPRING  Hist.'
	PRINT *,'PINF: 1(0) = Print (not print) BASES/SPRING Info.'
	PRINT *,'PLOG: 1(0) = Print (not print) BASES results.'
	PRINT *,'INF:  Input file name for GETB=2 case.'
	PRINT *,'OUTF: Output file name for PUTB=2 case.'
	PRINT *,'Valid combination of parameters are:'
	PRINT *,' (1)  GETB=0, PUTB=0/1/2, DOSP=0/1 '
        PRINT *,' (2)  GETB=1, PUTB=0/2, DOSP=1 '
	PRINT *,' (3)  GETB=2, PUTB=0/1/2, DOSP=1/2'
        PRINT *,'MXTRY: Max try in SPRING'
        PRINT *,' '
C
C       NBSON = 0, no beam strulung.
C             = 1, with Beamstrulung.
C             = 2, with Bremstrulung.
C             = 3, with Bremstrulung and beamstrulung.
        PRINT *,'Parameters specific for this generator(ee->WW).'
        PRINT *,'EBEAM: Beam energy(GeV)'
        PRINT *,'NBSON: Beamstrulung/Bremstrulung switch.'
        PRINT *,'       = 0 ; Neither Beamstrulung nor Bremstrulung'
        PRINT *,'       = 1 ; With Beamstrulung'
        PRINT *,'       = 2 ; With Bremstrulung'
        PRINT *,'       = 3 ; With Beamstrulung and Bremstrlung'
        PRINT *,'NBTYPE : Beamstrahlung spectrum type '
        PRINT *,'ITMX1: ITMX1 for BASES.'
        PRINT *,'ITMX2: ITMX2 for BASES.'
        PRINT *,'NCALL: NCALL for BASES.'
        PRINT *,'ACC1:  ACC1 for BASES.'
        PRINT *,'ACC2:  ACC2 for BASES.'
      ENDIF
      GOTO 100
C
C     Call FLTRD( 'message', XDATA ) ; Read REAL*4 value.
C     Call INTRD( 'message', IDATA ) ; Read Integer*4 calue.
C     Call TXTRD( 'message', STRING ) ; Read String
C     Call TITRD( 'message', STRING ) ; Read a line.
C
C -------------------------------------------------------------------- C
C     Return
C -------------------------------------------------------------------- C
C
900   CONTINUE
      PRINT *,'Parameters for BASES V5.1'
      PRINT *,' GETB=',NGETBS,' PUTB=',NPUTBS,' DOSP=',NDOSPR
      PRINT *,' PHST=',NPRHST,' PINF=',NPRINF,' PLOG=',NBSINF
      IF( NGETBS.EQ.2 ) PRINT *,' In file=',BSINF(:LNBLNK(BSINF))
      IF( NPUTBS.EQ.2 ) PRINT *,' Out file=',BSOUTF(:LNBLNK(BSOUTF))  
      PRINT *,'Parameters specific for this generator(ee->WW).'
      PRINT *,'Ebeam =',EBEAM,' GeV'
      PRINT *,' NBSON =',NBSON
      PRINT *,'       = 0 ; Neither Beamstrulung nor Bremstrulung'
      PRINT *,'       = 1 ; With Beamstrulung'
      PRINT *,'       = 2 ; With Bremstrulung'
      PRINT *,'       = 3 ; With Beamstrulung and Bremstrlung'
      PRINT *,' NBTYPE : Beamstrahlung spectrum type '
      PRINT *,'       NBTYPE =',NBTYPE
      PRINT *,' ITMX1=',JTMX1,' ITMX2=',JTMX2,' NCALL=',JCALL
      PRINT *,' ACC1=',BCC1,' ACC2=',BCC2

      RETURN
      END

C***********************************************************************
C*                                                                     *
C*===================                                                  *
C* Block Data WWDCYD
C*===================                                                  *
C*                                                                     *
C*(Purpose)
C*   Block data for WW event generator.
C*(Author)                                                             *
C*   Coded by A. Miyamoto  22-Nov-1994
C*                                                                     *
C***********************************************************************

      BLOCK DATA WWDCYD

      INCLUDE 'WWZPRM.inc'
      INCLUDE 'BS51DF.inc'

      DATA EBEAM/250.D0/
      DATA NBSON/0/
      DATA NBTYPE/0/

      DATA JTMX1/10/
      DATA JTMX2/10/
      DATA JCALL/100000/
      DATA BCC1/0.40/
      DATA BCC2/0.20/

      END

