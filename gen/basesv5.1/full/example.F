C *******************************************************************
C  
C  Example to use BASES V5.1 with FULL
C
C  A.Miyamoto  19-Oct-1994
C
C *******************************************************************



CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C* Subroutine FLANLI                                                  *C
C*=================                                                   *C
C*    For e+e- ->nu nub gamma                                         *C
C*                                                                    *C
C*(Purpose)                                                           *C
C*    Define analysis program.                                        *C
C*(Relation)                                                          *C
C*    Called by PRMAIN.
C*(Author)                                                            *C
C*   17-May-1990  A. Miyamoto  For Full system                        *C
C*                                                                    *C
CC********************************************************************CC
C
      SUBROUTINE FLANLI
C
#include "fldscr.inc"
C
C======< ENTRY POINT >==================================================
C
C ----------------------------------------------------------------------
C (1) Declare version number of Full module.
C ----------------------------------------------------------------------
C
      CALL PRPDSC( 'e+e-->nu nu gamma', 101, 941019)
C
C ----------------------------------------------------------------------
C (2) Reset counters.
C ----------------------------------------------------------------------
C
      NPROG = 0
      NENTR = 0
      NHIST = 0
C
C ----------------------------------------------------------------------
C (3) Define analysis routine and calling sequence.
C ----------------------------------------------------------------------
C
C                     Define BASES generator.
      CALL DSBS51
C                     Define Lund Hadronizer.
      CALL DSLHAD
C                     List Generator:Particle_List bank
      CALL DSGELS
C                     Define Swimmer  routine.
      CALL DSCSWM
C                     Define Smearing routine.
      CALL DSCSMR
C                     Define User routine.
C      CALL DSCUSR
 
      RETURN
      END

C
C *********************************************************************
C
C  User initialization for BASES V5.1
C
C *********************************************************************
C
      SUBROUTINE USERIN

C
      IMPLICIT REAL*8(A-H,O-Z)
      EXTERNAL FUNC
 
      PARAMETER (MXDIM = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
C
      COMMON/KINEM/W,EM,ZM,ZGAM,CZ,CV,CA,FACTOR,XK,COSTH
C      REAL*4 P(4)
      DATA PI,ALP,GEVNB,GENER /
     .     3.1415926D0, 137.036D0, 0.38927D6, 3.0D0 /
 
********************************************************************
*     Initialization of BASES/SPRING V5.1
********************************************************************
*===> Initialization of BASES by calling BSINIT
 
C           CALL BSINIT
 
*===>  Initialization of parameters for kinematics etc.
 
           EM    = 0.511E-3
           ZM    = 90.0
           WM    = 78.97
           ZGAM  = 2.6
           ALPHA  = 1./ALP
           RAD    = PI/180.
           TWOPI  = 2.D0*PI
           SQ     = SQRT(ZM**2-WM**2)
           CZ     = ZM**2/(2.*WM*SQ)
           CA     = CZ*(1./2.)
           CV     = 2.*CA*(-1./2.+2.*SQ**2/ZM**2)
           FACTOR = GENER*CZ**2*ALPHA**3/12.*GEVNB
           COSMIN = 15.
           COSMAX =180.-COSMIN
           W      = 105.0D0
           XKMIN  = 1./W
 
*===>  Initialization of BASES parameters
 
           NDIM  = 2
           NWILD = 1
           NCALL = 5000
           ITMX1 = 10
           ACC1  = 0.1D0
           ITMX2 = 100
           ACC2  = 0.05D0
 
           XL(1)= XKMIN
           XU(1)= W/2.
           XL(2)= DCOS(COSMAX*RAD)
           XU(2)= DCOS(COSMIN*RAD)
 
*===>  Initialization of Histograms
          CALL XHINIT(1,XL(1),XU(1),40,'Photon Energy (GeV)')
          CALL XHINIT(2,XL(2),XU(2),50,'Cos(theta) of Photon')
          CALL DHINIT(1,XL(2),XU(2),50,XL(1),XU(1),50,
     .                 ' x : cos(theta)  --  y : Photon Energy ')
 
********************************************************************
*     Nimerical Integration by BASES V5.1
********************************************************************
 
C          CALL BASES( FUNC, ESTIM, SIGMA, CTIME, IT1, IT2 )
 
C          LU     = 6
 
C          CALL BSINFO( LU )
 
C          CALL BHPLOT( LU )
 
********************************************************************
*     Event generation by SPRING V5.1
********************************************************************
*===> Initialization of additional histograms
 
          CALL XHINIT( 5,  0.0D0, 4.0D1, 40,
     .                     'Photon Transverse Energy (GeV)')
 
*===> Event generation loop
 
C          MXTRY  = 50
C          MXEVNT = 10000
 
C          DO 100 NEVNT = 1, MXEVNT
 
C             CALL SPRING( FUNC, MXTRY )
 
*           Compute the four vectors of generated event
*           from the kinematical variables
 
C             PHI   = TWOPI*DRN(DUMY)
C             PXY   = XK*SQRT(1.0 - COSTH*COSTH)
C             P(1)  = PXY*COS(PHI)
C             P(2)  = PXY*SIN(PHI)
C             P(3)  = XK*COSTH
C             P(4)  = XK
 
C            WRITE(20) P
 
C             CALL XHFILL( 5, PXY, 1.D0)
 
C  100     CONTINUE
 
C          CALL SPINFO( LU )
 
C          CALL SHPLOT( LU )
 
C      STOP
      RETURN
      END



C
C***********************************************************************
C*===================                                                  *
C* Subroutine SPINIT                                                   *
C*===================                                                  *
C*                                                                     *
C*     This routine is called at Run initialization.                   *
C*     Store Run conditions into Begin_Run records.                    *
C*                                                                     *
C*   Coded by A. Miyamoto  28-Aug-1990                                 *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE SPINIT
C
      IMPLICIT REAL*8 (A-H,O-Z)

C
      COMMON /SSWORK/ ELMNO2(20), ELMNO3(10)
      CHARACTER*32    PRCNAM
C
C =====< Entry Point >=================================================
C
C
C
C ---------------------------------------------------------------------
C (1) Store Run parapemter into Bank 'Spring' Elm# 2.
C ---------------------------------------------------------------------
C
C     ELMNO2(1) = E-beam (GeV)
C           (2) = 1. When Beamstrahlung is on
C               = 0. When Beamstrahlung is off
C           (3:12) = reserved to store beam parameter.
C
      ELMNO2(1) = EBEAM
      ELMNO2(2) = NBSON
      ELMNO2(3) = 0.
      ELMNO2(4) = 0.
      ELMNO2(5) = 0.
      ELMNO2(6) = 0.
      ELMNO2(7) = 0.
      ELMNO2(8) = 0
      ELMNO2(9) = 0
      ELMNO2(10)= 0
      ELMNO2(11)= 0
      ELMNO2(12)= 0
C
      PRCNAM    = 'nu nub gamma'
      READ(PRCNAM,'(8A4)') (ELMNO2(K),K=13,20)
      NWD       = 20
C
      CALL TBPUT(2,'Spring',2,NWD,ELMNO2, IRET )
      IF ( IRET .LT. 0 ) THEN
        PRINT *,' USERIN failed to TBPUT SPRING Elm#2 ',IRET
        STOP
      ENDIF
C
C ---------------------------------------------------------------------
C (2) Store Run parapemter into Bank 'Spring' Elm# 3.
C     Store information specific to this process.
C ---------------------------------------------------------------------
C
C     ELMNO3(1) = AHMAS
C     ELMNO3(2) = AHWID
C     ELMNO3(3) = NFBMAS
C
C     NWD = 3
C     CALL TBPUT(2,'Spring',3,NWD,ELMNO3, IRET )
C     IF ( IRET .LT. 0 ) THEN
C       PRINT *,' USERIN failed to TBPUT SPRING Elm#3 ',IRET
C       STOP
C     ENDIF
C
C
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*===================                                                  *
C* Subroutine SPEVNT( NRET )                                           *
C*===================                                                  *
C*                                                                     *
C*(Purpose)                                                            *
C*   To enerate One event.                                             *
C*(Output)
C*   NRET : = 0 for normal return
C*          > 0 Warning, rest of the modules are processed.
C*          < 0 Error, rest ot the modules are skipped.
C*          > -999  the job is terminated imediately.
C*(Author)                                                             *
C*   Coded by A. Miyamoto 28-Aug-1990                                  *
C*            A. Miyamoto 19-Oct-1994 for BASES V5.1                   *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE SPEVNT(NRET)
C
C
      COMMON /SSWORK/ IBUF(1000), RBUF(1000)
C
      PARAMETER (MXDIM = 50 )
      REAL*8 XL,XU,ACC1,ACC2
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
C
      REAL*8 W,EM,ZM,ZGAM,CZ,CV,CA,FACTOR,XK,COSTH
      COMMON/KINEM/W,EM,ZM,ZGAM,CZ,CV,CA,FACTOR,XK,COSTH
      REAL*8 PXY

      REAL*4 P(4)
      DATA PI,ALP,GEVNB,GENER /
     .     3.1415926D0, 137.036D0, 0.38927D6, 3.0D0 /
      DATA   NEVENT/0/
C
C =====< Entry Point >=================================================
C
C
C ---------------------------------------------------------------------
C (1) Output event information into bank.
C ---------------------------------------------------------------------
C
C ... Spring:Header
C       Elm#1  IBUF(1) = Event #.
C                  (2) = Date
C                  (3) = Time
C                  (4) = not used
C                  (5) = E (e-)    MeV unit.
C                  (6) = E (e+)    MeV unit.
C                  (7) = Ebeam     MeV unit.
C                  (8) = Pol (e-)
C                  (9) = Pol (e+)
C                 (10) = not used.
 
 
      NEVENT = NEVENT + 1
      IBUF(1) = NEVENT
      CALL UIDATE( IBUF(2) )
      CALL UITIME( IBUF(3) )
      IBUF(5) = EMINS*1.E3
      IBUF(6) = EPLUS*1.E3
      IBUF(7) = EBEAM*1.E3
      IBUF(8) = 0.
      IBUF(9) = 0
      CALL TBPUT(1, 'Spring:Header', 1, 10, IBUF, IRET )
C
C ... Store event  parameter into element # 2.
C
C     CALL TBPUT(1,'Spring:Header',2,36,RS,IBUF)
C
C
C ... Spring:Parton_List
C       Elm#i  RBUF(1) = Particle serial number.
C                  (2) = Particle ID
C                  (3) = Mass (GeV)
C                  (4) = Charge
C                  (5) = Px(GeV)
C                  (6) = Py(GeV)
C                  (7) = Pz(GeV)
C                  (8) = E(GeV)
C                  (9) =    not used
C                 (10) =    not used
C                 (11) =    not used
C                 (12) = # daughter parton
C                 (13) = particle serial # of the 1st daughter
C                 (14) = particle serial # of the parent daughter
C                 (15) =    not used
C                 (16) =    not used
C                 (17) = helicity
C                 (18) = colour single group ID
C                 (19) =    not used
C                 (20) =    not used
C
C --------------------------------------------------------------------
C (2) Store 4 Vector of produced particles.
C --------------------------------------------------------------------
C
 
*           Compute the four vectors of generated event
*           from the kinematical variables
 
             PHI   = TWOPI*DRN(DUMY)
             PXY   = XK*SQRT(1.0 - COSTH*COSTH)
             P(1)  = PXY*COS(PHI)
             P(2)  = PXY*SIN(PHI)
             P(3)  = XK*COSTH
             P(4)  = XK
 
	     RBUF(1) = 1
             RBUF(2) = 22
	     RBUF(3) = 0
	     RBUF(4) = 0
	     RBUF(5) = P(1)
	     RBUF(6) = P(2)
	     RBUF(7) = P(3)
	     RBUF(8) = P(4)
	     DO 200 I = 9, 20
               RBUF(I) = 0.
200          CONTINUE
 
             CALL XHFILL( 5, PXY, 1.D0)

        CALL TBPUT(1,'Spring:Parton_List', 1, 20, RBUF, IRET )


        NRET = 0
 
      RETURN
      END
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
      INCLUDE 'bs51pr.inc'
      CHARACTER*4   CMD
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
     >         'INF OUTF'
      CALL TXTRD ( 'Enter parameter name or EXIT ', CMD )
      CALL STRUPC( 4, CMD )

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

      RETURN
      END
