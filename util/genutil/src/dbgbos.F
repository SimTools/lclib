C
C ... Generate debug event to study calorimeter segmentaion.
C
C***********************************************************************
C*                                                                    *C
C*--------------------------------------------=====                   *C
C*   SUBROUTINE DBGBOS(IDREC, LEVEL, IDEBUG, NRET )                  *C
C*--------------------------------------------=====                   *C
C*                                                                    *C
C*(Function)
C*   Generate Debug event to study calorimeter segmentaion.
C*
C*(Author)
C*   A. Miyamoto   19-Apr-1990  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C --------------------------------------------=====
      SUBROUTINE DBGBOS(IDREC, LEVEL, IDEBUG, NRET)
C --------------------------------------------=====
C
c      IMPLICIT REAL*8 (A-H,O-Z,c_$)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON  /DBGPAR/ DBGDAT(20)
      REAL*4   DBGDAT
C
C    Parameters for detector smearing.
C      DBGDAT( 1) : = 0, 1, 2 ( Generate both, Z, and W )
C            ( 2) : = Z mass (GeV)
C            ( 3) : = Z width
C            ( 4) : = W mass
C            ( 5) : = W width
C            ( 6) : = Boson P min.(GeV)
C            ( 7) : = Boson P max.(GeV)
C            ( 8) : =       cos(th) min
C            ( 9) : =       cos(th) max
C            (10) : =       Phi(min)  radian
C            (11) : =       Phi(max)  radian
C
C
C    COMMON /SSWORK/ NOELM(500), GENTRK(20), DETINF(50)
C
      REAL*8   PK(0:3) , PH(0:3), PF(0:3, 4 )
C
      REAL*4   BR(12), BRSUM(12), DMOD
      DATA     BR/ 6.59, 6.59, 6.59, 3.32, 3.32, 3.31,
     >             15.23, 15.23, 15.08, 11.82, 11.81, 0.0/
C
C ... Particle Code According to PDG
C
      INTEGER*4   IDPART(12), NIDF(4), IMPNT(12)
      DATA     IDPART/ 12, 14, 16, 11, 13, 15,
     >                  1,  3,  5,  2,  4,  6/
      DATA     IMPNT/  1, 5, 9,   2, 6, 10,  4, 8, 12, 3, 7, 11/
      REAL*8    AMASS(12)
      DATA      AMASS/ 0., 0.5110034D-3, 0.04D0,  0.04D0,
     >                 0., 0.10565946D0,  1.5D0,  0.1D0,
     >                 0., 1.782D0,       1.0D3,  4.7D0 /
C
C ... Temporary data for the bank system
C
      INTEGER*4  IBUF(10)
      REAL*4     RBUF(20)
#if defined(G77_521_OR_GREATER)
      INTEGER*4  TIME
#endif
      DATA     IFIRST/1/
      DATA     ISEED/62327492/
C
C =====< Entry Point >=================================================
C
C ---------------------------------------------------------------------
C (1) Initial call
C ---------------------------------------------------------------------
C
C
C ---------------------------------------------------------------------
C (2) Generate.
C ---------------------------------------------------------------------
C
      IPART = 1
      IF( IFIX(DBGDAT(1)+0.1) .EQ. 2 ) IPART = 2
C    IF( IFIX(DBGDAT(1)+0.1) .EQ. 0 ) THEN
C     IF( RAN(ISEED) .GT. 0.5 ) IPART = 2
C   ENDIF
      AMB0  = DBGDAT(2*IPART)
      WMB0  = DBGDAT(2*IPART+1)
      AMB   = AMB0 + WMB0*RANN(ISEED)
C
      PHA   = DBGDAT( 6) + (DBGDAT( 7)-DBGDAT( 6))*RAN(ISEED)
      COSTH = DBGDAT( 8) + (DBGDAT( 9)-DBGDAT( 8))*RAN(ISEED)
      PHI   = DBGDAT(10) + (DBGDAT(11)-DBGDAT(10))*RAN(ISEED)
      TH    = ACOS(COSTH)
      SINTH = SIN(TH)
C
      PH (0) = SQRT( PHA*PHA + AMB*AMB )
      PH (1) = PHA*SINTH*DCOS(PHI)
      PH (2) = PHA*SINTH*DSIN(PHI)
      PH (3) = PHA*COSTH
C
C ........
C     Higgs decay to b-bbar
C ........
C
      AMF = 0
      CALL DECAY2( PH, AMB, AMF, AMF, PF(0,1), PF(0,2) )
C   NIDF(1) =  5
C   NIDF(2) = -5
 
      IF( RAN(ISEED) .GT. 0.5 ) THEN
        NIDF(1) = 1
        NIDF(2) = -2
      ELSE
        NIDF(1) = 3
        NIDF(2) = -4
      ENDIF
C
C
C --------------------------------------------------------------------
C (4) Put Generated event into bank.
C --------------------------------------------------------------------
C
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
      EBEAM   = PHA
      CALL PRINFO( 4, IBUF(1) )
      CALL UIDATE( IBUF(2) )
#if defined(G77_521_OR_GREATER)
      IBUF(3) = TIME()
#else
      CALL   TIME( IBUF(3) )
#endif
      IBUF(5) = EBEAM*1.E3
      IBUF(6) = EBEAM*1.E3
      IBUF(7) = EBEAM*1.E3
      IBUF(8) = 0
      IBUF(9) = 0
      IBUF(10)= 0
      CALL TBPUT(1, 'Spring:Header', 1, 10, IBUF, IRET )
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
C
C ... For Higgs
C
      CALL UVZERO( 20, RBUF )
      RBUF( 1) =   1
      RBUF( 2) =  25
      RBUF( 3) =  AMB
      RBUF( 4) =   0.
      RBUF( 5) = PH (1)
      RBUF( 6) = PH (2)
      RBUF( 7) = PH (3)
      RBUF( 8) = PH (0)
      RBUF(12) = 2
      RBUF(13) = 2
      RBUF(14) = -1
      RBUF(17) = 0
      CALL TBPUT(1,'Spring:Parton_List', 1, 20, RBUF, IRET )
C
C ... For Decay fermion.
C     Mass and charge of ferminon has no meaning.
C
      DO 500 IF = 1, 2
        CALL UVZERO( 20, RBUF )
        RBUF( 1) =   1 + IF
        RBUF( 2) =   NIDF(IF)
        RBUF( 3) =   0
        RBUF( 4) =   0
        RBUF( 5) = PF(1,IF)
        RBUF( 6) = PF(2,IF)
        RBUF( 7) = PF(3,IF)
        RBUF( 8) = PF(0,IF)
        RBUF(12) = 0
        RBUF(13) = 0
        IPAR     = 1
        RBUF(14) = IPAR
        RBUF(18) = IPAR
        CALL TBPUT(1,'Spring:Parton_List',1+IF, 20, RBUF, IRET )
500   CONTINUE
C
C
      RETURN
      END
CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Block Data DBGBOD                                                  *C
C*====================                                                *C
C*                                                                    *C
C*(Function)
C*   Default parameters for Smearing routine.
C*(Author)
C*   A.Miyamoto   4-Apr-1990  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
#ifdef HIUXF77
      SUBROUTINE DBGBOD
#else
      BLOCK DATA DBGBOD
#endif
C ==========================
C
      COMMON  /DBGPAR/ DBGDAT(20)
      REAL*4   DBGDAT
C
C    Parameters for detector smearing.
C      DBGDAT( 1) : = 0, 1, 2 ( Generate both, Z, and W )
C            ( 2) : = Z mass (GeV)
C            ( 3) : = Z width
C            ( 4) : = W mass
C            ( 5) : = W width
C            ( 6) : = Boson P min.(GeV)
C            ( 7) : = Boson P max.(GeV)
C            ( 8) : =       cos(th) min
C            ( 9) : =       cos(th) max
C            (10) : =       Phi(min)  radian
C            (11) : =       Phi(max)  radian
C
C
      DATA  DBGDAT/ 2,  91.1, 1.0, 80.0, 1.0, 100., 500.,
     >             -0.1, 0.1, 0., 3.1415 , 9*0. /
C
      END
C
CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine DBGBOR                                                  *C
C*====================                                                *C
C*                                                                    *C
C*(Function)
C*   Reads parameters for WW Event analysis.
C*(Author)
C*   A.Miyamoto  19-Oct-1989  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE DBGBOR
C ==========================
C
C
      COMMON  /DBGPAR/ DBGDAT(20)
      REAL*4   DBGDAT
      CHARACTER*40  INPUT
C
      NRET = 0
      NMOD = 0
100   CONTINUE
      IADR = 0
      CALL INTRD('DBGDAT Address ? ', IADR )
      IF( IADR .EQ. 0 .OR. IADR .GT. 18 ) GO TO 200
      CALL FLTRD(' Value ? ', DBGDAT(IADR) )
      NMOD = NMOD + 1
      GO TO 100
C
200   CONTINUE
 
      INPUT = '?'
      CALL TITRD('EXIT ?:', INPUT )
 
      WRITE(6,*) '%DBGBOR modified ',NMOD,' parameters.'
      WRITE(6,*) ' DBGBOS ... parameters used for analysis.'
      WRITE(6,*) '        Boson type(0,1,2)            ', DBGDAT(1)
      WRITE(6,*) '        Z Mass                       ', DBGDAT(2)
      WRITE(6,*) '        Z Width (Gaussian)           ', DBGDAT(3)
      WRITE(6,*) '        W Mmass                      ', DBGDAT(4)
      WRITE(6,*) '        W Width (Gaussian)           ', DBGDAT(5)
      WRITE(6,*) '        P min                        ', DBGDAT(6)
      WRITE(6,*) '        P max                        ', DBGDAT(7)
      WRITE(6,*) '        Cos(th) min                  ', DBGDAT(8)
      WRITE(6,*) '        Cos(th) max                  ', DBGDAT(9)
      WRITE(6,*) '        Phi min                      ', DBGDAT(10)
      WRITE(6,*) '        Phi max                      ', DBGDAT(11)
C
      RETURN
      END
C***********************************************************************
C*
C* -----------------
C* Subroutine DSDBGB
C* -----------------
C*
C*(Function)
C*   Describe Smearing routine.
C*
C*(Input & Output)
C*   None.
C*
C*(Relation)
C*   CALLed from FLANLI
C*
C*(Author)
C*   A. Miyamoto    8-Sep-1989  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DSDBGB
C
C =EXPAND 'T#PR.FULL.FORT(@FLDSCR)'
C =EXPAND 'T#PR.FULL.FORT(@FLLEVL)'
C =EXPAND 'T#PR.P#STER.FORT(@PRJFLG)'
C
      INCLUDE 'fldscr.inc'
C  T#FP.TOPAZANL.COM
      INCLUDE 'fllevl.inc'
C  T#FP.TOPAZANL.COM
      INCLUDE 'prjflg.inc'
C  T#FP.TOPAZANL.COM
C
C FUNCTIONS
C
      EXTERNAL      DBGBOS
      EXTERNAL      DBGBOR
      CHARACTER*12  PRGNAM/'DBG:Boson'/
C
C ======< Entry Point >=================================================
C
#ifdef HIUXF77
      CALL DBGBOD
#endif
C
C-----------------------------------------------------------------------
C 1. Define initialization and disgnostic entry.
C-----------------------------------------------------------------------
C
      NPROG = NPROG + 1
      NAMCOM(NPROG) = PRGNAM
      ENTDIA(NPROG) = 0
      ENTSTA(NPROG) = 0
C
C ----------------------------------------------------------------------
C (2) Define Initialization entry.
C ----------------------------------------------------------------------
C
      CALL EXTSUB ( ENTDIA(NPROG), DBGBOR )
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),DBGBOS )
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
 
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Spring:Header'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
 
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Spring:Parton_List'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
 
C
C-----------------------------------------------------------------------
C 3. Define default calling sequence.
C-----------------------------------------------------------------------
C
      LISTSP(0)     = NENTR
      LISTSP(NENTR) = NENTR
C
C-----------------------------------------------------------------------
C 4. Define Level and Debug operand.
C-----------------------------------------------------------------------
C
      EXELVL(NENTR) = 1
      DBGLVL(NENTR) = 1
C
      RETURN
      END
