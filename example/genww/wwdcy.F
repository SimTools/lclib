C*****************************************************************
C*
C*
C*    To generate
C*
C*          e+ + e- --> W + W
C*
C*          Include decay of W, W
C*
C*    Version saved at 24-Apr-1991
C*    Before working on CP violating anomalous coupling
C*
C*
C***********************************************************************
C*===================                                                  *
C* Subroutine USERIN                                                   *
C*===================                                                  *
C*                                                                     *
C*     This is USERIN for Z pair production and decay.                 *
C*                                                                     *
C*   Coded by A. Miyamoto  18-Oct-1989                                 *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE USERIN
C
      INCLUDE 'WWZPRM.inc'
      INCLUDE 'BSPARM.inc'
      INCLUDE 'BS51DF.inc'
C
C     
C =====< Entry Point >=================================================
C
C
C ----------------------------------------------------------------------
C (1) Initialization, set parameters.
C ----------------------------------------------------------------------
C
C

      PRINT *,' EBEAM =',EBEAM
      ACC1 = BCC1
      ACC2 = BCC2
      ITMX1 = JTMX1
      ITMX2 = JTMX2
      NCALL = JCALL

      PRINT *,' ACC1=',ACC1,' ACC2=',ACC2
      PRINT *,' TMX1, ITMX2=',ITMX1,ITMX2
      PRINT *,' NCALL =',NCALL

      EB      = EBEAM
      RS      = 2.*EBEAM
C
C ---------------------------------------------------------------------
C (2) Parameters for the BASES.
C ---------------------------------------------------------------------
C
C  Definition of vairables
C   1 ; Polarization variable.             0 --> 1
C   2 ; Invariant mass of W-               0 --> 1
C   3 ; Invariant mass of W+               0 --> 1
C   4 ; Cos(th) of W-                     -1 --> 1
C   5 ; Cos(th)-hat of f in W- system     -1 --> 1
C   6 ; Cos(th)-hat of f in W+ system     -1 --> 1
C   7 ; Parameter for Bremstrulung         0 --> 1
C   8 ; E- beam                            0 --> 1
C   9 ; E+ beam                            0 --> 1
C  10 ; Phi/2pi of W-                      0 --> 1
C  11 ; Phi-hat/2pi of f in W- system      0 --> 1
C  12 ; phi-hat/2pi of f in W+ system      0 --> 1
C  13 ; Decay mode of W-                   0 --> 1
C  14 ; Decay mode of W+                   0 --> 1
C
      NDIM  = 14
      NGDIM = 10
 
      DO 200 I = 1, NDIM
        XL(I) = 0.D0
        XU(I) = 1.D0
200   CONTINUE
      DO 210 I =  4,  6
        XL(I) =-1.D0
210   CONTINUE
 
CCOSW08       XU(4) = +0.8D0
C
      CALL XHINIT( 1,   0.D0,  1.D0,  2, 'ds/hel ')
      CALL XHINIT( 2,XL( 2),XU( 2), 50,' ds/d(W-mass)')
      CALL XHINIT( 3,XL( 3),XU( 3), 50,' ds/d(W+mass)')
      CALL XHINIT( 4,XL( 4),XU( 4), 50,' ds/dcosth(W-)' )
      CALL XHINIT( 5,XL( 5),XU( 5), 50,' ds/dcosth(f) of W-')
      CALL XHINIT( 6,XL( 6),XU( 6), 50,' ds/dcosth(f) of W+')
      CALL XHINIT( 7,XL( 7),XU( 7), 50,' ds/dX(Brems)')
      CALL XHINIT( 8,XL( 8),XU( 8), 50,' ds/dX(Beamst(E-)')
      CALL XHINIT( 9,XL( 9),XU( 9), 50,' ds/dX(Beamst(E+)')
      CALL XHINIT(10,XL(10),XU(10), 50,' ds/dphi of W-')
      CALL XHINIT(11,XL(11),XU(11), 50,' ds/dphi of f of W-')
      CALL XHINIT(12,XL(12),XU(12), 50,' ds/dphi of f of W+')
      CALL XHINIT(13,XL(13),XU(13), 50,' ds/ddecay mode of W-')
      CALL XHINIT(14,XL(14),XU(14), 50,' ds/ddecay mode of W+')
C
      CALL XHINIT(15, 1.d0,12.d0, 12,  'Decay mode of W-')
      CALL XHINIT(16, 1.d0,12.d0, 12,  'Decay mode of W+')
      CALL XHINIT(17, 0.0d0, EBEAM, 50, 'W- invariant mass')
      CALL XHINIT(18, 0.0d0, EBEAM, 50, 'W+ invariant mass')
      CALL XHINIT(19, 0.0d0, 1.D0 , 50, 'Effective root-S')
C
      RETURN
      END
C***********************************************************************
C*===================                                                  *
C* FUNCTION   FUNC*8(X)                                                *
C*===================                                                  *
C*                                                                     *
C*     This is FUNC to calculate TOTAL cross section of                *
C*       e+e- --> W W --> decay.                                       *
C*(Input)                                                              *
C*                                                                     *
C*   Definition of vairables                                           *
C*
C*                                                                     *
C*(Output)                                                             *
C*  FUNC : Cross section (fb)                                          *
C*                                                                     *
C*(Author)                                                             *
C*  A. Miyamoto  15-Aug-1990   Original version.                       *
C*                                                                     *
C***********************************************************************
C
      REAL*8 FUNCTION   FUNC( X )
C
      IMPLICIT REAL*4 (A-B,D-H,O-Z)
      IMPLICIT COMPLEX*8  (C)
C
      REAL*8 X(25)
C
      INCLUDE 'WWZPRM.inc'
C
      REAL*4  PEMOM(0:3,2), PVMOM(0:3,2), PFMOM(0:3,4)
      REAL*4  PINI(0:3)
      REAL*4  ELAB(3,3)/1.,0.,0., 0.,1.,0., 0.,0.,1./
      REAL*4  EWM(3,3), EWP(3,3)
      COMPLEX*8  CAMPL(0:4), CVCUR(6,2), CSPE(6,2), CSTMP(6,2)
CC      REAL*8  XTMP(25)
      DATA    IFIRST/0/
      SAVE    IFIRST
CC    DATA    AME/0.51099906E-3/
      REAL*4  COUPLE(2,12)
      REAL*4  ABNCPL(4,2)
      INTEGER*4  NDMOD(2)
      INTEGER*4  NHDATA(2,2)/1,-1,  -1,1/
      DATA    FBGEV2/0.38927E+12/
      DATA   PI/3.14159265/
C
      PARAMETER  (AMW    = 80.0,
     >            AMZ    = 91.17,
     >            ALPHA  = 1./128.,
     >            COS2TW = (AMW/AMZ)**2,
     >            SIN2TW = 1.-COS2TW,
     >            ALPHAS = 0.12,
     >            ALPHA0 = 1./137.0359895)
C ... Variable for Higgs width calculation.
CC      REAL*4  AMZERO(11)/5*0., 150., 3*0., AMW, AMZ  /
      REAL*4  GCOUPL(3) / ALPHA, SIN2TW, ALPHAS /
      REAL*4  AM(4)/AMZ, 2.5, AMW, 2.0/
      REAL*4  VKMMTX(3, 3 )
      DATA    VKMMTX/ 0.975D0,   0.222D0,  0.010D0,
     >                0.222D0,   0.974D0,  0.043D0,
     >                0.010D0,   0.043D0,  0.999D0 /
      REAL*4   AMF(12)
      DATA AMF   /  1.E-10      ,   1.E-10    ,   1.E-10 ,
     .              0.5110034E-3, 105.65946E-3,   1.732E0,
     .              0.04E0      ,   1.5E0     , 150.0E0  ,
     .              0.04E0      ,   0.1E0     ,   4.7E0  /
      REAL*4  WIDTHW(0:12), GWWP(2), WIDTHZ(0:12)
      REAL*4  ZERO/0.E0/, ONE/1.E0/
C
      INTEGER*4  IDF(2,12)/ 1,4,  2,5,  3,6,   10,7, 11,7, 12,7,
     >                      10,8, 11,8, 12,8,  10,9, 11,9, 12,9 /

C
C =====< Entry Point >===============================================
C
C
C ----------------------------------------------------------------------
C (1) Initialization at the very begining of the job.
C ----------------------------------------------------------------------
C
      IF( IFIRST .EQ. 0 ) THEN
        SW2 = SIN2TW
        CALL COUPLA(SW2, COUPLE(1,1),  COUPLE(1,2),  COUPLE(1,3),
     >                   COUPLE(1,4),  COUPLE(1,5),  COUPLE(1,6),
     >                   COUPLE(1,7),  COUPLE(1,8),  COUPLE(1,9),
     >                   COUPLE(1,10), COUPLE(1,11), COUPLE(1,12) )
        CALL WIDZ( 6, AM(1), GCOUPL, AMF(4),WIDTHZ)
        AWZ = WIDTHZ(0)
        CALL WIDW( 6, AM(3), GCOUPL, AMF(4),VKMMTX, WIDTHW)
        AWW = WIDTHW(0)
        AME   = AMF(4)
        AM(2) = AWZ
        AM(4) = AWW
 
        IFIRST = 1
c        print *,'initialization is completed'
      ENDIF
C
C ----------------------------------------------------------------------
C (2) Set Beam energy and flux factor.
C ----------------------------------------------------------------------
C
      FUNC   = 0
      EB     = EBEAM
      S      = (2.*EB)**2
      EMINS  = EBEAM
      EPLUS  = EBEAM
C
C  Beam strulung.
C
      IF( NBSON .EQ. 1 .OR. NBSON .EQ. 3 ) THEN
        CALL EPROBX( X(8), NBTYPE, EMINS )
        CALL EPROBX( X(9), NBTYPE, EPLUS )
        EMINS = EBEAM*EMINS
        EPLUS = EBEAM*EPLUS
        S    =  4.*EMINS*EPLUS
        EB   = 0.5*SQRT(S)
        IF( 2.*EB .LE. AM(1) ) RETURN
      ENDIF
C
C  Bremstrulung.
C
      RSCOL   = 2.*EB
      IF( NBSON .EQ. 2 .OR. NBSON .EQ. 3 ) THEN
        BEEE = 2.*ALPHA0/PI*( ALOG(S/(AME*AME)) - 1. )
        BEEINV = 1./BEEE
        S      = RSCOL*RSCOL
        SCOL   = (1. - X(7)**BEEINV)*S
        RSCOL  = SQRT(SCOL)
      ENDIF
 
      IF( RSCOL .LE. AM(1) ) RETURN
 
      EBCOL  = RSCOL*0.5
 
 
CCUT        IF( EBCOL .GT. 0.9*EBEAM ) RETURN
 
 
      SCOL   = RSCOL*RSCOL
      BETAE  = SQRT(1.-4.*AME**2/SCOL)
      FLUX   = 1./(2.*BETAE*SCOL)
      SPNAVG = 1./4.
      STAT   = 1.
      COLOR  = 1.
      PHASE  = 1.
      PHSPIN = FLUX*SPNAVG*STAT*PHASE*FBGEV2*COLOR
C
C ----------------------------------------------------------------------
C (3) Kinematics.
C ----------------------------------------------------------------------
C
      CALL MOMNTX(EBCOL,AME,ONE,ZERO , PEMOM(0,1))
      CALL PMIRRX(PEMOM(0,1) , PEMOM(0,2))
C
C .. Decide decay mode of W-,W+
C
      CALL UHDMOD( X(13), 12, WIDTHW, NDMOD(1))
      CALL UHDMOD( X(14), 12, WIDTHW, NDMOD(2))
      BRPROD = WIDTHW(0)*WIDTHW(0)/(WIDTHW(NDMOD(1))*WIDTHW(NDMOD(2)))
            
C
C ... W-
      QMIN = 10.
      QMAX = RSCOL - 10.
CC    QMIN = AM(3) - 5.*AM(4)
CC    QMAX = AM(3) + 5.*AM(4)
      CALL UHPHMM(X(2), QMAX, QMIN, AM(3), AM(4), QWM, PHMWM )
      IF( PHMWM .LE. 0. ) RETURN
C ... W+
      QMIN = 10.
      QMAX = RSCOL - 10. - QWM
CC    QMIN = AM(3) - 5.*AM(4)
CC    QMAX = AM(3) + 5.*AM(4)
      CALL UHPHMM(X(3), QMAX, QMIN, AM(3), AM(4), QWP, PHMWP )
      IF( PHMWP .LE. 0. ) RETURN
C
      PINI(0) = -RSCOL
      PINI(1) = 0.
      PINI(2) = 0.
      PINI(3) = 0.
C
      CALL UHPH2B( X(4), X(10), PINI, ELAB, RSCOL, QWM, QWP,
     >             PVMOM(0,1), PVMOM(0,2), PH1 )
C     PRINT *,' PH1=',PH1
      IF( PH1 .LE. 0. ) RETURN
      CALL UHSETF( PVMOM(1,1), ELAB, EWM )
      CALL UHPH2B( X(5), X(11), PVMOM(0,1), EWM, QWM, ZERO, ZERO,
     >             PFMOM(0,1),  PFMOM(0,2), PHWM)
C     PRINT *,' PHWM=',PHWM
      IF( PHWM .LE. 0. ) RETURN
      CALL UHSETF( PVMOM(1,2), ELAB, EWP )
      CALL UHPH2B( X(6), X(12), PVMOM(0,2), EWP, QWP, ZERO, ZERO,
     >             PFMOM(0,3),  PFMOM(0,4), PHWP)
C     PRINT *,' PHWP=',PHWP
      IF( PHWP .LE. 0. ) RETURN
      PHSPOT = PHMWP*PHMWM*PH1*PHWM*PHWP*BRPROD
 
C
C -------------------------------------------------------------------
C (4) We fix helicities.
C -------------------------------------------------------------------
C
      XNHEL = 2.
      NHI   = 1
      IF( X(1) .GT. 0.5D0 ) NHI = 2
      NHEM  = NHDATA(1,NHI)
      NHEP  = NHDATA(2,NHI)
 
C
C -------------------------------------------------------------------
C (5) Spinor of initial state
C -------------------------------------------------------------------
C
      CALL IXXXXX(PEMOM(0,1),AME,NHEM,+1,CSPE(1,1))
      CALL OXXXXX(PEMOM(0,2),AME,NHEP,-1,CSPE(1,2))
C
C -------------------------------------------------------------------
C (6) Spinor and polarization vector of final state.
C -------------------------------------------------------------------
C
C ..   W- --> f and fbar
C              1      2
C
      XCOLOR = 1.0
      CALL OXXXXX(PFMOM(0,1),AMF(IDF(1,NDMOD(1))),-1,+1,CSTMP(1,1))
      CALL IXXXXX(PFMOM(0,2),AMF(IDF(2,NDMOD(1))),+1,-1,CSTMP(1,2))
      IF( NDMOD(1) .LE. 3 ) THEN
        GWWP(1) = COUPLE(1,4)
        GWWP(2) = COUPLE(2,4)
      ELSE
        IUP = (NDMOD(1) - 1)/3
        IDN =  NDMOD(1) - 3*IUP
        GWWP(1) = VKMMTX(IUP,IDN)*COUPLE(1,4)
        GWWP(2) = VKMMTX(IUP,IDN)*COUPLE(2,4)
        XCOLOR  = XCOLOR*3.*(1.+ALPHAS/PI)
      ENDIF
      CALL JIOXXX(CSTMP(1,2),CSTMP(1,1),GWWP,AMW,AWW,CVCUR(1,1))
C
C ..   W+ --> f and fbar
C              3      4
C
      CALL IXXXXX(PFMOM(0,3),AMF(IDF(1,NDMOD(2))),+1,-1,CSTMP(1,1))
      CALL OXXXXX(PFMOM(0,4),AMF(IDF(2,NDMOD(2))),-1,+1,CSTMP(1,2))
      IF( NDMOD(2) .LE. 3 ) THEN
        GWWP(1) = COUPLE(1,4)
        GWWP(2) = COUPLE(2,4)
      ELSE
        IUP = (NDMOD(2) - 1)/3
        IDN =  NDMOD(2) - 3*IUP
        GWWP(1) = VKMMTX(IUP,IDN)*COUPLE(1,4)
        GWWP(2) = VKMMTX(IUP,IDN)*COUPLE(2,4)
        XCOLOR  = XCOLOR*3.*(1.+ALPHAS/PI)
      ENDIF
      CALL JIOXXX(CSTMP(1,1),CSTMP(1,2),GWWP,AMW,AWW,CVCUR(1,2))
C
C ----------------------------------------------------------------------
C (6) Amplitude for e e --> W+ W-
C ----------------------------------------------------------------------
C
      CALL WWAMP(AM, PEMOM, CSPE, PVMOM, CVCUR, 0, ABNCPL, CAMPL )
C
C ----------------------------------------------------------------------
C (7) Cross section.
C ----------------------------------------------------------------------
C
      FUNC   = ABS(CAMPL(0))**2*PHSPIN*PHSPOT*XNHEL*XCOLOR
C
C ----------------------------------------------------------------------
C (7.2) Store variables for event generation.
C ----------------------------------------------------------------------
C
      DO 720 I = 0, 3
        DO 730 J = 1, 2
          PGEN(I,J) = PVMOM(I,J)
730     CONTINUE
        DO 740 J = 1, 4
          PGEN(I,J+2) = PFMOM(I,J)
740     CONTINUE
720   CONTINUE
      NGDCY(1) = NDMOD(1)
      NGDCY(2) = NDMOD(2)
      DO 750 I = 1, 14
        XG(I) = X(I)
750   CONTINUE

C
C ---------------------------------------------------------------------
C (8) Histograming.
C ---------------------------------------------------------------------
C
      DO 800 I = 1, 14
        CALL XHFILL(I,X(I),FUNC)
800   CONTINUE
 
      CALL XHFILL( 15,DFLOAT(NDMOD(1)), FUNC )
      CALL XHFILL( 16,DFLOAT(NDMOD(2)), FUNC )
      XMWM = UH4MAS( PFMOM(0,1), PFMOM(0,2) )
      XMWP = UH4MAS( PFMOM(0,3), PFMOM(0,4) )
      CALL XHFILL( 17, DBLE(XMWM), FUNC )
      CALL XHFILL( 18, DBLE(XMWP), FUNC )
      XRSCOL = RSCOL/EBEAM*0.5
      CALL XHFILL( 19,DBLE(XRSCOL), FUNC )

 
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*===================                                                  *
C* Subroutine USROUT                                                   *
C*===================                                                  *
C*                                                                     *
C*(Purpose)                                                            *
C*   To printout the results of the integration.                       *
C*(Author)                                                             *
C*   Coded by A. Miyamoto  6-Sep-1989                                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE USROUT
C
      IMPLICIT REAL*4 (A-H,O-Z)
C
C
      INCLUDE  'WWZPRM.inc'
C
      REAL*8 AVGI, SD, CHI2A, STIME
      COMMON / BSRSLT / AVGI, SD, CHI2A, STIME, ITF
C
C =====< Entry Point >=================================================
C
C ---------------------------------------------------------------------
C (1) Printout the results of the integration.
C ---------------------------------------------------------------------
C
      WRITE( 6, 100 ) NBSON,NBTYPE,EBEAM,AVGI,SD,CHI2A,STIME,ITF
100   FORMAT(1H1,//,
     >      '   ee --> WW   ',/,
     >      '   NBSON (Beamsthulung)    =',I4,/,
     >      '   NBTYPE (BeamsthulunType)=',I4,/,
     >      '   Ebeam                   =',F10.3,' GeV',/,
     >      '   Total cross section(fb) =',F10.5,' +- ',F10.5,/,
     >      '   Chi-square/Iteration    =',G10.5,/,
     >      '   CPU time(sec)           =',F10.3,/,
     >      '   # of iteration.         =',I7)
C
C ... Dynamic allocation of output file.
C
C
      RETURN
      END
