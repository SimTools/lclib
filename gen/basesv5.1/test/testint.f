C      Main Program for BASES50 and SPRING50.
C      Test program is coded by S.Kawabata on 4 April 1994
C       and the code for the process was developed by Y.Shimizu.
C ....... Lowest order cross-section for e+ e- --> nu-pair gamma
C                            Created  on 28 June 1984 by Y. Shimizu
C                            Modified on  7 FEBR 1985 by Y. Shimizu
C
      IMPLICIT REAL*8(A-H,O-Z)
      EXTERNAL FUNC
 
      PARAMETER (MXDIM = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2

C     DIMENSION XL(2), XU(2)
C
      COMMON/KINEM/W,EM,ZM,ZGAM,CZ,CV,CA,FACTOR,XK,COSTH
 
      REAL*8 WCM(6)
      DATA WCM / 40.D0, 60.D0, 70.D0, 105.D0, 150.D0, 260.D0 /
 
      DATA PI /3.1415926D0/
      DATA ALP /137.036D0/
      DATA GEVNB /0.38927D6/
      DATA GENER /3./
 
********************************************************************
*               Initialization of BASES v5.1
********************************************************************
*=========================================================
*          Initialization of BASES by calling BSINIT
*=========================================================
*         -------------
           CALL BSINIT
*         -------------
*=========================================================
*      Initialization of parameters
*          for kinematics and matrix elements
*=========================================================
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
 
           W      = WCM(4)
           XKMIN  = 1./W
 
*=========================================================
*      Initialization of BASES parameters
*=========================================================
 
         NDIM  = 2
         NWILD = 2
 
         NCALL = 3125
         ITMX1 = 10
         ACC1  = 0.1
         ITMX2 = 20
         ACC2  = 0.05
 
         XL(1)= XKMIN
         XU(1)= W/2.
         XL(2)= DCOS(COSMAX*RAD)
         XU(2)= DCOS(COSMIN*RAD)
*
*        CALL BSDIMS( NDIM, NWILD, XL, XU )
*
*        CALL BSPARM( NCALL, ACC1, ACC2, ITMX1, ITMX2 )
* 
*=========================================================
*      Initialization of Histograms
*=========================================================
*   For histogram
       CALL XHINIT(1,XL(1),XU(1),40,'Photon Energy (GeV)')
       CALL XHINIT(2,XL(2),XU(2),50,'Cos(theta) of Photon')
*   For scatter plot
       CALL DHINIT(1,XL(2),XU(2),50,XL(1),XU(1),50,
     .             ' x : cos(theta)  --  y : Photon Energy ')
 
********************************************************************
*              Nimerical Integration by BASES V5.1
********************************************************************
 
      CALL BASES( FUNC, ESTIM, SIGMA, CHI2, CTIME, IT1, IT2 )
 
      LU     = 6
 
      CALL BSINFO( LU )
 
      CALL BHPLOT( LU )
 
 
      LN  = 20
      open(LN,file='bases.hist',status='unknown',form='formatted')
 
      CALL XHSAVE( LN, 1 )
      CALL XHSAVE( LN, 2 )
*==================================================
*     Save the probability information to the file
*==================================================
 
      LUN = 23
      open(LUN,file='bases.data',status='unknown',form='unformatted')
 
      CALL BSWRIT( LUN )
 
      STOP
      END
