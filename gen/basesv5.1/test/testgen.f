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
C
      COMMON/KINEM/W,EM,ZM,ZGAM,CZ,CV,CA,FACTOR,XK,COSTH
      REAL*4 P(4)
      REAL*8 WCM(6)
      DATA WCM / 40.D0, 60.D0, 70.D0, 105.D0, 150.D0, 260.D0 /
 
      DATA PI /3.1415926D0/
      DATA ALP /137.036D0/
      DATA GEVNB /0.38927D6/
      DATA GENER /3./
 
********************************************************************
*               Initialization of BASES/SPRING v5.1
********************************************************************
*=========================================================
*          Initialization of BASES by calling BSINIT
*=========================================================
*         -------------
           CALL BSINIT
*         -------------
*=========================================================
*          Read the probability information from the file
*=========================================================
           LUN = 23
           open(LUN,file='bases.data',status='old',form='unformatted')
*         -------------
           CALL BSREAD( LUN )
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
 
*=========================================================
*     Initialization of additional histograms for SPRING
*=========================================================
 
      CALL XHINIT( 5,  0.0D0, 4.0D1, 40,
     .                    'Photon Transverse Energy (GeV)')
 
*==================================================
*     Event generation by SPRING V5.1
*==================================================
 
      MXTRY  = 50
      MXEVNT = 10000
 
      DO 100 NEVNT = 1, MXEVNT
 
         CALL SPRING( FUNC, MXTRY )
 
*    ------------------------------------------------
*        Compute the four vectors of generated event
*          from the kinematical variables
*    ------------------------------------------------
 
         PHI   = TWOPI*DRN(DUMY)
         PXY   = XK*SQRT(1.0 - COSTH*COSTH)
         P(1)  = PXY*COS(PHI)
         P(2)  = PXY*SIN(PHI)
         P(3)  = XK*COSTH
         P(4)  = XK
 
C        WRITE(20) P
 
         CALL XHFILL( 5, PXY, 1.D0)
 
  100 CONTINUE
 
      LU     = 6
 
      CALL SPINFO( LU )
 
      CALL SHPLOT( LU )
 
      STOP
      END
