C      Main Program for BASES/SPRING V5.1
C
      IMPLICIT REAL*8(A-H,O-Z)
      EXTERNAL FUNC
 
      PARAMETER (MXDIM = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
C
      COMMON/KINEM/W,EM,ZM,ZGAM,CZ,CV,CA,FACTOR,XK,COSTH
      REAL*4 P(4)
      DATA PI,ALP,GEVNB,GENER /
     .     3.1415926D0, 137.036D0, 0.38927D6, 3.0D0 /
 
********************************************************************
*     Initialization of BASES/SPRING V5.1
********************************************************************
*===> Initialization of BASES by calling BSINIT
 
           CALL BSINIT
 
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
 
          CALL BASES( FUNC, ESTIM, SIGMA, CTIME, IT1, IT2 )
 
          LU     = 6
 
          CALL BSINFO( LU )
 
          CALL BHPLOT( LU )
 
********************************************************************
*     Event generation by SPRING V5.1
********************************************************************
*===> Initialization of additional histograms
 
          CALL XHINIT( 5,  0.0D0, 4.0D1, 40,
     .                     'Photon Transverse Energy (GeV)')
 
*===> Event generation loop
 
          MXTRY  = 50
          MXEVNT = 10000
 
          DO 100 NEVNT = 1, MXEVNT
 
             CALL SPRING( FUNC, MXTRY )
 
*           Compute the four vectors of generated event
*           from the kinematical variables
 
             PHI   = TWOPI*DRN(DUMY)
             PXY   = XK*SQRT(1.0 - COSTH*COSTH)
             P(1)  = PXY*COS(PHI)
             P(2)  = PXY*SIN(PHI)
             P(3)  = XK*COSTH
             P(4)  = XK
 
C            WRITE(20) P
 
             CALL XHFILL( 5, PXY, 1.D0)
 
  100     CONTINUE
 
          CALL SPINFO( LU )
 
          CALL SHPLOT( LU )
 
      STOP
      END
