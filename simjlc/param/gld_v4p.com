@!
@!(Description)
@!  This is a sample input stream for the QuickSim
@!  Major detector parameters of this file are as follows.
@!    Beam Pipe: R=1.5cm, Be
@!               Thickness 0.0007 rad. length
@!    VTX  : 3 layer at r=1.9, 4.1, 6.3 cm  (2.2cm spacing)
@!           | cosTheta| < 0.90
@!           sigma_rphi=sigma_z=2 micron
@!           thickness = 0.0021 rad. length/layer
@!              ( Note: To mimic three double layers of FPCCD, 3 layers of VTX
@!                      with 200 micon m thickness , 2 micron resolution was assumed )
@!    IT   : 4 layers , 8.5 cm spacing, at r=9.3, 17.8, 26.3, 34.8 cm
@!           | cosTheta| < 0.90
@!           thickness = 0.006 rad. length/layer
@!           sigma_rphi=sigma_z=10 micron
@!    Matterial between IT and TPC : 0.013 rad. length
@!    TPC  : 40 cm < r < 206 cm,  200 sampling    
@!           -235.0cm < z < 235.0 cm
@!           sigma-rphi= 150 micron m
@!           sigma-z   = 1 mm
@!   Calorimeter assumes 2cm cell size
@!    ECL  : dE/E      = 15%/Sqrt(E) + 1% 
@!           R=210cm, -280 cm < Z < 280cm   (Barrel)
@!           40cm<R<210cm at Z=+-280cm      (EndCap)
@!           # Segment in phi = 660
@!           # Segment in theta, barrel = 230
@!           # Segment in theta, end cap = 87
@!    HDC  : dE/E      = 40%/Sqrt(E) + 1%
@!           R=230cm, -300 cm < Z < 300 cm  (Barrel)
@!           40cm<R<230cm at Z=300cm        (EndCap)
@!           # Segment in phi = 220
@!           # Segment in theta, barrel = 86
@!           # Segment in theta, end cap= 29
@!    Magnet : 3 tesla
@!
@!(Update Record)
@!  91/02/18  K.Fujii          This version contains parameters for
@!                             vertex detector. The calorimeter
@!                             parameters are consistent with the
@!                             default clustering parameters.
@!                             Those who wish to modify calorimeter
@!                             parameters should tune the clustering
@!                             parameters for yourself. Ask TAYM for
@!                             details of the tunig.
@!  91/03/06  K.Fujii          New detector parameters.
@!
@!  92/07/01  A.Miyamoto       Parameter set for Proposed Detector.
@!  98/12/25  A.Miyamoto       Parameter set for 4 layer VTX,
@!                             by Sugimoto-san's recomendation
@!  99/03/30  A.Miyamoto       If NERRVX=3, create smeared VTX;Track_parameter
@!                             If NUMVTX>NSMPVX+1, Hits of Inner tracker is 
@!                             created, with a spacial resolution different 
@!                             from VTX.  See comments below about how to set
@!                             parameters for Inner Tracker.
@!  99/04/06  A.Miyamoto       detect6.com is revised to have consistency 
@!                             with JIM and Background study
@!  99/05/07  A.Miyamoto       Thick ness of 1st VTX layer is changes from 0.0015 to 0.003
@!2000/01/25  A.Miyamoto       Parameter set for 3T solenoid, with Intermideate tracker
@!2005/02/13  A.Miyamoto       Parameter set for 3T solenoid, for GLD
@!
@! $Id$
@!
@! Tracking device parameters.
@!
  1    314159 ! seed for smearing
  2    30.    ! B field (kG) ( dummy if Swimmer specifies B )
@!
@!
@! Tracking device parameters.
@!
 11    40.    ! inner radius(cm)
 12   205.    ! outer radius(cm)
 13  -235.    ! Z-(cm)
 14   235.    ! Z+(cm)
 15   200     ! # sampling points
 16    10     ! cut on # sampling points
 17   1.50E-2 ! sigma_(r-phi)
 18   0.4E-1 ! sigma_Z
@!
@! EM_calorimeter parameters.
@!
@! 31   660    ! # phi segments
@! 32   230    ! # theta segments of barrel
@! 33    87    ! # radial segments of endcap
 31   220    ! # phi segments
 32    77    ! # theta segments of barrel
 33    29    ! # radial segments of endcap
 34    40.   ! inner radius(cm)
 35   210.   ! outer radius(cm)
 36  -280.   ! Z-(cm)
 37   280.   ! Z+(cm)
 38   1.E-2  ! sigma_E/E pedestal for barrel
 39   1.7E-1  ! sigma_E/E at 1 GeV
 40   1.E-2  ! sigma_E/E pedestal for endcap
 41   1.7E-1  ! sigma_E/E at 1 GeV
@! 38   1.E-2  ! sigma_E/E pedestal for barrel
@! 39   1.2E-1  ! sigma_E/E at 1 GeV
@! 40   1.E-2  ! sigma_E/E pedestal for endcap
@! 41   1.2E-1  ! sigma_E/E at 1 GeV
@!
@! HD_calorimeter parameters.
@!
@! 51   220    ! # phi segments
@! 52    86    ! # theta segments of barrel
@! 53    29    ! # radial segments of endcap
 51    74    ! # phi segments
 52    29    ! # theta segments of barrel
 53    10    ! # radial segments of endcap
 54    40.   ! inner radius(cm)
 55   230.   ! outer radius(cm)
 56  -300.   ! Z-(cm)
 57   300.   ! Z+(CM)
 58    2.E-2 ! sigma_E/E pedestal for barrel
 59    4.5E-1 ! sigma_E/E at 1 GeV
 60    2.E-2 ! sigma_E/E pedestal for endcap
 61    4.5E-1 ! sigma_E/E at 1 GeV
@! 58    2.E-2 ! sigma_E/E pedestal for barrel
@! 59    3.3E-1 ! sigma_E/E at 1 GeV
@! 60    2.E-2 ! sigma_E/E pedestal for endcap
@! 61    3.3E-1 ! sigma_E/E at 1 GeV
@!
@! Vertex detector parameters.
@!    NSMPVX specifies # of measuring layers. Layer #0 is beampipe
@!    and layer #NSMPVX is CDC inner cylinder. A negative NSMPVX
@!    skips VTX simulation.
@!
@! ***** Caution *******************************************************
@! *  Up to parameter #71 has a fixed format, while parameters #72     *
@! *  and after depend on NSMPVX. In the following, for instance,      *
@! *  parameters #86-89 are for the CDC inner cylinder since NSMPVX    *
@! *  is 3 here. If NSMPVX is 4, parameters #86-89 are for 3-rd VTX    *
@! *  layer and the CDC inner cylinder parameters becom #90-93.        *
@! *********************************************************************
@! *  24-Dec-1998 : Data format is changed to allow gaussian error.
@! *  When word #70 (NERRVX) is 1, VTX space point is digitized by the
@! *  size of pixel ( phi and z pitch ).  If NERRVX is 2, VTX Space
@! *  point is smeared by gaussian, whose standard deviation is given by 
@! *  the word following CDC inner cylynder section. 
@! *********************************************************************
@! *  29-March-1999 : Add NERRVTX=3 format.
@! *  # of sampling layer is a sum of sampling layers of VTX and that of 
@! *  inner tracker.  Number of VTX layer must be greater than 2, since
@! *  they are used to create smeared VTX:Track_Parameters.
@! *  If NSMPVX+1 > NUMVTX, hit points for Inner Tracker are created and
@! *  stored in the bank, Production:IT;Hit_Points.
@! *  Spacing between VTX sampling layer must be equal since it is assumed
@! *  in the routine to create VTX Smeared track.
@! *********************************************************************
@!
 70     3    ! NERRVX Type of parameter format
@! 71     8    ! # sampling layers + 1 = NSMPVX
 71     5    ! # sampling layers + 1 = NSMPVX
 72     4    ! NUMVTX Number of VTX layers ( those used for tracking)
 73    2.E-4 ! r-phi resolution (cm) of VTX
 74    2.E-4 ! Z   resolution (cm) of VTX
 75   10.E-4 ! r-phi resolution (cm) of Inner Tracker
 76   10.E-4 ! Z   resolution (cm) of Inner Tracker
@! layer #0 (beampipe) 
 77     1.5   ! radius (cm)
 78  -280.   ! Z-(cm)
 79   280.   ! Z+(cm)
 80   0.000007  ! thickness in radiation length
@! 80   0.0007  ! thickness in radiation length
@! layer #1
 81     1.6  ! radius (cm)
 82    -3.92  ! Z-(cm)
 83     3.92  ! Z+(cm)
 84   0.00002134  ! thickness in radiation length
@! 84   0.002134  ! thickness in radiation length
@! layer #2 
 85     4.1  ! radius (cm)
 86    -8.47  ! Z-(cm)
 87     8.47  ! Z+(cm)
 88   0.00002134  ! thickness in radiation leng5h
@! layer #3
 89     6.6  ! radius (cm)
 90   -13.01  ! Z-(cm)
 91    13.01  ! Z+(cm)
 92   0.00002134  ! thickness in radiation length
@! layer #4
 93     9.1  ! radius (cm)
 94   -18.56  ! Z-(cm)
 95    18.56  ! Z+(cm)
 96   0.00002134  ! thickness in radiation length
@!          .........
@!          .........
@! layer #NSMPVX (Matterial between TPC and IT)
 97    38.   ! radius (cm)
 98  -235.   ! Z-(cm)
 99   235.   ! Z+(cm)
100   0.026524   ! thickness in radiation length
@!          .........                                                          
@!  Parameters for Clustering.                                                 
@!          .........
1001  10.   ! Eth(MeV) for barrel EMC
1002   0.4   ! C1 for barrel EMC
1003   0.2   ! C2 for barrel EMC
1004   1.0   ! C3 for barrel EMC
1005   0.3   ! Alpha (to determine poistion) of EMC
1006  10.   ! Eth(MeV) for Endcal EMC
1007   0.4   ! C1 for Endcap EMC
1008   0.2   ! C2 for endCap EMC
1009   1.0   ! C3 for Endcal EMC
1010   0.3   ! Alpha (to determine poistion) of HDC
1011  10.   ! Eth(MeV) for barrel HDC
1012   0.4   ! C1 for barrel HDC
1013   0.2   ! C2 for barrel HDC
1014   1.0   ! C3 for barrel HDC
1015   0.25  ! Alpha (to determine poistion) of HDC
1016  10.   ! Eth(MeV) for Endcap HDC
1017   0.4   ! C1 for Endcap HDC
1018   0.2   ! C2 for Endcap HDC
1019   1.0   ! C3 for Endcap HDC
1020   0.25  ! Alpha (to determine poistion) of HDC
@!
@! Parameters to define lateral spread of shower in cal.
@!  Initialize a lateral shower shape function:
@!    f(x) = ANM(1,i)*EXP(-!x!/ALM(1,i)) + ANM(2,i)*EXP(-!x!/ALM(2,i))
@!                  core part                 peripheral part
@! where i = (1,2) = (EM,HD).
1101   0.055424   ! ANM(1,EM)
1102   0.395883   ! ANM(2,EM)
1103   0.45       ! ALM(1,EM)
1104   1.20       ! ALM(2,EM)
1111   0.0877193  ! ANM(1,HD)
1112   0.043860   ! ANM(2,HD)
1113   2.20       ! ALM(1,HD)
1114   7.00       ! ALM(2,HD)
@!
@! Chamber gas thickness per cm in terms of radiation length
@! 2001 0.000030 ! 0.000030 is for gld configuration
2001 0.000022 ! 0.000030 is for gld configuration
