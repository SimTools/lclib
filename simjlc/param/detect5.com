@!
@!(Description)
@!  This is a sample input stream for the JLC quick detector simulator.
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
@! Tracking device parameters.
@!
  1    314159 ! seed for smearing
  2    20.    ! B field (kG) ( dummy if Swimmer specifies B )

@!
@! Tracking device parameters.
@!
 11    30.    ! inner radius(cm)
 12   230.    ! outer radius(cm)
 13  -230.    ! Z-(cm)
 14   230.    ! Z+(cm)
 15   100     ! # sampling points
 16    10     ! cut on # sampling points
 17     1.E-2 ! sigma_(r-phi)
 18     3.E-1 ! sigma_Z
@!
@! EM_calorimeter parameters.
@!
 31   156    ! # phi segments
 32    50    ! # theta segments of barrel
 33    18    ! # radial segments of endcap
 34    45.   ! inner radius(cm)
 35   250.   ! outer radius(cm)
 36  -300.   ! Z-(cm)
 37   300.   ! Z+(cm)
 38   1.E-2  ! sigma_E/E pedestal for barrel
 39  1.5E-1  ! sigma_E/E at 1 GeV
 40   1.E-2  ! sigma_E/E pedestal for endcap
 41  1.5E-1  ! sigma_E/E at 1 GeV
@!
@! HD_calorimeter parameters.
@!
 51    78    ! # phi segments
 52    25    ! # theta segments of barrel
 53     9    ! # radial segments of endcap
 54    48.   ! inner radius(cm)
 55   270.   ! outer radius(cm)
 56  -324.   ! Z-(cm)
 57   324.   ! Z+(CM)
 58    1.E-2 ! sigma_E/E pedestal for barrel
 59    4.E-1 ! sigma_E/E at 1 GeV
 60    1.E-2 ! sigma_E/E pedestal for endcap
 61    4.E-1 ! sigma_E/E at 1 GeV
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
@!
 70     2    ! NERRVX Type of parameter format
 71     5    ! # sampling layers + 1 = NSMPVX
 72   25.E-4 ! phi pitch (cm)
 73   25.E-4 ! Z   pitch (cm)
@! layer #0 (beampipe) 
 74     1.   ! radius (cm)
 75  -100.   ! Z-(cm)
 76   100.   ! Z+(cm)
 77   0.003  ! thickness in radiation length
@! layer #1
 78     2.4  ! radius (cm)
 79    -5.0  ! Z-(cm)
 80     5.0  ! Z+(cm)
 81   0.003  ! thickness in radiation length
@! layer #2 
 82     3.6  ! radius (cm)
 83    -7.5  ! Z-(cm)
 84     7.5  ! Z+(cm)
 85   0.003  ! thickness in radiation length
@! layer #3
 86     4.8  ! radius (cm)
 87   -10.0  ! Z-(cm)
 88    10.0  ! Z+(cm)
 89   0.003  ! thickness in radiation length
@! layer #4
 90     6.0  ! radius (cm)
 91   -12.5  ! Z-(cm)
 92    12.5  ! Z+(cm)
 93   0.003  ! thickness in radiation length
@!          .........
@!          .........
@!          .........
@! layer #NSMPVX (CDC inner cylinder)
 94    25.   ! radius (cm)
 95  -230.   ! Z-(cm)
 96   230.   ! Z+(cm)
 97   0.01   ! thickness in radiation length
@!
@! Words following the section of CCD inner cylinder is specially
@! to specify VTX space point resolution.  They are valid when NERRVX=2
 98   4.0E-4 ! r-phi and Z resolution (sigma)
@!
@!
@!
@!          .........                                                          
@!  Parameters for Clustering.                                                 
@!          .........
1001  100.   ! Eth(MeV) for barrel EMC
1002   0.4   ! C1 for barrel EMC
1003   0.2   ! C2 for barrel EMC
1004   1.0   ! C3 for barrel EMC
1005   0.3   ! Alpha (to determine poistion) of EMC
1006  100.   ! Eth(MeV) for Endcal EMC
1007   0.4   ! C1 for Endcap EMC
1008   0.2   ! C2 for endCap EMC
1009   1.0   ! C3 for Endcal EMC
1010   0.3   ! Alpha (to determine poistion) of HDC
1011  100.   ! Eth(MeV) for barrel HDC
1012   0.4   ! C1 for barrel HDC
1013   0.2   ! C2 for barrel HDC
1014   1.0   ! C3 for barrel HDC
1015   0.25  ! Alpha (to determine poistion) of HDC
1016  100.   ! Eth(MeV) for Endcap HDC
1017   0.4   ! C1 for Endcap HDC
1018   0.2   ! C2 for Endcap HDC
1019   1.0   ! C3 for Endcap HDC
1020   0.25  ! Alpha (to determine poistion) of HDC
