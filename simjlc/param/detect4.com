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
@!                                                                              
@! Tracking device parameters.                                                  
@!                                                                              
  1    314159 ! seed for smearing                                       00005000
  2    20.    ! B field (kG) ( dummy if Swimmer specifies B )           00005000
@!                                                                              
@! Tracking device parameters.                                                  
@!                                                                              
 11    30.    ! inner radius(cm)                                        00005000
 12   230.    ! outer radius(cm)                                        00005000
 13  -230.    ! Z-(cm)                                                  00005000
 14   230.    ! Z+(cm)                                                  00005000
 15   100     ! # sampling points                                       00005000
 16    10     ! cut on # sampling points                                00005000
 17     1.E-2 ! sigma_(r-phi)                                           00005000
 18     3.E-1 ! sigma_Z                                                 00005000
@!                                                                              
@! EM_calorimeter parameters.                                                   
@!                                                                              
 31   156    ! # phi segments                                           00005000
 32    50    ! # theta segments of barrel                               00005000
 33    18    ! # radial segments of endcap                              00005000
 34    45.   ! inner radius(cm)                                         00005000
 35   250.   ! outer radius(cm)                                         00005000
 36  -300.   ! Z-(cm)                                                   00005000
 37   300.   ! Z+(cm)                                                   00005000
 38   1.E-2  ! sigma_E/E pedestal for barrel                            00005000
 39  1.5E-1  ! sigma_E/E at 1 GeV                                       00005000
 40   1.E-2  ! sigma_E/E pedestal for endcap                            00005000
 41  1.5E-1  ! sigma_E/E at 1 GeV                                       00005000
@!                                                                              
@! HD_calorimeter parameters.                                                   
@!                                                                              
 51    78    ! # phi segments                                           00005000
 52    25    ! # theta segments of barrel                               00005000
 53     9    ! # radial segments of endcap                              00005000
 54    48.   ! inner radius(cm)                                         00005000
 55   270.   ! outer radius(cm)                                         00005000
 56  -324.   ! Z-(cm)                                                   00005000
 57   324.   ! Z+(CM)                                                   00005000
 58    1.E-2 ! sigma_E/E pedestal for barrel                            00005000
 59    4.E-1 ! sigma_E/E at 1 GeV                                       00005000
 60    1.E-2 ! sigma_E/E pedestal for endcap                            00005000
 61    4.E-1 ! sigma_E/E at 1 GeV                                       00005000
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
@!                                                                              
 71     3    ! # sampling layers + 1 = NSMPVX                           00005000
 72   25.E-4 ! phi pitch (cm)                                           00005000
 73   25.E-4 ! Z   pitch (cm)                                           00005000
@! layer #0 (beampipe)                                                          
 74     1.   ! radius (cm)                                              00005000
 75  -100.   ! Z-(cm)                                                   00005000
 76   100.   ! Z+(cm)                                                   00005000
 77   0.003  ! thickness in radiation length                            00005000
@! layer #1                                                                     
 78     2.5  ! radius (cm)                                              00005000
 79    -7.5  ! Z-(cm)                                                   00005000
 80     7.5  ! Z+(cm)                                                   00005000
 81   0.003  ! thickness in radiation length                            00005000
@! layer #2                                                                     
 82     7.5  ! radius (cm)                                              00005000
 83   -22.5  ! Z-(cm)                                                   00005000
 84    22.5  ! Z+(cm)                                                   00005000
 85   0.003  ! thickness in radiation length                            00005000
@!          .........                                                           
@!          .........                                                           
@!          .........                                                           
@! layer #NSMPVX (CDC inner cylinder)                                           
 86    25.   ! radius (cm)                                              00005000
 87  -230.   ! Z-(cm)                                                   00005000
 88   230.   ! Z+(cm)                                                   00005000
 89   0.01   ! thickness in radiation length                            00005000
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
