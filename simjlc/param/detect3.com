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
@!                                                                              
@! Tracking device parameters.                                                  
@!                                                                              
  1    314159 ! seed for smearing                                       00005000
  2    20.    ! B field (kG) ( dummy if Swimmer specifies B )           00005000
@!                                                                              
@! Tracking device parameters.                                                  
@!                                                                              
 11    30.    ! inner radius(cm)                                        00005000
 12   200.    ! outer radius(cm)                                        00005000
 13  -200.    ! Z-(cm)                                                  00005000
 14   200.    ! Z+(cm)                                                  00005000
 15   100     ! # sampling points                                       00005000
 16    10     ! cut on # sampling points                                00005000
 17     1.E-2 ! sigma_(r-phi)                                           00005000
 18     5.E-2 ! sigma_Z                                                 00005000
@!                                                                              
@! EM_calorimeter parameters.                                                   
@!                                                                              
 31   450    ! # phi segments                                           00005000
 32   171    ! # theta segments of barrel                               00005000
 33    59    ! # radial segments of endcap                              00005000
 34    45.   ! inner radius(cm)                                         00005000
 35   250.   ! outer radius(cm)                                         00005000
 36  -300.   ! Z-(cm)                                                   00005000
 37   300.   ! Z+(cm)                                                   00005000
 38   2.E-2  ! sigma_E/E pedestal for barrel                            00005000
 39  1.5E-1  ! sigma_E/E at 1 GeV                                       00005000
 40   2.E-2  ! sigma_E/E pedestal for endcap                            00005000
 41  1.5E-1  ! sigma_E/E at 1 GeV                                       00005000
@!                                                                              
@! HD_calorimeter parameters.                                                   
@!                                                                              
 51   128    ! # phi segments                                           00005000
 52    45    ! # theta segments of barrel                               00005000
 53    17    ! # radial segments of endcap                              00005000
 54    60.   ! inner radius(cm)                                         00005000
 55   350.   ! outer radius(cm)                                         00005000
 56  -400.   ! Z-(cm)                                                   00005000
 57   400.   ! Z+(CM)                                                   00005000
 58    2.E-2 ! sigma_E/E pedestal for barrel                            00005000
 59    4.E-1 ! sigma_E/E at 1 GeV                                       00005000
 60    2.E-2 ! sigma_E/E pedestal for endcap                            00005000
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
 87  -200.   ! Z-(cm)                                                   00005000
 88   200.   ! Z+(cm)                                                   00005000
 89   0.01   ! thickness in radiation length                            00005000
