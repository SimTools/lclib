C* (Contents)
C*    NSMPVX        :(I*4):  #sampling points + 1.
C*    DPHIVX        :(R*4):  phi width.
C*    DZEEVX        :(R*4):  z   width.
C*                          DPHIVX and DZEEVX is valid only when NERRVX=1
C*    RCYVX(  LYR)  :(R*4):  r of layer #LYR.
C*    ZCYVX(1,LYR)  :(R*4): -z of layer #LYR.
C*         (2,LYR)  :(R*4): +z of layer #LYR.
C*    RDLVX(  LYR)  :(R*4):  radiation length of layer #LYR.
C*
C*    NERRVX        :(I*4): =1 error due to pixel size digitization
C*                          =2 put single gaussian error.
C*                          =3 produce Smeared VTX track parameter and hit points of 
C*                             Inner tracker when necessary
C*    ERRVX(1)      :(R*4): r/z resolution ( when NERRVX=2)
C*         (2:5)         : reserved for future use.
C*         When NERRVX=3, ERRVX(1:2) is r-phi and Z resolution of VTX
C*                        ERRVX(3:4) is r-phi and Z resolution of Inner tracker
C*                            
C*    NUMVTX        : Number of VTX layer to be used for tracking
C*    
C* (Update Record)
C*    1/30/91  K.Fujii       Original version.
C*                           LYR = 0      : beampipe.
C*                               = ....   : measuring layers.
C*                               = NSMPVX : CDC inner cylinder.
C*                           A negative NSMPVX skips VTX smearing.
C*  12/24/98   A.Miyamoto    Put gaussian resolution for each pixel. 
C*   3/29/99   A.Miyamoto    Extend to create Smeared VTX Track parameter and
C*                           hit points for Inner Tracker
C*
C*      
C*  
      PARAMETER      ( MXxVXL = 22 )
      COMMON /SMRVGO/ NSMPVX, DPHIVX, DZEEVX, RCYVX(0:MXxVXL),
     .                ZCYVX(2,0:MXxVXL), RDLVX(0:MXxVXL),
     .                NERRVX, ERRVX(5), NUMVTX
      INTEGER*4       NSMPVX
      REAL   *4       DPHIVX, DZEEVX, RCYVX, ZCYVX, RDLVX
      INTEGER*4       NERRVX, NUMVTX
      REAL*4          ERRVX
C
CC=    DATA    NSMPVX /    3  /
C=    DATA    NSMPVX /    -3  /
C=    DATA    DPHIVX / 25.E-4 /
C=    DATA    DZEEVX / 25.E-4 /
C=    DATA    RCYVX  / 1.,  2.5,   7.5,  25., 7*0. /
C=    DATA    ZCYVX  / -100., 100.,
C=   .                   -7.5,   7.5,
C=   .                  -22.5,  22.5,
C=   .                 -230., 230.,
C=   .                  14*0      /
C=    DATA    RDLVX  / 0.003, 0.003, 0.003, 0.01, 7*0. /
C=    DATA    NERRVX /2/
C=    DATA    ERRVX  / 4.E-4, 4*0.0/





