C* (Contents)
C*    NSMPVX        :(I*4):  #sampling points + 1.
C*    DPHIVX        :(R*4):  phi width.
C*    DZEEVX        :(R*4):  z   width.
C*    RCYVX(  LYR)  :(R*4):  r of layer #LYR.
C*    ZCYVX(1,LYR)  :(R*4): -z of layer #LYR.
C*         (2,LYR)  :(R*4): +z of layer #LYR.
C*    RDLVX(  LYR)  :(R*4):  radiation length of layer #LYR.
C* (Update Record)
C*    1/30/91  K.Fujii       Original version.
C*                           LYR = 0      : beampipe.
C*                               = ....   : measuring layers.
C*                               = NSMPVX : CDC inner cylinder.
C*                           A negative NSMPVX skips VTX smearing.
C*
      PARAMETER      ( MXxVXL = 10 )
      COMMON /SMRVGO/ NSMPVX, DPHIVX, DZEEVX, RCYVX(0:MXxVXL),
     .                ZCYVX(2,0:MXxVXL), RDLVX(0:MXxVXL)
      INTEGER*4       NSMPVX
      REAL   *4       DPHIVX, DZEEVX, RCYVX, ZCYVX, RDLVX
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
