      SUBROUTINE COUP1X(SW2 , GW,GWWA,GWWZ)
C
C This subroutine sets up the coupling constants of the gauge bosons in
C the STANDARD MODEL.
C
C INPUT:
C       real    SW2            : square of sine of the weak angle
C
C OUTPUT:
C       real    GW             : weak coupling constant
C       real    GWWA           : dimensionLESS coupling of W-,W+,A
C       real    GWWZ           : dimensionLESS coupling of W-,W+,Z
C
      REAL*8  SW2,GW,GWWA,GWWZ,ALPHA,FOURPI,EE,SW,CW
C
      ALPHA=1.D0/128.D0
C     ALPHA=1./REAL(137.0359895)
      FOURPI=4.D0*3.14159265358979323846D0
      EE=DSQRT(ALPHA*FOURPI)
      SW=DSQRT(SW2)
      CW=DSQRT(1.D0-SW2)
C
      GW    =  EE/SW
      GWWA  =  EE
      GWWZ  =  EE*CW/SW
C
      RETURN
      END
