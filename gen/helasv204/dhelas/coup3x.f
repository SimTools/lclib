      SUBROUTINE COUP3X(SW2,ZMASS,HMASS ,
     &                  GWWH,GZZH,GHHH,GWWHH,GZZHH,GHHHH)
C
C This subroutine sets up the coupling constants of the gauge bosons and
C Higgs boson in the STANDARD MODEL.
C
C INPUT:
C       real    SW2            : square of sine of the weak angle
C       real    ZMASS          : mass of Z
C       real    HMASS          : mass of Higgs
C
C OUTPUT:
C       real    GWWH           : dimensionFUL  coupling of W-,W+,H
C       real    GZZH           : dimensionFUL  coupling of Z, Z, H
C       real    GHHH           : dimensionFUL  coupling of H, H, H
C       real    GWWHH          : dimensionFUL  coupling of W-,W+,H, H
C       real    GZZHH          : dimensionFUL  coupling of Z, Z, H, H
C       real    GHHHH          : dimensionLESS coupling of H, H, H, H
C
      REAL*8  SW2,ZMASS,HMASS,GWWH,GZZH,GHHH,GWWHH,GZZHH,GHHHH,
     &        ALPHA,FOURPI,EE2,SC2,V
C
      ALPHA=1.D0/128.D0
C     ALPHA=1.D0/REAL(137.0359895)
      FOURPI=4.D0*3.14159265358979323846D0
      EE2=ALPHA*FOURPI
      SC2=SW2*(1.D0-SW2)
      V=2.D0*ZMASS*DSQRT(SC2)/SQRT(EE2)
C
      GWWH  =   EE2/SW2*0.5D0*V
      GZZH  =   EE2/SC2*0.5D0*V
      GHHH  =  -HMASS**2/V*3.D0
      GWWHH =   EE2/SW2*0.5D0
      GZZHH =   EE2/SC2*0.5D0
      GHHHH = -(HMASS/V)**2*3.D0
C
      RETURN
      END
