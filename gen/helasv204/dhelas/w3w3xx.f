      SUBROUTINE W3W3XX(WM,W31,WP,W32,G31,G32,WMASS,WWIDTH , VERTEX)
C
C This subroutine computes an amplitude of the four-point coupling of
C the W-, W+ and two W3/Z/A.  The amplitude includes the contributions
C of W exchange diagrams.  The internal W propagator is given in unitary
C gauge.  If one sets WMASS=0.0, then the gggg vertex is given (see sect
C 2.9.1 of the manual).
C
C INPUT:
C       complex WM(0:3)        : flow-OUT W-                         WM
C       complex W31(0:3)       : first    W3/Z/A                     W31
C       complex WP(0:3)        : flow-OUT W+                         WP
C       complex W32(0:3)       : second   W3/Z/A                     W32
C       real    G31            : coupling of W31 with W-/W+
C       real    G32            : coupling of W32 with W-/W+
C                                                  (see the table below)
C       real    WMASS          : mass  of W
C       real    WWIDTH         : width of W
C
C The possible sets of the inputs are as follows:
C   -------------------------------------------
C   |  WM  |  W31 |  WP  |  W32 |  G31 |  G32 |
C   -------------------------------------------
C   |  W-  |  W3  |  W+  |  W3  |  GW  |  GW  |
C   |  W-  |  W3  |  W+  |  Z   |  GW  | GWWZ |
C   |  W-  |  W3  |  W+  |  A   |  GW  | GWWA |
C   |  W-  |  Z   |  W+  |  Z   | GWWZ | GWWZ |
C   |  W-  |  Z   |  W+  |  A   | GWWZ | GWWA |
C   |  W-  |  A   |  W+  |  A   | GWWA | GWWA |
C   -------------------------------------------
C where all the bosons are defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex VERTEX         : amplitude          Gamma(WM,W31,WP,W32)
C
      COMPLEX*16 WM(6),W31(6),WP(6),W32(6),VERTEX
CCCCC COMPLEX*32 DV1(0:3),DV2(0:3),DV3(0:3),DV4(0:3),
      COMPLEX*16 DV1(0:3),DV2(0:3),DV3(0:3),DV4(0:3),
     &           J12(0:3),J34(0:3),J14(0:3),J32(0:3),DVERTX,
     &           SV1,SV2,SV3,SV4,TV1,TV2,TV3,TV4,DWS,DWT,
     &           V12,V13,V14,V23,V24,V34,JS12,JS34,JS14,JS32,JS,JT
      REAL*8     PWM(0:3),PW31(0:3),PWP(0:3),PW32(0:3),
     &           G31,G32,WMASS,WWIDTH
      REAL*8     Q(0:3),K(0:3),DP1(0:3),DP2(0:3),DP3(0:3),DP4(0:3),
     &           DMW,DWIDTH,DM2INV,S,T
C
      PWM(0)=DBLE( WM(5))
      PWM(1)=DBLE( WM(6))
      PWM(2)=DIMAG(WM(6))
      PWM(3)=DIMAG(WM(5))
      PWP(0)=DBLE( WP(5))
      PWP(1)=DBLE( WP(6))
      PWP(2)=DIMAG(WP(6))
      PWP(3)=DIMAG(WP(5))
      PW31(0)=DBLE( W31(5))
      PW31(1)=DBLE( W31(6))
      PW31(2)=DIMAG(W31(6))
      PW31(3)=DIMAG(W31(5))
      PW32(0)=DBLE( W32(5))
      PW32(1)=DBLE( W32(6))
      PW32(2)=DIMAG(W32(6))
      PW32(3)=DIMAG(W32(5))
C
      DV1(0)=DCMPLX(WM(1))
      DV1(1)=DCMPLX(WM(2))
      DV1(2)=DCMPLX(WM(3))
      DV1(3)=DCMPLX(WM(4))
      DP1(0)=DBLE(PWM(0))
      DP1(1)=DBLE(PWM(1))
      DP1(2)=DBLE(PWM(2))
      DP1(3)=DBLE(PWM(3))
      DV2(0)=DCMPLX(W31(1))
      DV2(1)=DCMPLX(W31(2))
      DV2(2)=DCMPLX(W31(3))
      DV2(3)=DCMPLX(W31(4))
      DP2(0)=DBLE(PW31(0))
      DP2(1)=DBLE(PW31(1))
      DP2(2)=DBLE(PW31(2))
      DP2(3)=DBLE(PW31(3))
      DV3(0)=DCMPLX(WP(1))
      DV3(1)=DCMPLX(WP(2))
      DV3(2)=DCMPLX(WP(3))
      DV3(3)=DCMPLX(WP(4))
      DP3(0)=DBLE(PWP(0))
      DP3(1)=DBLE(PWP(1))
      DP3(2)=DBLE(PWP(2))
      DP3(3)=DBLE(PWP(3))
      DV4(0)=DCMPLX(W32(1))
      DV4(1)=DCMPLX(W32(2))
      DV4(2)=DCMPLX(W32(3))
      DV4(3)=DCMPLX(W32(4))
      DP4(0)=DBLE(PW32(0))
      DP4(1)=DBLE(PW32(1))
      DP4(2)=DBLE(PW32(2))
      DP4(3)=DBLE(PW32(3))
      DMW   =DBLE(WMASS)
      DWIDTH=DBLE(WWIDTH)
      DMW2  =DMW**2
C
      IF (WMASS.EQ.0.) GOTO 10
C
      DM2INV=1.0D0/DMW2
C
      V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
      K(0)=DP1(0)+DP4(0)
      K(1)=DP1(1)+DP4(1)
      K(2)=DP1(2)+DP4(2)
      K(3)=DP1(3)+DP4(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      T=K(0)**2-K(1)**2-K(2)**2-K(3)**2
      DWS=-1.D0/DCMPLX( S-DMW2 , DMAX1(DSIGN(DMW*DWIDTH,S),0.D0) )
      DWT=-1.D0/DCMPLX( T-DMW2 , DMAX1(DSIGN(DMW*DWIDTH,T),0.D0) )
C
      SV1= (DP2(0)+Q(0))*DV1(0)-(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2)-(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0)+(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2)+(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0)-(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2)-(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0)+(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2)+(DP3(3)-Q(3))*DV4(3)
C
      TV1= (DP4(0)+K(0))*DV1(0)-(DP4(1)+K(1))*DV1(1)
     &    -(DP4(2)+K(2))*DV1(2)-(DP4(3)+K(3))*DV1(3)
      TV2=-(DP3(0)-K(0))*DV2(0)+(DP3(1)-K(1))*DV2(1)
     &    +(DP3(2)-K(2))*DV2(2)+(DP3(3)-K(3))*DV2(3)
      TV3= (DP2(0)-K(0))*DV3(0)-(DP2(1)-K(1))*DV3(1)
     &    -(DP2(2)-K(2))*DV3(2)-(DP2(3)-K(3))*DV3(3)
      TV4=-(DP1(0)+K(0))*DV4(0)+(DP1(1)+K(1))*DV4(1)
     &    +(DP1(2)+K(2))*DV4(2)+(DP1(3)+K(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      J14(0)=(DP1(0)-DP4(0))*V14 +TV1*DV4(0) +TV4*DV1(0)
      J14(1)=(DP1(1)-DP4(1))*V14 +TV1*DV4(1) +TV4*DV1(1)
      J14(2)=(DP1(2)-DP4(2))*V14 +TV1*DV4(2) +TV4*DV1(2)
      J14(3)=(DP1(3)-DP4(3))*V14 +TV1*DV4(3) +TV4*DV1(3)
      J32(0)=(DP3(0)-DP2(0))*V23 +TV3*DV2(0) +TV2*DV3(0)
      J32(1)=(DP3(1)-DP2(1))*V23 +TV3*DV2(1) +TV2*DV3(1)
      J32(2)=(DP3(2)-DP2(2))*V23 +TV3*DV2(2) +TV2*DV3(2)
      J32(3)=(DP3(3)-DP2(3))*V23 +TV3*DV2(3) +TV2*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
      JS14=K(0)*J14(0)-K(1)*J14(1)-K(2)*J14(2)-K(3)*J14(3)
      JS32=K(0)*J32(0)-K(1)*J32(1)-K(2)*J32(2)-K(3)*J32(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
      JT=J14(0)*J32(0)-J14(1)*J32(1)-J14(2)*J32(2)-J14(3)*J32(3)
C
      DVERTX = V12*V34 +V14*V23 -2.D0*V13*V24
     &        +DWS*(JS -JS12*JS34*DM2INV) +DWT*(JT -JS14*JS32*DM2INV)
C
      VERTEX = DCMPLX( DVERTX ) * (G31*G32)
C
      RETURN
C
  10  V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      DWS=-1.D0/DCMPLX( S )
C
      SV1= (DP2(0)+Q(0))*DV1(0)-(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2)-(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0)+(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2)+(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0)-(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2)-(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0)+(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2)+(DP3(3)-Q(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
C
      DVERTX = V14*V23 -V13*V24 +DWS*JS
C
      VERTEX = DCMPLX( DVERTX ) * (G31*G32)
C
      RETURN
      END
