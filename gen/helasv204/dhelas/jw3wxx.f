      SUBROUTINE JW3WXX(W1,W2,W3,G1,G2,WMASS,WWIDTH,VMASS,VWIDTH , JW3W)
C
C This subroutine computes an off-shell W+, W-, W3, Z or photon current
C from the four-point gauge boson coupling, including the contributions
C of W exchange diagrams.  The vector propagator is given in Feynman
C gauge for a photon and in unitary gauge for W and Z bosons.  If one
C sets WMASS=0.0, then the ggg-->g current is given (see sect 2.9.1 of
C the manual).
C
C INPUT:
C       complex W1(6)          : first  vector                        W1
C       complex W2(6)          : second vector                        W2
C       complex W3(6)          : third  vector                        W3
C       real    G1             : first  coupling constant
C       real    G2             : second coupling constant
C                                                  (see the table below)
C       real    WMASS          : mass  of internal W
C       real    WWIDTH         : width of internal W
C       real    VMASS          : mass  of OUTPUT W'
C       real    VWIDTH         : width of OUTPUT W'
C
C The possible sets of the inputs are as follows:
C   -------------------------------------------------------------------
C   |  W1  |  W2  |  W3  | G1 | G2 |WMASS|WWIDTH|VMASS|VWIDTH || JW3W |
C   -------------------------------------------------------------------
C   |  W-  |  W3  |  W+  | GW |GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   |
C   |  W-  |  W3  |  W+  | GW |GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   |
C   |  W-  |  Z   |  W+  |GWWZ|GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   |
C   |  W-  |  Z   |  W+  |GWWZ|GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   |
C   |  W-  |  A   |  W+  |GWWA|GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   |
C   |  W-  |  A   |  W+  |GWWA|GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   |
C   -------------------------------------------------------------------
C   |  W3  |  W-  |  W3  | GW | GW |WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  W3  |  W+  |  W3  | GW | GW |WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  W3  |  W-  |  Z   | GW |GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  W3  |  W+  |  Z   | GW |GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  W3  |  W-  |  A   | GW |GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  W3  |  W+  |  A   | GW |GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  Z   |  W-  |  Z   |GWWZ|GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  Z   |  W+  |  Z   |GWWZ|GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  Z   |  W-  |  A   |GWWZ|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  Z   |  W+  |  A   |GWWZ|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  A   |  W-  |  A   |GWWA|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  A   |  W+  |  A   |GWWA|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   -------------------------------------------------------------------
C where all the bosons are defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex JW3W(6)        : W current             J^mu(W':W1,W2,W3)
C
      COMPLEX*16 W1(6),W2(6),W3(6),JW3W(6)
CCCCC COMPLEX*32 DW1(0:3),DW2(0:3),DW3(0:3),
      COMPLEX*16 DW1(0:3),DW2(0:3),DW3(0:3),
     &           JJ(0:3),JS(0:3),JT(0:3),J4(0:3),
     &           JT12(0:3),JT32(0:3),J12(0:3),J32(0:3),
     &           DWS,DWT,DV,W12,W32,W13,P1W2,P2W1,P3W2,P2W3,
     &           JK12,JK32,JSW3,JTW1,P3JS,KSW3,P1JT,KTW1,JQ
      REAL*8     G1,G2,WMASS,WWIDTH,VMASS,VWIDTH
      REAL*8     P1(0:3),P2(0:3),P3(0:3),Q(0:3),KS(0:3),KT(0:3),
     &           DG2,DMW,DWW,DMV,DWV,MW2,MV2,MW2INV,Q2,KS2,KT2
C
      JW3W(5) = W1(5)+W2(5)+W3(5)
      JW3W(6) = W1(6)+W2(6)+W3(6)
C
      DW1(0)=DCMPLX(W1(1))
      DW1(1)=DCMPLX(W1(2))
      DW1(2)=DCMPLX(W1(3))
      DW1(3)=DCMPLX(W1(4))
      DW2(0)=DCMPLX(W2(1))
      DW2(1)=DCMPLX(W2(2))
      DW2(2)=DCMPLX(W2(3))
      DW2(3)=DCMPLX(W2(4))
      DW3(0)=DCMPLX(W3(1))
      DW3(1)=DCMPLX(W3(2))
      DW3(2)=DCMPLX(W3(3))
      DW3(3)=DCMPLX(W3(4))
      P1(0)=DBLE(      W1(5))
      P1(1)=DBLE(      W1(6))
      P1(2)=DBLE(DIMAG(W1(6)))
      P1(3)=DBLE(DIMAG(W1(5)))
      P2(0)=DBLE(      W2(5))
      P2(1)=DBLE(      W2(6))
      P2(2)=DBLE(DIMAG(W2(6)))
      P2(3)=DBLE(DIMAG(W2(5)))
      P3(0)=DBLE(      W3(5))
      P3(1)=DBLE(      W3(6))
      P3(2)=DBLE(DIMAG(W3(6)))
      P3(3)=DBLE(DIMAG(W3(5)))
      Q(0)=-(P1(0)+P2(0)+P3(0))
      Q(1)=-(P1(1)+P2(1)+P3(1))
      Q(2)=-(P1(2)+P2(2)+P3(2))
      Q(3)=-(P1(3)+P2(3)+P3(3))
      KS(0)=P1(0)+P2(0)
      KS(1)=P1(1)+P2(1)
      KS(2)=P1(2)+P2(2)
      KS(3)=P1(3)+P2(3)
      KT(0)=P2(0)+P3(0)
      KT(1)=P2(1)+P3(1)
      KT(2)=P2(2)+P3(2)
      KT(3)=P2(3)+P3(3)
      Q2 =Q(0)**2 -(Q(1)**2 +Q(2)**2 +Q(3)**2)
      KS2=KS(0)**2-(KS(1)**2+KS(2)**2+KS(3)**2)
      KT2=KT(0)**2-(KT(1)**2+KT(2)**2+KT(3)**2)
      DG2=DBLE(G1)*DBLE(G2)
      DMW=DBLE(WMASS)
      DWW=DBLE(WWIDTH)
      DMV=DBLE(VMASS)
      DWV=DBLE(VWIDTH)
      MW2=DMW**2
      MV2=DMV**2
      MW2INV=1.D0/MW2
C
      DWS=-DG2/DCMPLX( KS2-MW2 , DMAX1(DSIGN(DMW*DWW,KS2),0.D0) )
      DWT=-DG2/DCMPLX( KT2-MW2 , DMAX1(DSIGN(DMW*DWW,KT2),0.D0) )
      IF (VMASS.EQ.0.D0) THEN
      DV = 1.D0/DCMPLX( Q2 )
      ELSE
      DV = 1.D0/DCMPLX( Q2 -MV2 , DMAX1(DSIGN(DMV*DWV,Q2 ),0.D0) )
      ENDIF
C  For the running width, use below instead of the above DV.
C      DV = 1.D0/DCMPLX( Q2 -MV2 , DMAX1(DWV*Q2/DMV,0.D0) )
C
      W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      IF (WMASS.EQ.0.D0) GOTO 10
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
      P3W2= (P3(0)+KT(0))*DW2(0)-(P3(1)+KT(1))*DW2(1)
     &     -(P3(2)+KT(2))*DW2(2)-(P3(3)+KT(3))*DW2(3)
      P2W3= (P2(0)+KT(0))*DW3(0)-(P2(1)+KT(1))*DW3(1)
     &     -(P2(2)+KT(2))*DW3(2)-(P2(3)+KT(3))*DW3(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
      JT32(0)= (P3(0)-P2(0))*W32 + P2W3*DW2(0) - P3W2*DW3(0)
      JT32(1)= (P3(1)-P2(1))*W32 + P2W3*DW2(1) - P3W2*DW3(1)
      JT32(2)= (P3(2)-P2(2))*W32 + P2W3*DW2(2) - P3W2*DW3(2)
      JT32(3)= (P3(3)-P2(3))*W32 + P2W3*DW2(3) - P3W2*DW3(3)
C
      JK12=(JT12(0)*KS(0)-JT12(1)*KS(1)-JT12(2)*KS(2)-JT12(3)*KS(3))
     &     *MW2INV
      JK32=(JT32(0)*KT(0)-JT32(1)*KT(1)-JT32(2)*KT(2)-JT32(3)*KT(3))
     &     *MW2INV
C
      J12(0)=(JT12(0)-KS(0)*JK12)*DWS
      J12(1)=(JT12(1)-KS(1)*JK12)*DWS
      J12(2)=(JT12(2)-KS(2)*JK12)*DWS
      J12(3)=(JT12(3)-KS(3)*JK12)*DWS
      J32(0)=(JT32(0)-KT(0)*JK32)*DWT
      J32(1)=(JT32(1)-KT(1)*JK32)*DWT
      J32(2)=(JT32(2)-KT(2)*JK32)*DWT
      J32(3)=(JT32(3)-KT(3)*JK32)*DWT
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
      JTW1=J32(0)*DW1(0)-J32(1)*DW1(1)-J32(2)*DW1(2)-J32(3)*DW1(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
      P1JT= (P1(0)-Q(0))*J32(0)-(P1(1)-Q(1))*J32(1)
     &     -(P1(2)-Q(2))*J32(2)-(P1(3)-Q(3))*J32(3)
      KTW1= (KT(0)-Q(0))*DW1(0)-(KT(1)-Q(1))*DW1(1)
     &     -(KT(2)-Q(2))*DW1(2)-(KT(3)-Q(3))*DW1(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
      JT(0)= (KT(0)-P1(0))*JTW1 + P1JT*DW1(0) - KTW1*J32(0)
      JT(1)= (KT(1)-P1(1))*JTW1 + P1JT*DW1(1) - KTW1*J32(1)
      JT(2)= (KT(2)-P1(2))*JTW1 + P1JT*DW1(2) - KTW1*J32(2)
      JT(3)= (KT(3)-P1(3))*JTW1 + P1JT*DW1(3) - KTW1*J32(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DG2*( DW1(0)*W32 + DW3(0)*W12 - 2.D0*DW2(0)*W13 )
      J4(1)=DG2*( DW1(1)*W32 + DW3(1)*W12 - 2.D0*DW2(1)*W13 )
      J4(2)=DG2*( DW1(2)*W32 + DW3(2)*W12 - 2.D0*DW2(2)*W13 )
      J4(3)=DG2*( DW1(3)*W32 + DW3(3)*W12 - 2.D0*DW2(3)*W13 )
C
      JJ(0)=JS(0)+JT(0)+J4(0)
      JJ(1)=JS(1)+JT(1)+J4(1)
      JJ(2)=JS(2)+JT(2)+J4(2)
      JJ(3)=JS(3)+JT(3)+J4(3)
C
      IF (VMASS.EQ.0.D0) GOTO 20
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MV2
C
      JW3W(1) = DCMPLX( (JJ(0)-JQ*Q(0))*DV )
      JW3W(2) = DCMPLX( (JJ(1)-JQ*Q(1))*DV )
      JW3W(3) = DCMPLX( (JJ(2)-JQ*Q(2))*DV )
      JW3W(4) = DCMPLX( (JJ(3)-JQ*Q(3))*DV )
C
      RETURN
C
  10  W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
C
      J12(0)=JT12(0)*DWS
      J12(1)=JT12(1)*DWS
      J12(2)=JT12(2)*DWS
      J12(3)=JT12(3)*DWS
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DG2*( DW1(0)*W32 - DW2(0)*W13 )
      J4(1)=DG2*( DW1(1)*W32 - DW2(1)*W13 )
      J4(2)=DG2*( DW1(2)*W32 - DW2(2)*W13 )
      J4(3)=DG2*( DW1(3)*W32 - DW2(3)*W13 )
C
      JJ(0)=JS(0)+J4(0)
      JJ(1)=JS(1)+J4(1)
      JJ(2)=JS(2)+J4(2)
      JJ(3)=JS(3)+J4(3)
C
      IF (VMASS.EQ.0.D0) GOTO 20
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MV2
C
      JW3W(1) = DCMPLX( (JJ(0)-JQ*Q(0))*DV )
      JW3W(2) = DCMPLX( (JJ(1)-JQ*Q(1))*DV )
      JW3W(3) = DCMPLX( (JJ(2)-JQ*Q(2))*DV )
      JW3W(4) = DCMPLX( (JJ(3)-JQ*Q(3))*DV )
C
      RETURN
C
  20  JW3W(1) = DCMPLX( JJ(0)*DV )
      JW3W(2) = DCMPLX( JJ(1)*DV )
      JW3W(3) = DCMPLX( JJ(2)*DV )
      JW3W(4) = DCMPLX( JJ(3)*DV )
C
      RETURN
      END
