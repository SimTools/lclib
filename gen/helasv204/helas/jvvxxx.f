      SUBROUTINE JVVXXX(V1,V2,G,VMASS,VWIDTH , JVV)
C
C This subroutine computes an off-shell vector current from the three-  
C point gauge boson coupling.  The vector propagator is given in Feynman
C gauge for a massless vector and in unitary gauge for a massive vector.
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       real    G              : coupling constant (see the table below)
C       real    VMASS          : mass  of OUTPUT vector V               
C       real    VWIDTH         : width of OUTPUT vector V               
C                                                                       
C The possible sets of the inputs are as follows:                       
C    ------------------------------------------------------------------ 
C    |   V1   |   V2   |  JVV   |      G       |   VMASS  |  VWIDTH   | 
C    ------------------------------------------------------------------ 
C    |   W-   |   W+   |  A/Z   |  GWWA/GWWZ   | 0./ZMASS | 0./ZWIDTH | 
C    | W3/A/Z |   W-   |  W+    | GW/GWWA/GWWZ |   WMASS  |  WWIDTH   | 
C    |   W+   | W3/A/Z |  W-    | GW/GWWA/GWWZ |   WMASS  |  WWIDTH   | 
C    ------------------------------------------------------------------ 
C where all the bosons are defined by the flowing-OUT quantum number.   
C                                                                       
C OUTPUT:                                                               
C       complex JVV(6)         : vector current            J^mu(V:V1,V2)
C
      COMPLEX V1(6),V2(6),JVV(6),J12(0:3),JS,DG,
     &        SV1,SV2,S11,S12,S21,S22,V12
      REAL    P1(0:3),P2(0:3),Q(0:3),G,VMASS,VWIDTH,GS,S,VM2,M1,M2
C
      JVV(5) = V1(5)+V2(5)
      JVV(6) = V1(6)+V2(6)
C
      P1(0)=REAL( V1(5))
      P1(1)=REAL( V1(6))
      P1(2)=AIMAG(V1(6))
      P1(3)=AIMAG(V1(5))
      P2(0)=REAL( V2(5))
      P2(1)=REAL( V2(6))
      P2(2)=AIMAG(V2(6))
      P2(3)=AIMAG(V2(5))
      Q(0)=-REAL( JVV(5))
      Q(1)=-REAL( JVV(6))
      Q(2)=-AIMAG(JVV(6))
      Q(3)=-AIMAG(JVV(5))
      S=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      V12=V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4)
      SV1= (P2(0)-Q(0))*V1(1) -(P2(1)-Q(1))*V1(2)
     &    -(P2(2)-Q(2))*V1(3) -(P2(3)-Q(3))*V1(4)
      SV2=-(P1(0)-Q(0))*V2(1) +(P1(1)-Q(1))*V2(2)
     &    +(P1(2)-Q(2))*V2(3) +(P1(3)-Q(3))*V2(4)
      J12(0)=(P1(0)-P2(0))*V12 +SV1*V2(1) +SV2*V1(1)
      J12(1)=(P1(1)-P2(1))*V12 +SV1*V2(2) +SV2*V1(2)
      J12(2)=(P1(2)-P2(2))*V12 +SV1*V2(3) +SV2*V1(3)
      J12(3)=(P1(3)-P2(3))*V12 +SV1*V2(4) +SV2*V1(4)
C
      IF (VMASS.EQ.0.) GOTO 10
C
      M1=P1(0)**2-(P1(1)**2+P1(2)**2+P1(3)**2)
      M2=P2(0)**2-(P2(1)**2+P2(2)**2+P2(3)**2)
      S11=P1(0)*V1(1)-P1(1)*V1(2)-P1(2)*V1(3)-P1(3)*V1(4)
      S12=P1(0)*V2(1)-P1(1)*V2(2)-P1(2)*V2(3)-P1(3)*V2(4)
      S21=P2(0)*V1(1)-P2(1)*V1(2)-P2(2)*V1(3)-P2(3)*V1(4)
      S22=P2(0)*V2(1)-P2(1)*V2(2)-P2(2)*V2(3)-P2(3)*V2(4)
      JS=(V12*(-M1+M2) +S11*S12 -S21*S22)/VM2
C
      DG=-G/CMPLX( S-VM2 , MAX(SIGN( VMASS*VWIDTH ,S),0.) )
C  For the running width, use below instead of the above DG.
C      DG=-G/CMPLX( S-VM2 , MAX( VWIDTH*S/VMASS ,0.) )
C
      JVV(1) = DG*(J12(0)-Q(0)*JS)
      JVV(2) = DG*(J12(1)-Q(1)*JS)
      JVV(3) = DG*(J12(2)-Q(2)*JS)
      JVV(4) = DG*(J12(3)-Q(3)*JS)
C
      RETURN
C
  10  GS=-G/S
C
      JVV(1) = GS*J12(0)
      JVV(2) = GS*J12(1)
      JVV(3) = GS*J12(2)
      JVV(4) = GS*J12(3)
C
      RETURN
      END
