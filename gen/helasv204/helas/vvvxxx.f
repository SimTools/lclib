      SUBROUTINE VVVXXX(WM,WP,W3,G , VERTEX)
C
C This subroutine computes an amplitude of the three-point coupling of  
C the gauge bosons.                                                     
C                                                                       
C INPUT:                                                                
C       complex WM(6)          : vector               flow-OUT W-       
C       complex WP(6)          : vector               flow-OUT W+       
C       complex W3(6)          : vector               J3 or A    or Z   
C       real    G              : coupling constant    GW or GWWA or GWWZ
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude               Gamma(WM,WP,W3)
C
      COMPLEX WM(6),WP(6),W3(6),VERTEX, 
     &        XV1,XV2,XV3,V12,V23,V31,P12,P13,P21,P23,P31,P32
      REAL    PWM(0:3),PWP(0:3),PW3(0:3),G
C
      PWM(0)=REAL( WM(5))
      PWM(1)=REAL( WM(6))
      PWM(2)=AIMAG(WM(6))
      PWM(3)=AIMAG(WM(5))
      PWP(0)=REAL( WP(5))
      PWP(1)=REAL( WP(6))
      PWP(2)=AIMAG(WP(6))
      PWP(3)=AIMAG(WP(5))
      PW3(0)=REAL( W3(5))
      PW3(1)=REAL( W3(6))
      PW3(2)=AIMAG(W3(6))
      PW3(3)=AIMAG(W3(5))
C
      V12=WM(1)*WP(1)-WM(2)*WP(2)-WM(3)*WP(3)-WM(4)*WP(4)
      V23=WP(1)*W3(1)-WP(2)*W3(2)-WP(3)*W3(3)-WP(4)*W3(4)
      V31=W3(1)*WM(1)-W3(2)*WM(2)-W3(3)*WM(3)-W3(4)*WM(4)
      XV1=0.
      XV2=0.
      XV3=0.
      IF (ABS(WM(1)).NE.0.) THEN
      IF (ABS(WM(1)).GE.MAX(ABS(WM(2)),ABS(WM(3)),ABS(WM(4)))*1.E-1)
     &      XV1=PWM(0)/WM(1)
      ENDIF
      IF (ABS(WP(1)).NE.0.) THEN
      IF (ABS(WP(1)).GE.MAX(ABS(WP(2)),ABS(WP(3)),ABS(WP(4)))*1.E-1)
     &      XV2=PWP(0)/WP(1)
      ENDIF
      IF (ABS(W3(1)).NE.0.) THEN
      IF (ABS(W3(1)).GE.MAX(ABS(W3(2)),ABS(W3(3)),ABS(W3(4)))*1.E-1)
     &      XV3=PW3(0)/W3(1)
      ENDIF
      P12= (PWM(0)-XV1*WM(1))*WP(1)-(PWM(1)-XV1*WM(2))*WP(2)
     &    -(PWM(2)-XV1*WM(3))*WP(3)-(PWM(3)-XV1*WM(4))*WP(4)
      P13= (PWM(0)-XV1*WM(1))*W3(1)-(PWM(1)-XV1*WM(2))*W3(2)
     &    -(PWM(2)-XV1*WM(3))*W3(3)-(PWM(3)-XV1*WM(4))*W3(4)
      P21= (PWP(0)-XV2*WP(1))*WM(1)-(PWP(1)-XV2*WP(2))*WM(2)
     &    -(PWP(2)-XV2*WP(3))*WM(3)-(PWP(3)-XV2*WP(4))*WM(4)
      P23= (PWP(0)-XV2*WP(1))*W3(1)-(PWP(1)-XV2*WP(2))*W3(2)
     &    -(PWP(2)-XV2*WP(3))*W3(3)-(PWP(3)-XV2*WP(4))*W3(4)
      P31= (PW3(0)-XV3*W3(1))*WM(1)-(PW3(1)-XV3*W3(2))*WM(2)
     &    -(PW3(2)-XV3*W3(3))*WM(3)-(PW3(3)-XV3*W3(4))*WM(4)
      P32= (PW3(0)-XV3*W3(1))*WP(1)-(PW3(1)-XV3*W3(2))*WP(2)
     &    -(PW3(2)-XV3*W3(3))*WP(3)-(PW3(3)-XV3*W3(4))*WP(4)
C
      VERTEX = -(V12*(P13-P23)+V23*(P21-P31)+V31*(P32-P12))*G
C
      RETURN
      END
