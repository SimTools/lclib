      SUBROUTINE HVVXXX(V1,V2,G,SMASS,SWIDTH , HVV)
C
C This subroutine computes an off-shell scalar current from the vector-
C vector-scalar coupling.
C
C INPUT:
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       real    G              : coupling constant                  GVVH
C       real    SMASS          : mass  of OUTPUT scalar S
C       real    SWIDTH         : width of OUTPUT scalar S
C
C OUTPUT:
C       complex HVV(3)         : off-shell scalar current     J(S:V1,V2)
C
      COMPLEX*16 V1(6),V2(6),HVV(3),DG
      REAL*8     Q(0:3),G,SMASS,SWIDTH,Q2
C
      HVV(2) = V1(5)+V2(5)
      HVV(3) = V1(6)+V2(6)
C
      Q(0)=DBLE( HVV(2))
      Q(1)=DBLE( HVV(3))
      Q(2)=DIMAG(HVV(3))
      Q(3)=DIMAG(HVV(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/DCMPLX( Q2-SMASS**2 , DMAX1(DSIGN( SMASS*SWIDTH ,Q2),0.D0) )
C
      HVV(1) = DG*(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
