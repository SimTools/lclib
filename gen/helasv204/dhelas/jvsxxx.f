      SUBROUTINE JVSXXX(VC,SC,G,VMASS,VWIDTH , JVS)
C
C This subroutine computes an off-shell vector current from the vector-
C vector-scalar coupling.  The vector propagator is given in Feynman
C gauge for a massless vector and in unitary gauge for a massive vector.
C
C INPUT:
C       complex VC(6)          : input vector                          V
C       complex SC(3)          : input scalar                          S
C       real    G              : coupling constant                  GVVH
C       real    VMASS          : mass  of OUTPUT vector V'
C       real    VWIDTH         : width of OUTPUT vector V'
C
C OUTPUT:
C       complex JVS(6)         : vector current             J^mu(V':V,S)
C
      COMPLEX*16 VC(6),SC(3),JVS(6),DG,VK
      REAL*8     Q(0:3),G,VMASS,VWIDTH,Q2,VM2
C
      JVS(5) = VC(5)+SC(2)
      JVS(6) = VC(6)+SC(3)
C
      Q(0)=DBLE( JVS(5))
      Q(1)=DBLE( JVS(6))
      Q(2)=DIMAG(JVS(6))
      Q(3)=DIMAG(JVS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.D0) GOTO 10
C
      DG=G*SC(1)/DCMPLX( Q2-VM2 , DMAX1(DSIGN( VMASS*VWIDTH ,Q2),0.D0) )
C  For the running width, use below instead of the above DG.
C      DG=G*SC(1)/DCMPLX( Q2-VM2 , DMAX1( VWIDTH*Q2/VMASS ,0.D0) )
C
      VK=(-Q(0)*VC(1)+Q(1)*VC(2)+Q(2)*VC(3)+Q(3)*VC(4))/VM2
C
      JVS(1) = DG*(Q(0)*VK+VC(1))
      JVS(2) = DG*(Q(1)*VK+VC(2))
      JVS(3) = DG*(Q(2)*VK+VC(3))
      JVS(4) = DG*(Q(3)*VK+VC(4))
C
      RETURN
C
  10  DG=G*SC(1)/Q2
C
      JVS(1) = DG*VC(1)
      JVS(2) = DG*VC(2)
      JVS(3) = DG*VC(3)
      JVS(4) = DG*VC(4)
C
      RETURN
      END
