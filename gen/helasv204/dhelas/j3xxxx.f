      SUBROUTINE J3XXXX(FI,FO,GAF,GZF,ZMASS,ZWIDTH , J3)
C
C This subroutine computes the sum of photon and Z currents with the
C suitable weights ( J(W3) = cos(theta_W) J(Z) + sin(theta_W) J(A) ).
C The output J3 is useful as an input of VVVXXX, JVVXXX or W3W3XX.
C The photon propagator is given in Feynman gauge, and the Z propagator
C is given in unitary gauge.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       real    GAF(2)         : FI couplings with A                 GAF
C       real    GZF(2)         : FI couplings with Z                 GZF
C       real    ZMASS          : mass  of Z
C       real    ZWIDTH         : width of Z
C
C OUTPUT:
C       complex J3(6)          : W3 current             J^mu(<FO|W3|FI>)
C
      COMPLEX*16 FI(6),FO(6),J3(6),
     &           C0L,C1L,C2L,C3L,CSL,C0R,C1R,C2R,C3R,CSR,DZ,DDIF
      REAL*8  GAF(2),GZF(2),Q(0:3),ZMASS,ZWIDTH,ZM2,ZMW,Q2,DA,WW,
     &        CW,SW,GN,GZ3L,GA3L
C
      J3(5) = FO(5)-FI(5)
      J3(6) = FO(6)-FI(6)
C
      Q(0)=-DBLE( J3(5))
      Q(1)=-DBLE( J3(6))
      Q(2)=-DIMAG(J3(6))
      Q(3)=-DIMAG(J3(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      ZM2=ZMASS**2
      ZMW=ZMASS*ZWIDTH
C
      DA=1.D0/Q2
      WW=DMAX1(DSIGN( ZMW ,Q2),0.D0)
      DZ=1.D0/DCMPLX( Q2-ZM2 , WW )
      DDIF=DCMPLX( -ZM2 , WW )*DA*DZ
C DDIF is the difference : DDIF=DA-DZ
C  For the running width, use below instead of the above WW,DZ and DDIF.
C      WW=DMAX1( ZWIDTH*Q2/ZMASS ,0.D0)
C      DZ=1.D0/DCMPLX( Q2-ZM2 , WW )
C      DDIF=DCMPLX( -ZM2 , WW )*DA*DZ
C
      CW=1.D0/DSQRT(1.D0+(GZF(2)/GAF(2))**2)
      SW=DSQRT((1.D0-CW)*(1.D0+CW))
      GN=GAF(2)*SW
      GZ3L=GZF(1)*CW
      GA3L=GAF(1)*SW
      C0L=  FO(3)*FI(1)+FO(4)*FI(2)
      C0R=  FO(1)*FI(3)+FO(2)*FI(4)
      C1L=-(FO(3)*FI(2)+FO(4)*FI(1))
      C1R=  FO(1)*FI(4)+FO(2)*FI(3)
      C2L= (FO(3)*FI(2)-FO(4)*FI(1))*DCMPLX(0.D0,1.D0)
      C2R=(-FO(1)*FI(4)+FO(2)*FI(3))*DCMPLX(0.D0,1.D0)
      C3L= -FO(3)*FI(1)+FO(4)*FI(2)
      C3R=  FO(1)*FI(3)-FO(2)*FI(4)
      CSL=(Q(0)*C0L-Q(1)*C1L-Q(2)*C2L-Q(3)*C3L)/ZM2
      CSR=(Q(0)*C0R-Q(1)*C1R-Q(2)*C2R-Q(3)*C3R)/ZM2
C
      J3(1) =  GZ3L*DZ*(C0L-CSL*Q(0))+GA3L*C0L*DA
     &       + GN*(C0R*DDIF+CSR*Q(0)*DZ)
      J3(2) =  GZ3L*DZ*(C1L-CSL*Q(1))+GA3L*C1L*DA
     &       + GN*(C1R*DDIF+CSR*Q(1)*DZ)
      J3(3) =  GZ3L*DZ*(C2L-CSL*Q(2))+GA3L*C2L*DA
     &       + GN*(C2R*DDIF+CSR*Q(2)*DZ)
      J3(4) =  GZ3L*DZ*(C3L-CSL*Q(3))+GA3L*C3L*DA
     &       + GN*(C3R*DDIF+CSR*Q(3)*DZ)
C
      RETURN
      END
