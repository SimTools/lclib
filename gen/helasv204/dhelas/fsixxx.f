      SUBROUTINE FSIXXX(FI,SC,GC,FMASS,FWIDTH , FSI)
C
C This subroutine computes an off-shell fermion wavefunction from a
C flowing-IN external fermion and a vector boson.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex SC(3)          : input    scalar                      S
C       complex GC(2)          : coupling constants                 GCHF
C       real    FMASS          : mass  of OUTPUT fermion F'
C       real    FWIDTH         : width of OUTPUT fermion F'
C
C OUTPUT:
C       complex FSI(6)         : off-shell fermion             |F',S,FI>
C
      COMPLEX*16 FI(6),SC(3),FSI(6),GC(2),SL1,SL2,SR1,SR2,DS
      REAL*8  PF(0:3),FMASS,FWIDTH,PF2,P0P3,P0M3
C
      FSI(5) = FI(5)-SC(2)
      FSI(6) = FI(6)-SC(3)
C
      PF(0)=DBLE( FSI(5))
      PF(1)=DBLE( FSI(6))
      PF(2)=DIMAG(FSI(6))
      PF(3)=DIMAG(FSI(5))
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      DS=-SC(1)/DCMPLX(PF2-FMASS**2,DMAX1(DSIGN(FMASS*FWIDTH,PF2),0.D0))
      P0P3=PF(0)+PF(3)
      P0M3=PF(0)-PF(3)
      SL1=GC(1)*(P0P3*FI(1)+CONJG(FSI(6))*FI(2))
      SL2=GC(1)*(P0M3*FI(2)      +FSI(6) *FI(1))
      SR1=GC(2)*(P0M3*FI(3)-CONJG(FSI(6))*FI(4))
      SR2=GC(2)*(P0P3*FI(4)      -FSI(6) *FI(3))
C
      FSI(1) = ( GC(1)*FMASS*FI(1) + SR1 )*DS
      FSI(2) = ( GC(1)*FMASS*FI(2) + SR2 )*DS
      FSI(3) = ( GC(2)*FMASS*FI(3) + SL1 )*DS
      FSI(4) = ( GC(2)*FMASS*FI(4) + SL2 )*DS
C
      RETURN
      END
