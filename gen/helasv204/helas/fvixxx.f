      SUBROUTINE FVIXXX(FI,VC,G,FMASS,FWIDTH , FVI)
C
C This subroutine computes an off-shell fermion wavefunction from a     
C flowing-IN external fermion and a vector boson.                       
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex VC(6)          : input    vector                      V 
C       real    G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'             
C       real    FWIDTH         : width of OUTPUT fermion F'             
C                                                                       
C OUTPUT:                                                               
C       complex FVI(6)         : off-shell fermion             |F',V,FI>
C
      COMPLEX FI(6),VC(6),FVI(6),SL1,SL2,SR1,SR2,D,CI
      REAL    G(2),PF(0:3),FMASS,FWIDTH,PF2
C
      FVI(5) = FI(5)-VC(5)
      FVI(6) = FI(6)-VC(6)
C
      PF(0)=REAL( FVI(5))
      PF(1)=REAL( FVI(6))
      PF(2)=AIMAG(FVI(6))
      PF(3)=AIMAG(FVI(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FI(1)
     &    +(VC(2)-CI*VC(3))*FI(2)
      SL2= (VC(2)+CI*VC(3))*FI(1)
     &    +(VC(1)-   VC(4))*FI(2)
C
      IF (G(2).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FI(3)
     &    -(VC(2)-CI*VC(3))*FI(4)
      SR2=-(VC(2)+CI*VC(3))*FI(3)
     &    +(VC(1)+   VC(4))*FI(4)
C
      FVI(1) = ( G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVI(2) = ( G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
      FVI(3) = ( G(2)*((PF(0)+PF(3))*SR1 +CONJG(FVI(6))*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVI(4) = ( G(2)*(       FVI(6)*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
C
      RETURN          
C
  10  FVI(1) = G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)*D
      FVI(2) = G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)*D
      FVI(3) = G(1)*FMASS*SL1*D
      FVI(4) = G(1)*FMASS*SL2*D
C
      RETURN          
      END
