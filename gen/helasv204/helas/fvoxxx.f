      SUBROUTINE FVOXXX(FO,VC,G,FMASS,FWIDTH , FVO)
C
C This subroutine computes an off-shell fermion wavefunction from a     
C flowing-OUT external fermion and a vector boson.                      
C                                                                       
C INPUT:                                                                
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex VC(6)          : input    vector                      V 
C       real    G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'             
C       real    FWIDTH         : width of OUTPUT fermion F'             
C                                                                       
C OUTPUT:                                                               
C       complex FVO(6)         : off-shell fermion             <FO,V,F'|
C
      COMPLEX FO(6),VC(6),FVO(6),SL1,SL2,SR1,SR2,D,CI
      REAL    G(2),PF(0:3),FMASS,FWIDTH,PF2
C
      FVO(5) = FO(5)+VC(5)
      FVO(6) = FO(6)+VC(6)
C
      PF(0)=REAL( FVO(5))
      PF(1)=REAL( FVO(6))
      PF(2)=AIMAG(FVO(6))
      PF(3)=AIMAG(FVO(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FO(3)
     &    +(VC(2)+CI*VC(3))*FO(4)
      SL2= (VC(2)-CI*VC(3))*FO(3)
     &    +(VC(1)-   VC(4))*FO(4)
C
      IF (G(2).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FO(1)
     &    -(VC(2)+CI*VC(3))*FO(2)
      SR2=-(VC(2)-CI*VC(3))*FO(1)
     &    +(VC(1)+   VC(4))*FO(2)
C
      FVO(1) = ( G(2)*( (PF(0)+PF(3))*SR1        +FVO(6)*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVO(2) = ( G(2)*( CONJG(FVO(6))*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
      FVO(3) = ( G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVO(4) = ( G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
C
      RETURN          
C
  10  FVO(1) = G(1)*FMASS*SL1*D
      FVO(2) = G(1)*FMASS*SL2*D
      FVO(3) = G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)*D
      FVO(4) = G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)*D
C
      RETURN          
      END
