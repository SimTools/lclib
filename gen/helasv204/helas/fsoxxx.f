      SUBROUTINE FSOXXX(FO,SC,GC,FMASS,FWIDTH , FSO)
C
C This subroutine computes an off-shell fermion wavefunction from a     
C flowing-OUT external fermion and a vector boson.                      
C                                                                       
C INPUT:                                                                
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex SC(6)          : input    scalar                      S 
C       complex GC(2)          : coupling constants                 GCHF
C       real    FMASS          : mass  of OUTPUT fermion F'             
C       real    FWIDTH         : width of OUTPUT fermion F'             
C                                                                       
C OUTPUT:                                                               
C       complex FSO(6)         : off-shell fermion             <FO,S,F'|
C
      COMPLEX FO(6),SC(6),FSO(6),GC(2),SL1,SL2,SR1,SR2,DS
      REAL    PF(0:3),FMASS,FWIDTH,PF2,P0P3,P0M3
C
      FSO(5) = FO(5)+SC(2)
      FSO(6) = FO(6)+SC(3)
C
      PF(0)=REAL( FSO(5))
      PF(1)=REAL( FSO(6))
      PF(2)=AIMAG(FSO(6))
      PF(3)=AIMAG(FSO(5))
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      DS=-SC(1)/CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      P0P3=PF(0)+PF(3)
      P0M3=PF(0)-PF(3)
      SL1=GC(2)*(P0P3*FO(3)      +FSO(6) *FO(4))
      SL2=GC(2)*(P0M3*FO(4)+CONJG(FSO(6))*FO(3))
      SR1=GC(1)*(P0M3*FO(1)      -FSO(6) *FO(2))
      SR2=GC(1)*(P0P3*FO(2)-CONJG(FSO(6))*FO(1))
C
      FSO(1) = ( GC(1)*FMASS*FO(1) + SL1 )*DS
      FSO(2) = ( GC(1)*FMASS*FO(2) + SL2 )*DS
      FSO(3) = ( GC(2)*FMASS*FO(3) + SR1 )*DS
      FSO(4) = ( GC(2)*FMASS*FO(4) + SR2 )*DS
C
      RETURN          
      END
