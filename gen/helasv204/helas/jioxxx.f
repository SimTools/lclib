      SUBROUTINE JIOXXX(FI,FO,G,VMASS,VWIDTH , JIO)
C
C This subroutine computes an off-shell vector current from an external 
C fermion pair.  The vector boson propagator is given in Feynman gauge  
C for a massless vector and in unitary gauge for a massive vector.      
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       real    G(2)           : coupling constants                  GVF
C       real    VMASS          : mass  of OUTPUT vector V               
C       real    VWIDTH         : width of OUTPUT vector V               
C                                                                       
C OUTPUT:                                                               
C       complex JIO(6)         : vector current          J^mu(<FO|V|FI>)
C
      COMPLEX FI(6),FO(6),JIO(6),C0,C1,C2,C3,CS,D
      REAL    G(2),Q(0:3),VMASS,VWIDTH,Q2,VM2,DD
C
      JIO(5) = FO(5)-FI(5)
      JIO(6) = FO(6)-FI(6)
C
      Q(0)=REAL( JIO(5))
      Q(1)=REAL( JIO(6))
      Q(2)=AIMAG(JIO(6))
      Q(3)=AIMAG(JIO(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.) GOTO 50
C
      D=1./CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above D.
C      D=1./CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      IF (G(2).EQ.0.) GOTO 10
C
      C0=  G(1)*( FO(3)*FI(1)+FO(4)*FI(2))
     &    +G(2)*( FO(1)*FI(3)+FO(2)*FI(4))
      C1= -G(1)*( FO(3)*FI(2)+FO(4)*FI(1))
     &    +G(2)*( FO(1)*FI(4)+FO(2)*FI(3))
      C2=( G(1)*( FO(3)*FI(2)-FO(4)*FI(1)) 
     &    +G(2)*(-FO(1)*FI(4)+FO(2)*FI(3)))*CMPLX(0.,1.)
      C3=  G(1)*(-FO(3)*FI(1)+FO(4)*FI(2))
     &    +G(2)*( FO(1)*FI(3)-FO(2)*FI(4))
      CS=(Q(0)*C0-Q(1)*C1-Q(2)*C2-Q(3)*C3)/VM2
C
      JIO(1) = (C0-CS*Q(0))*D
      JIO(2) = (C1-CS*Q(1))*D
      JIO(3) = (C2-CS*Q(2))*D
      JIO(4) = (C3-CS*Q(3))*D
C
      RETURN
C
  10  D=D*G(1)
      C0=  FO(3)*FI(1)+FO(4)*FI(2)
      C1= -FO(3)*FI(2)-FO(4)*FI(1)
      C2=( FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,1.)
      C3= -FO(3)*FI(1)+FO(4)*FI(2)
      CS=(Q(0)*C0-Q(1)*C1-Q(2)*C2-Q(3)*C3)/VM2
C
      JIO(1) = (C0-CS*Q(0))*D
      JIO(2) = (C1-CS*Q(1))*D
      JIO(3) = (C2-CS*Q(2))*D
      JIO(4) = (C3-CS*Q(3))*D
C
      RETURN
C
  50  CONTINUE
C
      DD=1./Q2
C
      IF (G(2).EQ.0.) GOTO 60
C
      JIO(1) = ( G(1)*( FO(3)*FI(1)+FO(4)*FI(2))
     &          +G(2)*( FO(1)*FI(3)+FO(2)*FI(4)) )*DD
      JIO(2) = (-G(1)*( FO(3)*FI(2)+FO(4)*FI(1))
     &          +G(2)*( FO(1)*FI(4)+FO(2)*FI(3)) )*DD
      JIO(3) = ( G(1)*( FO(3)*FI(2)-FO(4)*FI(1))
     &          +G(2)*(-FO(1)*FI(4)+FO(2)*FI(3)) )*CMPLX(0.,DD)
      JIO(4) = ( G(1)*(-FO(3)*FI(1)+FO(4)*FI(2))
     &          +G(2)*( FO(1)*FI(3)-FO(2)*FI(4)) )*DD
C
      RETURN
C
  60  DD=DD*G(1)
C
      JIO(1) =  ( FO(3)*FI(1)+FO(4)*FI(2))*DD
      JIO(2) = -( FO(3)*FI(2)+FO(4)*FI(1))*DD
      JIO(3) =  ( FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,DD)
      JIO(4) =  (-FO(3)*FI(1)+FO(4)*FI(2))*DD
C
      RETURN
      END
