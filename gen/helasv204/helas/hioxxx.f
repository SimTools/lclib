      SUBROUTINE HIOXXX(FI,FO,GC,SMASS,SWIDTH , HIO)
C
C This subroutine computes an off-shell scalar current from an external 
C fermion pair.                                                         
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex GC(2)          : coupling constants                 GCHF
C       real    SMASS          : mass  of OUTPUT scalar S               
C       real    SWIDTH         : width of OUTPUT scalar S               
C                                                                       
C OUTPUT:                                                               
C       complex HIO(3)         : scalar current             J(<FI|S|FO>)
C
      COMPLEX FI(6),FO(6),HIO(3),GC(2),DN
      REAL    Q(0:3),SMASS,SWIDTH,Q2
C
      HIO(2) = FO(5)-FI(5)
      HIO(3) = FO(6)-FI(6)
C
      Q(0)=REAL( HIO(2))
      Q(1)=REAL( HIO(3))
      Q(2)=AIMAG(HIO(3))
      Q(3)=AIMAG(HIO(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DN=-CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HIO(1) = ( GC(1)*(FO(1)*FI(1)+FO(2)*FI(2))
     &          +GC(2)*(FO(3)*FI(3)+FO(4)*FI(4)) )/DN
C
      RETURN
      END
