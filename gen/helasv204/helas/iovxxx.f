      SUBROUTINE IOVXXX(FI,FO,VC,G , VERTEX)
C
C This subroutine computes an amplitude of the fermion-fermion-vector   
C coupling.                                                             
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex VC(6)          : input    vector                      V 
C       real    G(2)           : coupling constants                  GVF
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude                     <FO|V|FI>
C
      COMPLEX FI(6),FO(6),VC(6),VERTEX
      REAL    G(2)
C
      VERTEX =  G(1)*( (FO(3)*FI(1)+FO(4)*FI(2))*VC(1)
     &                +(FO(3)*FI(2)+FO(4)*FI(1))*VC(2)
     &                -(FO(3)*FI(2)-FO(4)*FI(1))*VC(3)*CMPLX(0.,1.)
     &                +(FO(3)*FI(1)-FO(4)*FI(2))*VC(4)             )
      IF (G(2).NE.0.) VERTEX = VERTEX
     &        + G(2)*( (FO(1)*FI(3)+FO(2)*FI(4))*VC(1)
     &                -(FO(1)*FI(4)+FO(2)*FI(3))*VC(2)
     &                +(FO(1)*FI(4)-FO(2)*FI(3))*VC(3)*CMPLX(0.,1.)
     &                -(FO(1)*FI(3)-FO(2)*FI(4))*VC(4)             )
C
      RETURN
      END
