      SUBROUTINE HVVSXX(V1,V2,SC,G,SMASS,SWIDTH , HVVS)
C
C This subroutine computes an off-shell scalar current of the vector-   
C vector-scalar-scalar coupling.                                        
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex SC(3)          : input  scalar                        S 
C       real    G              : coupling constant                 GVVHH
C       real    SMASS          : mass  of OUTPUT scalar S'              
C       real    SWIDTH         : width of OUTPUT scalar S'              
C                                                                       
C OUTPUT:                                                               
C       complex HVVS(3)        : scalar current            J(S':V1,V2,S)
C
      COMPLEX V1(6),V2(6),SC(3),HVVS(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
      HVVS(2) = V1(5)+V2(5)+SC(2)
      HVVS(3) = V1(6)+V2(6)+SC(3)
C
      Q(0)=REAL( HVVS(2))
      Q(1)=REAL( HVVS(3))
      Q(2)=AIMAG(HVVS(3))
      Q(3)=AIMAG(HVVS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HVVS(1) = DG * SC(1)
     &         *(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
