      SUBROUTINE BOOSTX(P,Q , PBOOST)
C
C This subroutine performs the Lorentz boost of a four-momentum.  The   
C momentum P is assumed to be given in the rest frame of Q.  PBOOST is  
C the momentum P boosted to the frame in which Q is given.  Q must be a 
C timelike momentum.                                                    
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum P in the Q rest  frame   
C       real    Q(0:3)         : four-momentum Q in the boosted frame   
C                                                                       
C OUTPUT:                                                               
C       real    PBOOST(0:3)    : four-momentum P in the boosted frame   
C
      REAL    P(0:3),Q(0:3),PBOOST(0:3),PQ,QQ,M,LF
C
      QQ=Q(1)**2+Q(2)**2+Q(3)**2
C
      IF (QQ.NE.0.) THEN
         PQ=P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3)
         M=SQRT(Q(0)**2-QQ)
         LF=((Q(0)-M)*PQ/QQ+P(0))/M
         PBOOST(0) = (P(0)*Q(0)+PQ)/M
         PBOOST(1) =  P(1)+Q(1)*LF
         PBOOST(2) =  P(2)+Q(2)*LF
         PBOOST(3) =  P(3)+Q(3)*LF
      ELSE
         PBOOST(0)=P(0)
         PBOOST(1)=P(1)
         PBOOST(2)=P(2)
         PBOOST(3)=P(3)
      ENDIF
      RETURN
      END
