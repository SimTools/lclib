      SUBROUTINE VSSXXX(VC,S1,S2,G , VERTEX)
C
C This subroutine computes an amplitude from the vector-scalar-scalar   
C coupling.  The coupling is absent in the minimal SM in unitary gauge. 
C                                                                       
C       complex VC(6)          : input  vector                        V 
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant (S1 charge)          
C                                                                       
C Examples of the coupling constant G for SUSY particles are as follows:
C   -----------------------------------------------------------         
C   |    S1    | (Q,I3) of S1  ||   V=A   |   V=Z   |   V=W   |         
C   -----------------------------------------------------------         
C   | nu~_L    | (  0  , +1/2) ||   ---   |  GZN(1) |  GWF(1) |         
C   | e~_L     | ( -1  , -1/2) ||  GAL(1) |  GZL(1) |  GWF(1) |         
C   | u~_L     | (+2/3 , +1/2) ||  GAU(1) |  GZU(1) |  GWF(1) |         
C   | d~_L     | (-1/3 , -1/2) ||  GAD(1) |  GZD(1) |  GWF(1) |         
C   -----------------------------------------------------------         
C   | e~_R-bar | ( +1  ,  0  ) || -GAL(2) | -GZL(2) | -GWF(2) |         
C   | u~_R-bar | (-2/3 ,  0  ) || -GAU(2) | -GZU(2) | -GWF(2) |         
C   | d~_R-bar | (+1/3 ,  0  ) || -GAD(2) | -GZD(2) | -GWF(2) |         
C   -----------------------------------------------------------         
C where the S1 charge is defined by the flowing-OUT quantum number.     
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude                Gamma(V,S1,S2)
C
      COMPLEX VC(6),S1(3),S2(3),VERTEX
      REAL    P(0:3),G
C
      P(0)=REAL( S1(2)-S2(2))
      P(1)=REAL( S1(3)-S2(3))
      P(2)=AIMAG(S1(3)-S2(3))
      P(3)=AIMAG(S1(2)-S2(2))
C
      VERTEX = G*S1(1)*S2(1)
     &        *(VC(1)*P(0)-VC(2)*P(1)-VC(3)*P(2)-VC(4)*P(3))
C
      RETURN
      END
