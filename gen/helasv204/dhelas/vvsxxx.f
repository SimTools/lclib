      SUBROUTINE VVSXXX(V1,V2,SC,G , VERTEX)
C
C This subroutine computes an amplitude of the vector-vector-scalar
C coupling.
C
C INPUT:
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex SC(3)          : input  scalar                        S
C       real    G              : coupling constant                  GVVH
C
C OUTPUT:
C       complex VERTEX         : amplitude                Gamma(V1,V2,S)
C
      COMPLEX*16 V1(6),V2(6),SC(3),VERTEX
      REAL*8     G
C
      VERTEX = G*SC(1)*(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
