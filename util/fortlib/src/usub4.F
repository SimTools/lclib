CC********************************************************************CC
C*                                                                    *C
C*=========================---===                                     *C
C*  Subroutine USUB4(P1,P2,P12)                                       *C
C*=========================---===                                     *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Carries out 4-vector subtraction.                               *C
C* (Inputs)                                                           *C
C*      P1 (4) = a 4-vector from which another is subtracted.         *C
C*      P2 (4) = a 4-vector to subtract from another.                 *C
C* (Output)                                                           *C
C*      P12(4) = a resultant 4-vector.                                *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C ===========================---===
      SUBROUTINE USUB4(P1,P2,P12)
C ===========================---===
C  
      DIMENSION P1(4),P2(4),P12(4)
      DO 10 I=1,4
      P12(I)=P1(I)-P2(I)
   10 CONTINUE
      RETURN
      END
