CC********************************************************************CC
C*                                                                    *C
C* =======================---=                                        *C
C*   Subroutine UNCROS(A,B,C)                                         *C
C* =======================---=                                        *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   calculates 3-vector normed-cross product.                        *C
C*           C=AXB, Then C = C/!C!                                    *C
C* (Inputs)                                                           *C
C*   A,B  : 2 3-vectors to make cross product.                        *C
C* (Output)                                                           *C
C*     C  : results of the cross product with normalization.          *C
C* (Relation)                                                         *C
C*   calls                                                            *C
C*      UABSV3, UCROSS, USCLM3.                                       *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================---=
      SUBROUTINE UNCROS(A,B,C)
C =========================---=
C  
      DIMENSION A(3),B(3),C(3)
C-----
C  Calculate vector product.
C-----
      AMAG = UABSV3(A)
      BMAG = UABSV3(B)
      CALL UCROSS(A,B,C)
      CMAG = UABSV3(C)
      IF (CMAG.LT.(1.0E-6*AMAG*BMAG)) GOTO 5
      CALL USCLM3(1./CMAG,C,C)
      GO TO 10
C-----
C  Error return.
C-----
 5    CONTINUE
      WRITE(6,7)A,B
 7    FORMAT(' ***UNCROS Error...input vectors parallel...input',
     +   ' dumped ***',/' ***',6E12.4,' ***')
 10   RETURN
      END
