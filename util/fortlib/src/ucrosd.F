CC********************************************************************CC
C*                                                                    *C
C*=======================---===                                       *C
C*  Subroutine UCROSD(X,Y,Z)                                          *C
C*=======================---===                                       *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Calculates vector-product of X x Y = Z.                     *C
C*        X, Y, and Z are REAL*8.                                     *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*          Y(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          Z(3)   ; X x Y                                            *C
C*    3) Relation                                                     *C
C*    4) Update record                                                *C
C*         9/27/86 TKSF Double precision version.                     *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UCROSD(X,Y,Z)
   
      REAL*8 X(3),Y(3),Z(3)
      Z(1) = X(2)*Y(3)-X(3)*Y(2)
      Z(2) = X(3)*Y(1)-X(1)*Y(3)
      Z(3) = X(1)*Y(2)-X(2)*Y(1)
      RETURN
      END
