CC********************************************************************CC
C*                                                                    *C
C* =====================---==                                         *C
C*  Subroutine USUB3(X,Y,Z)                                           *C
C* =====================---==                                         *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Subtacts Y from X and returns the result as Z.              *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*          Y(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          Z(3)   ; = X - Y                                          *C
C*    4) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C* =======================---==
      SUBROUTINE USUB3(X,Y,Z)
C* =======================---==
C  
      DIMENSION X(3),Y(3),Z(3)
C  
      DO 10 I=1,3
        Z(I) = X(I)-Y(I)
 10   CONTINUE
C  
      RETURN
      END
