CC********************************************************************CC
C*                                                                    *C
C* ==========------======                                             *C
C*  Function UDOT3D(X,Y)                                              *C
C* ==========------======                                             *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Calculates scalar-product of X and Y.                       *C
C*        X, Y and UDOT3D are REAL*8.                                 *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*          Y(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          UDOT3  ; scalar-product of X and Y.                       *C
C*    3) Update record                                                *C
C*         9/27/86 TKSF Double precision version.                     *C
C*                                                                    *C
CC********************************************************************CC
   
      REAL FUNCTION UDOT3D*8(X,Y)
   
      REAL*8  X(3),Y(3)
   
      UDOT3D = 0.D0
      DO 10 I=1,3
        UDOT3D = UDOT3D + X(I)*Y(I)
 10   CONTINUE
   
      RETURN
      END
