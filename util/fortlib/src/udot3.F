CC********************************************************************CC
C*                                                                    *C
C* ==========-----=======                                             *C
C*  Function UDOT3(X,Y)                                               *C
C* ==========-----=======                                             *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Calculates scalar-product of X and Y.                       *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*          Y(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          UDOT3  ; scalar-product of X and Y.                       *C
C*    3) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C* ============-----=======
      FUNCTION UDOT3(X,Y)
C* ============-----=======
C  
      DIMENSION X(3),Y(3)
C  
      UDOT3 = 0
C  
      DO 10 I=1,3
        UDOT3 = UDOT3+X(I)*Y(I)
 10   CONTINUE
C  
      RETURN
      END
