CC********************************************************************CC
C*                                                                    *C
C* ==========-----=======                                             *C
C*  Function UDOT2(X,Y)                                               *C
C* ==========-----=======                                             *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Calculates scalar-product of X and Y.                       *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 2-dim. vector                                    *C
C*          Y(3)   ; 2-dim. vector                                    *C
C*        Output                                                      *C
C*          UDOT2  ; scalar-product of X and Y.                       *C
C*    3) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C* ============-----=======
      FUNCTION UDOT2(X,Y)
C* ============-----=======
C  
      DIMENSION X(2),Y(2)
C  
      UDOT2 = 0
C  
      DO 10 I=1,2
        UDOT2 = UDOT2+X(I)*Y(I)
 10   CONTINUE
C  
      RETURN
      END
