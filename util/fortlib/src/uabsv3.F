CC********************************************************************CC
C*                                                                    *C
C*===========------====                                               *C
C*  Function UABSV3(X)                                                *C
C*===========------====                                               *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Calculates the absolute value of X.                         *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          UABSV3 ; absolute value of X.                             *C
C*    3) Relation                                                     *C
C*        Calls                                                       *C
C*          UDOT3                                                     *C
C*    4) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =============------===
      FUNCTION UABSV3(X)
C =============------===
C  
      DIMENSION X(3)
C  
      UABSV3 = UDOT3(X,X)
      UABSV3 = SQRT(UABSV3)
C  
      RETURN
      END
