CC********************************************************************CC
C*                                                                    *C
C*===========------==========                                         *C
C*  Function UDELTA(X,Y,Z)                                            *C
C*===========------==========                                         *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   calculates invariant triangle function.                          *C
C* (Inputs)                                                           *C
C*     X,Y,Z  : 3 variables.                                          *C
C* (Output)                                                           *C
C*     UDELTA : value of triangle function.                           *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =============------==========
      FUNCTION UDELTA(X,Y,Z)
C =============------==========
C  
      UDELTA=X*X+Y*Y+Z*Z-2*(X*Y+Y*Z+Z*X)
      RETURN
      END
