CC********************************************************************CC
C*                                                                    *C
C*===========-----======                                              *C
C*  Function UDIST(X,Y)                                               *C
C*===========-----======                                              *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to find distance between X(3) and Y(3).                 *C
C* (Inputs)                                                           *C
C*       X,Y  :  2 3-position vectors whose distance will be          *C
C*               calculated.                                          *C
C* (Output)                                                           *C
C*       UDIST:  distance between X and Y.                            *C
C* (Output)                                                           *C
C*   Calls                                                            *C
C*       UDOT3, USUB3                                                 *C
C* (Upate Record)                                                     *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =============-----======
      FUNCTION UDIST(X,Y)
C =============-----======
C  
      DIMENSION X(3),Y(3),Z(3)
      CALL USUB3(X,Y,Z)
      D=UDOT3(Z,Z)
      D=SQRT(D)
      UDIST=D
      RETURN
      END
