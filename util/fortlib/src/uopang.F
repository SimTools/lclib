CC********************************************************************CC
C*                                                                    *C
C*========================-----===                                    *C
C*  Subroutine UOPANG(X,Y,THETA)                                      *C
C*========================-----===                                    *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates opening angle between X(3) and Y(3).                 *C
C* (Inputs)                                                           *C
C*       X(3), Y(3)  : 2 3-vectors.                                   *C
C* (Output)                                                           *C
C*       THETA       : opening angle between X and Y.                 *C
C* (Output)                                                           *C
C*       THETA       : opening angle between X and Y.                 *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*        UNDOT3                                                      *C
C* (Update Record)                                                    *C
C*   7/29/85   K. Fujii   Original version.                           *C
C*                                                                    *C
CC********************************************************************CC
C  
C ==========================-----===
      SUBROUTINE UOPANG(X,Y,THETA)
C ==========================-----===
C  
      DIMENSION X(3),Y(3)
C  
      THETA = UNDOT3(X,Y)
      IF(ABS(THETA).GT.1.) THETA = SIGN(1.,THETA)
      THETA = ACOS(THETA)
C  
      RETURN
      END
