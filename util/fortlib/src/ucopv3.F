CC********************************************************************CC
C*                                                                    *C
C* ====================---==                                          *C
C*  Subroutine UCOPV3(X,Y)                                            *C
C* ====================---==                                          *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Copies X to Y.                                              *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          Y(3)   ; = X(3)                                           *C
C*    3) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =======================---==
      SUBROUTINE UCOPV3(X,Y)
C =======================---==
C  
      DIMENSION X(3),Y(3)
C  
      DO 10 I=1,3
        Y(I) = X(I)
 10   CONTINUE
C  
      RETURN
      END
