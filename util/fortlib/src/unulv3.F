CC********************************************************************CC
C*                                                                    *C
C* ==================---=                                             *C
C*  Subroutine UNULV3(X)                                              *C
C* ======================                                             *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Zeros X.                                                    *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          X(3)   ; = null vector.                                   *C
C*    3) Update record                                                *C
C*         6/12/85 TKXF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C* ====================---=
      SUBROUTINE UNULV3(X)
C* ========================
C  
      DIMENSION X(3)
C  
      DO 10 I=1,3
        X(I) = 0.
 10   CONTINUE
C  
      RETURN
      END
