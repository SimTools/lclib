CC********************************************************************CC
C*                                                                    *C
C* ====================---===                                         *C
C*  Subroutine UNRMV3(X,UX)                                           *C
C* ====================---===                                         *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Normalizes the given vector,X.                              *C
C*    2) I/O specification                                            *C
C*        Input                                                       *C
C*          X(3)   ; 3-dim. vector                                    *C
C*        Output                                                      *C
C*          UX(3)  ; unit vector in the direction of X.               *C
C*    3) Relation                                                     *C
C*        Calls                                                       *C
C*            UABSV3, USCLM3                                          *C
C*    4) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C* ======================---===
      SUBROUTINE UNRMV3(X,UX)
C* ======================---===
C  
      DIMENSION X(3),UX(3)
C  
      AX   = UABSV3(X)
      AX   = 1./AX
      CALL USCLM3(AX,X,UX)
C  
      RETURN
      END
