CC********************************************************************CC
C*                                                                    *C
C*=========================-------====                                *C
C*  Subroutine UDCMPV(X,XR,XTV,XLV)                                   *C
C*=========================-------====                                *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to decompose a vector as:                               *C
C*      X=XTV+XLV                                                     *C
C*    with XR as a reference.                                         *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       UPLV, USUB3                                                  *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C ===========================-------====
      SUBROUTINE UDCMPV(X,XR,XTV,XLV)
C ===========================-------====
C  
      DIMENSION X(3),XR(3),XTV(3),XLV(3)
      CALL UPLV(X,XR,XLV)
      CALL USUB3(X,XLV,XTV)
      RETURN
      END
