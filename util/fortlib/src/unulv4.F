CC********************************************************************CC
C*                                                                    *C
C*========================                                            *C
C*  Subroutine UNULV4(P)                                              *C
C*===================---==                                            *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   Zeros a 4-vector, P(4).                                          *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C ==========================
      SUBROUTINE UNULV4(P)
C =====================---==
C  
      DIMENSION P(4)
      DO 10 I=1,4
      P(I)=0.
   10 CONTINUE
      RETURN
      END
