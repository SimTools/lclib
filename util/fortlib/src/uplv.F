CC********************************************************************CC
C*                                                                    *C
C*=======================---====                                      *C
C*  Subroutine UPLV(P,PR,PLV)                                         *C
C*=======================---====                                      *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates longitudinal vector PLV with PR as a reference.      *C
C* (Inputs)                                                           *C
C*      P  (3) = a 3-vector in question.                              *C
C*      PR (3) = a reference 3-vector defining longitudinal           *C
C*               direction.                                           *C
C* (Output)                                                           *C
C*      PLV(3) = a longitudinal 3-vector.                             *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*      UDOT3, USCLM3                                                 *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================---====
      SUBROUTINE UPLV(P,PR,PLV)
C =========================---====
C  
      DIMENSION P(3),PR(3),PLV(3)
      A = UDOT3(P,PR)
      IF(A.EQ.0.) GO TO 10
      A=A/UDOT3(PR,PR)
      CALL USCLM3(A,PR,PLV)
      RETURN
   10 DO 20 I=1,3
      PLV(I)=0.
   20 CONTINUE
      RETURN
      END
