CC********************************************************************CC
C*                                                                    *C
C*=======================---====                                      *C
C*  Subroutine UPTV(P,PR,PTV)                                         *C
C*=======================---====                                      *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates longitudinal vector PLV with PR as a reference.      *C
C* (Inputs)                                                           *C
C*      P  (3) = a 3-vector in question.                              *C
C*      PR (3) = a reference 3-vector defining longitudinal           *C
C*               direction.                                           *C
C* (Output)                                                           *C
C*      PTV(3) = a transverse 3-vector.                               *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*      UPLV,  USUB3                                                  *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================---====
      SUBROUTINE UPTV(P,PR,PTV)
C =========================---====
C  
      DIMENSION P(3),PR(3),PLV(3),PTV(3)
      CALL UPLV(P,PR,PLV)
      CALL USUB3(P,PLV,PTV)
      RETURN
      END
