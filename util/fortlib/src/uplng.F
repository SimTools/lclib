CC********************************************************************CC
C*                                                                    *C
C*========================--===                                       *C
C*  Subroutine UPLNG(P,PR,PL)                                         *C
C*========================--===                                       *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    calculates longitudinal momentum.                               *C
C* (Inputs)                                                           *C
C*      P (3)  : a 3-vector.                                          *C
C*      PR(3)  : a reference 3-vector.                                *C
C* (Output)                                                           *C
C*      PL     : longitudinal momentm.                                *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*        UDOT3                                                       *C
C* (Update Record)                                                    *C
C*   7/29/85   K. Fujii   Original version.                           *C
C*                                                                    *C
CC********************************************************************CC
C  
C ==========================--===
      SUBROUTINE UPLNG(P,PR,PL)
C ==========================--===
C  
      DIMENSION P(3),PR(3)
      PL = UDOT3(P,PR)
      PL = PL/SQRT(UDOT3(PR,PR))
      RETURN
      END
