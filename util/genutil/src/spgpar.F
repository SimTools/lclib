CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine SPGPAR                                                  *C
C*====================                                                *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Read parameter for SPRING job.                                   *C
C*   This routine should return to caller when read EXIT.             *C
C*                                                                    *C
C*(Author)                                                            *C
C*   Akiya Miyamoto   9-Feb-1990                                      *C
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE SPGPAR
C ==========================
C
C
      CHARACTER*72  INPUT
C
      INPUT = ' '
      CALL TITRD('EXIT:',INPUT)
C
C -------------------------------------------------------------------- C
C     Return
C -------------------------------------------------------------------- C
C
      RETURN
      END
