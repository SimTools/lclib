CC********************************************************************CC
C*                                                                    *C
C*=====================                                               *C
C* Subroutine BS51RT(LEVEL, IDEBUG)                                   *C
C*=====================                                               *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Run termination module for BASES V5.1.                           *C
C*   Print SPRING information.
C*                                                                    *C
C*(Author)                                                            *C
C*   A. Miyamoto   18-Oct-1994  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE BS51RT( LEVEL, IDEBUG )
C ==========================
C
      IMPLICIT NONE
      INTEGER*4 LEVEL, IDEBUG
      INCLUDE 'bs51pr.inc'
C
C=======< Entry Point >================================================
C
C
C -------------------------------------------------------------------- 
C (1) Print SPRING information
C -------------------------------------------------------------------- 
C
      IF( NPRINF .EQ. 1 ) CALL SPINFO(LUINFO)
      IF( NPRHST .EQ. 1 ) CALL SHPLOT(LUINFO)

      RETURN
      END
