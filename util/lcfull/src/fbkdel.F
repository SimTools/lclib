C***********************************************************************
C*
C* ------------------========
C*  Subroutine FBKDEL( NPNTR )
C* ------------------========
C*(Function)
C*  Delete bank defined as OUTPUT bank, before calling user routine.
C*(Input)
C*  NPNTR : Pointer to FULL module.
C*(Relation)
C*  Called by FEVUSR
C*(Author)
C*  A. Miyamoto  19-Sep-1989  Original version.
C*
C***********************************************************************
C*
      SUBROUTINE FBKDEL( NPNTR )
C
C
C COMMON
C
#include "fldscr.inc"
C
C
C =====< Entry Point >==================================================
C
C
      IF( OUTBLK(NPNTR) .EQ. 0 ) RETURN
C
      DO 100 I = 1, OUTBLK(NPNTR)
        CALL TBDELB( 1, OUBNAM(NPNTR,I), IRET )
100   CONTINUE
C
      RETURN
      END
