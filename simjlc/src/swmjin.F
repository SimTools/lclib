C***********************************************************************
C*
C*====================
C* Subroutine SWMJIN
C*====================
C*
C* (Purpose)
C*     Job initialization for swimming module.
C* (Inputs) None.
C* (Output) None.
C* (Relation)
C*     Called by steering.
C*     Calls no subroutines.
C* (Updaterecord)
C*     5/21/90  K.Fujii           Original version.
C*
C***********************************************************************
 
      SUBROUTINE SWMJIN
 
      EXTERNAL UCONST, SWMBLK
C
C========< Entry Point >================================================
C
C--
C  Set version number and date.
C--
C      CALL PRPVRS('Swimmer:',101,900521)
C
C 95/09/02  K.Fujii	modified for rundom number continuation.
C
      CALL PRPVRS('Swimmer:',102,950902)
C--
C  That's it.
C--
      RETURN
      END
