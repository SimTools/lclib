C***********************************************************************
C*                                                                     *
C*  -----------------------------------------====                      *
C*  Subroutine BS51EV( IDREC, LEVEL, IDEBUG, NRET )                    *
C*  -----------------------------------------====                      *
C*                                                                     *
C*(Function)                                                           *
C*  Event generation by SPRING.  called from FULL steering part.       *
C*                                                                     *
C*(Input)                                                              *
C*  IDREC ; record ID, use only ID=1                                   *
C*  LEVEL ; not used.                                                  *
C*  IDEBUG; not used.                                                  *
C*(Output)                                                             *
C*  NRET  ; Return code.                                               *
C*                                                                     *
C*(Author)                                                             *
C*  A.Miyamoto   9-Feb-1990  Original version.                         *
C*               9-Apr-1992  Add NRET argument to CALL SPEVNT          *
C*              19-Oct-1994  for BASES V5.1
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BS51EV( IDREC, LEVEL, IDEBUG, NRET )
C
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE 'bs51pr.inc'
      EXTERNAL FUNC
C
C =====< Entry Point >=================================================
C

C ----------------------------------------------------------
C (1) Set return code according to the parameters.
C ----------------------------------------------------------

      IF( NDOSPR.EQ.1 ) THEN
	NRET = 0
      ELSE
	NRET = -101
        RETURN
      ENDIF

      IF( IDREC .NE. 1 ) RETURN

C ----------------------------------------------------------
C (2) Do SPRING
C ----------------------------------------------------------

      CALL SPRING( FUNC, MXTRY )

C ----------------------------------------------------------
C (3) Create Bank data.
C ----------------------------------------------------------

      CALL SPEVNT( NRET )

      RETURN
      END
