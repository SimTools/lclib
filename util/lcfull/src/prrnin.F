CC********************************************************************CC
C*                                                                    *C
C*==================                                                  *C
C* Subroutine PRRNIN(LEVEL, IDEBUG)                                   *C
C*==================                                                  *C
C*  1) Purpose                                                        *C
C*       Parts of run initialization.  Begin called after the call    *C
C*     to the begin_run module of detector analysis.                  *C
C*     Use this routine to save Run analysis information.             *C
C*       Being called by FINIRN
C*
C*  2) Author
C*       10/28/87  A.Miyamoto  To save parameters of begin_run module.*C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----=====
      SUBROUTINE PRRNIN(LEVEL, IDEBUG)
C ======================----=====
C
C
C=========< Entry Point >===============================================
C
      LEVEL = LEVEL
      IDEBUG= IDEBUG
C
C-----------------------------------------------------------------------
C (1) Save version # of constants.
C-----------------------------------------------------------------------
C
      CALL PRWCNS(-2, 0 )
C
      RETURN
      END
