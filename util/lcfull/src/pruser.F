CC********************************************************************CC
C*                                                                    *C
C*-----------------------------------------====                       *C
C* Subroutine PRUSER(IDREC, LEVEL, IDEBUG, NRET)                      *C
C*-----------------------------------------====                       *C
C*                                                                    *C
C*(Function)                                                          *C
C*   User Event analysis routine.                                     *C
C*                                                                    *C
C*(Input)                                                             *C
C*   IDREC   :  Record ID.                                            *C
C*   ILEVEL  :  Level of analysis.                                    *C
C*   IDEBUG  :  Debug flag.                                           *C
C*                                                                    *C
C*(Output)                                                            *C
C*   IRET    :  Return flag.                                          *C
C*                                                                    *C
C*(Author)                                                            *C
C*   A. Miyamoto   23-Apr-1987  Original version.                     *C
C*                                                                    *C
CC********************************************************************CC
C
C     ----------------------------------------======
      SUBROUTINE PRUSER( IDREC, LEVEL, IDEBUG, NRET )
C     ----------------------------------------======
C
C==================<<  Entry Point  >>================================
C
C -------------------------------------------------------------------- C
C (1) Reset return code, Skip except EVENT record.                     C
C -------------------------------------------------------------------- C
C
      NRET = 0
      IF( IDREC .NE. 1 ) RETURN
      LEVEL  = LEVEL
      IDEBUG = IDEBUG
C
C -------------------------------------------------------------------- C
C (2) List track parameter of TPC and IDC.
C -------------------------------------------------------------------- C
C
C       CALL PHLTRK(1, IRT)
C       CALL PHLTRK(3, IRT)
C
C       CALL PHLMTK(1)
C       CALL PHLMTK(2)
C
C        CALL PHLCLS(1, IRT)
C        CALL PHLCLS(2, IRT)
C
C        CALL PHLMCL(1)
C
C       CALL PHLVTX(1, IRT)
C ----------------------------------------------------------------------
C (3) Return
C ----------------------------------------------------------------------
C
      RETURN
      END
