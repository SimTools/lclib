CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine PRRINP(NRET)
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Readin job parameters and set into common /PRJPRM/            *
C*       Job parameters red in this routine is written into bank       *
C*       Production when Begin_Run DSB is processed.                   *
C*                                                                     *
C*  (Relation)                                                         *
C*       Called by                                                     *
C*           PRJOBI : Job initialize routine.tine.                     *
C*  (Author)                                                           *
C*       A. Miyamoto   28-Jan-1985   Original version.                 *
C*                     16-Jun-1985   Use Command interpreter od D.B.   *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRRINP(NRET)
C
      CHARACTER*72  INPUT
C
C=======< Entry Point >=================================================
C
C (1) Read Input
C
      NRET = 0
      IERR = 0
      WRITE(6,'('' ========== List of data read from FT05F001 ===='',
     >''==============================================='')')
C
C
C (2) Loop to read input and call data base command interpreter.
C
200   CONTINUE
      READ(5,'(A72)',END=900) INPUT
      PRINT *,INPUT
      CALL DBCRED(INPUT, IRET)
      IF(IRET.LT.0) THEN
        IERR = IERR + 1
C       PRINT *,'%PRRINP .. Error line is,'
C       PRINT *,INPUT
      ENDIF
      GO TO 200
C
C
C (9) Return to caller.
C
900   CONTINUE
      IF(IERR.GT.0) THEN
        PRINT *,'%PRRINP..Error in input parameter caused STOP'
        STOP
      ENDIF
C
C  Write end of input data statement.
C
      WRITE(6,'('' ========== End  of data read from FT05F001 ===='',
     >''==============================================='')')
      RETURN
      END
