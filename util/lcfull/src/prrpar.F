CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine PRRPAR                                                  *C
C*====================                                                *C
C*                                                                    *C
C* 1) Purpose                                                         *C
C*       Read job parameters.   .                                     *C
C* 2) Update record                                                   *C
C*   14-Sep-1989  A.Miyamoto  Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE PRRPAR
C ==========================
C
      CHARACTER*72  INPUT, WORK
C
C---------------------- Initialize input interpreter.
C
      CALL PRDBIN(IRET)
CX    IF(IRET.LT.0)               GO TO 9000
C
C ----  read parameters.
C
      NRET = 0
      IERR = 0
      WRITE(6,'(2X)')
      WRITE(6,'(2X)')
      WRITE(6,'('' Input parameters for steering part. ==========='',
     >''==============================================='')')
C
C
C (2) Loop to read input and call data base command interpreter.
C
200   CONTINUE
      INPUT = ' '
      CALL TITRD('Steering:',INPUT)
#ifdef UNIX
      PRINT *,INPUT(:LNBLNK(INPUT))
#endif
C ... Search exit command
      WORK  = INPUT
      CALL UCUPER(72, WORK)
      LW    = LKBRD( WORK, NUM)
      IF( WORK(1:4) .EQ. 'EXIT' ) GO TO 900
 
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
      WRITE(6,'('' ========== End  of data read of Steering part.='',
     >''==============================================='')')
C
C
C  Copy LEVEL and DEBUG data to FULL common
C
      CALL PRSLVL
C
      RETURN
      END
