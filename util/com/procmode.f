C   12/11/86 611121908  MEMBER NAME  PROCMODE (FORT)     M  FORTRAN
C+++++ THIS IS PROCMODE.FOR
C
       SUBROUTINE PRCMOD(BATCH)
C
C                      Process_mode tells whether a process is in
C                      the batch or interactive mode.
C
C                      Batch is .True.  if process is batch
C                      Batch is .False. if process is interactive
C
C
C      IMPLICIT  NONE
C
       LOGICAL*4  BATCH
C
       LOGICAL*4  IFTSS
C
       BATCH = .NOT. IFTSS()
C
       RETURN
       END
