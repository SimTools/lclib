C**********************************************************************
C*
C* -------------------------------======
C* Subroutine PRLRNL( NEXP, NRUN, NRET )
C* -------------------------------======
C*
C*(Function)
C*  Load runlist data base information into common /PRUNLS/
C*  If requested run are already in buffer, does not access to the
C*  data base.
C*
C*(Input)
C*  NEXP   : Experiment #.
C*  NRUN   : Run #
C*
C*(Output)
C*  NRET   : Return code, negative when failed to get information.
C*
C*(Author)
C*  A. Miyamoto  1-Dec-1988  Original version.
C*               1-Jul-1991  To load Many run of data base.
C*
C**********************************************************************
C*
      SUBROUTINE PRLRNL( NEXP, NRUN , NRET )
C
#include "prunls.inc"
      PARAMETER (IFIRST=MXxRCD+1)
C
C
      DATA  NOWPNT/IFIRST/, LSTPNT/MXxRCD/
C
C ====< Entry Point >==================================================
C
      NRET = 0
CX       PRINT *,' PRLRNL...called for NEXP=',NEXP,' NRUN=',NRUN,
CX      >        ' NOWPNT =',NOWPNT,' LSTPNT=',LSTPNT
      IF( NEXP.EQ.LRUNLS(3) .AND. NRUN.EQ.LRUNLS(4) ) RETURN
C
C  If DATA base info. exist in buffer, search corresponding run.
C
      DO 200 IP = NOWPNT+1, LSTPNT, 1
        IF( NEXP.EQ.NBRNLS(3,IP) .AND. NRUN.EQ.NBRNLS(4,IP) ) THEN
          NWRNL  = NBWORD(IP)
          CALL UVCOPY(NBWORD(IP), NBRNLS(1,IP), LRUNLS(1))
          NOWPNT = IP
CX           PRINT *,'    Target run found in Temp. buffer.'
          RETURN
        ELSEIF( NEXP.NE. NBRNLS(3,IP) ) THEN
          GO TO 300
        ELSEIF( NBRNLS(4,IP) .GT. NRUN ) THEN
          GO TO 300
        ENDIF
200   CONTINUE
C
C   If not loaded yet, read data base of target run,
C  Then read the data base infor of sucessive run.
C
300   CONTINUE
C
      CALL MAOPEN( IRET )
      IF( IRET .LT. 0 ) THEN
         PRINT *,'%Error PRLRNL..Failed to open RUNLIST data base.'
         GO TO 990
      ENDIF
 
      NOWPNT = 0
      LSTPNT = 0
      CALL MAREAD( NEXP, NRUN, NRUN+MXxRCD-1, MXxRLW, MXxRCD,
     >             NBWORD, NBRNLS, NREC )
CX       PRINT *,'    CALL MAREAD for NRUN=',NRUN,' GET NREC=',NREC
 
      IF( NREC .LT. 0 ) THEN
          PRINT *,'%Error PRLRNL .. Exp#', NEXP,' Run#', NRUN,
     .            ' does not exists in RUNLIST data base.'
          GO TO 990
      ENDIF
C
      NWRNL  = NBWORD(1)
      CALL UVCOPY(NBWORD(1), NBRNLS(1,1), LRUNLS(1))
      NOWPNT = 1
      LSTPNT = NREC
C
      CALL MACLOS
      RETURN
C
C*
990   CONTINUE
      NRET = -1
      RETURN
      END
