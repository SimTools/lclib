C**********************************************************************
C*
C* -----------------
C* Subroutine MALDSN(MODE, NEXP, NRUNS, NRUNL)
C* -----------------
C*
C*(Function)
C*   List data set name information of the run.
C*
C*(Input)
C*   MODE   ; List mode
C*        = 1 ; Simple format.
C*        = 2 ; Precise format.
C*        = 3 ; Precise format, output to the sysprint file.
C*
C*   NEXP   ; Experiment # to list.
C*         0 = List all exp and run.
C*   NRUNS  ; Run # to start list.
C*         0 = List from the begining.
C*   NRUNL  ; Run # to Terminate List.
C*         0 = List to the last run.
C*
C*(Author)
C*
C*  A. Miyamoto.   9-Jan-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MALDSN(MODE, NEXP, NRUNS, NRUNL)
C
#include "macntl.inc"
#include "marpnt.inc"
C
      PARAMETER (MXxLEN=200)
C
      COMMON /MAWORK/ NVERS, NW, IBUF(MXxLEN)
      CHARACTER*16    KEY
      COMMON /MAWRKC/ KEY
      CHARACTER    TAPDSN*44, DSKDSN*44
      CHARACTER*137 MESAGE
C
C============< Entry Point >=========================================
C
C -------------------------------------------------------------------
C (1) Locate at the begining.
C -------------------------------------------------------------------
C
      ISKIP = 0
      IF(NEXP.LE.0) THEN
        CALL MAxLOC('                ',0,IRET)
CC        PRINT *,' LOCATE              ==,IRET=',IRET
      ELSE
        WRITE(KEY,'(''E'',I5.5,''.R'')') NEXP
        IF(NRUNS.LE.0) THEN
CC          PRINT *,' Locate KEY=',KEY(1:8),'==='
          CALL MAxLOC(KEY(1:8),0, IRET)
          IF(IRET.LT.0) THEN
            WRITE(MESAGE,110) NEXP
110         FORMAT(' %Warning_MALRUN..Experiment #',I5,' not exists.')
            GO TO 800
          ENDIF
        ELSE
C
C ... Search Run # NRUNS, if not there, go back
          IRUN = NRUNS
120       CONTINUE
          WRITE(KEY(9:16),'(I6.6)') IRUN
CCC         PRINT *,' Locate KEY=',KEY(1:16),'===='
          NVERS = 0
          CALL MAxGET(0, 1, MXxLEN, KEY(:MCxKYL), NVERS,NW, IBUF, IRET)
CCC       PRINT *,' MAGET returned with IRET =',IRET
CC        CALL MAxLOC(KEY(1:16),0,IRET)
          IF(NW.LT.0) THEN
            IF(IRUN.EQ.NRUNS) THEN
              WRITE(MESAGE,125) NRUNS
125           FORMAT(' %Warning_MALRUN..Run#',I6,' not exists.')
              CALL MAMSG(1, 1, MESAGE)
            ENDIF
            IF(IRUN.EQ.NRUNL) GO TO 900
            IRUN = IRUN - 1
            IF(IRUN.GT.0) GO TO 120
            WRITE(MESAGE,130) NRUNS
130         FORMAT(' %Warning_MALRUN..There are no run before ',
     >             ' Run #',I5)
            GO TO 800
          ENDIF
          ISKIP = 1
        ENDIF
      ENDIF
C
C -------------------------------------------------------------------
C (2) Write Header.
C -------------------------------------------------------------------
C
      IFORM = 2
      IF(MODE.EQ.2) THEN
        LU    = MCxMPT
        WRITE(LU,210) NEXP, NRUNS, NRUNL
      ELSEIF(MODE.EQ.3) THEN
        LU = MCxPRT
        WRITE(LU,210) NEXP, NRUNS, NRUNL
      ELSE
        IFORM = 1
        LU = MCxMPT
        WRITE(LU,210) NEXP, NRUNS, NRUNL
      ENDIF
210   FORMAT(/,
     > ' ===== Formatted data list ======== Exp# ',I5,
     > ' Run #',I6,' to ',I6,' ==',/,
     > '                    File      tape             Size',/,
     > '  Exp#  Run# volser Seq#      data set name    (Trk)',
     > '  Disk data set name ')
C
C -------------------------------------------------------------------
C (3) Get data and print out.
C -------------------------------------------------------------------
C
      NKEY = 1
      IEXP = -1
      IRUN = -1
300   CONTINUE
      IF(ISKIP.EQ.0) THEN
        CALL UVZERO(MXxLEN, IBUF)
        CALL MAxGET(3, NKEY, MXxLEN, KEY, NVERS, NW, IBUF, IRET)
      ENDIF
      ISKIP = 0
CC      PRINT *,' MAxGET..KEY=',KEY,' NVERS=',NVERS,' NW=',NW,
CC     >        ' IRET = ',IRET
      IF(IRET.NE.0)                           GO TO 900
      IF(IEXP.EQ.IBUF(3).AND.IRUN.EQ.IBUF(4)) GO TO 300
      IEXP = IBUF(3)
      IRUN = IBUF(4)
      IF(NEXP.NE.0) THEN
        IF(IEXP.NE.NEXP)  GO TO 300
        IF(NRUNL.NE.0.AND.IRUN.GT.NRUNL)      GO TO 900
      ENDIF
      CALL MAxSLC(NW, IBUF, IRET)
      IF(IRET.LT.0) GO TO 390
C
C (3.2) List run information.
C
      WRITE(TAPDSN,'(''FORMAT.E'',I5.5,''.R'',I6.6)') IEXP, IRUN
      DSKDSN = ' '
      IF(IBUF(MPxFMD).NE.0) DSKDSN = 'T@WK.'//TAPDSN
      WRITE(LU,340) IEXP, IRUN, IBUF(MPxFMV), IBUF(MPxFMV+1),
     >              IBUF(MPxFMF), TAPDSN(:21),IBUF(MPxFMS), DSKDSN(:26)
340   FORMAT(1X,I5,I6,1X,A4,A2,I5,1X,A,I6,1X,A)
C
390   CONTINUE
      IF(NEXP.EQ.0.OR.NRUNL.EQ.0)     GO TO 300
      IF(NEXP.NE.0.AND.IRUN.LT.NRUNL) GO TO 300
      GO TO 900
C
C -------------------------------------------------------------------
C (8) Write Error message.
C -------------------------------------------------------------------
800   CONTINUE
CC      PRINT *,MESAGE
      CALL MAMSG(4, 1, MESAGE)
      RETURN
C
900   CONTINUE
      RETURN
      END
