C**********************************************************************
C*
C* -----------------
C* Subroutine MALRUN(MODE, NEXP, NRUNS, NRUNL)
C* -----------------
C*
C*(Function)
C*   List run information to the sysout.
C*
C*(Input)
C*   MODE   ; List mode
C*        = 1 ; Simple format.
C*        = 2 ; Precise format.
C*        = 3 ; Precise format, output to the sysprint file.
C*        = 4 ; Very precise format, for the data base modification.
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
      SUBROUTINE MALRUN(MODE, NEXP, NRUNS, NRUNL)
C
#include "macntl.inc"
C
      PARAMETER (MXxLEN=200)
C
      COMMON /MAWORK/ NVERS, NW, IBUF(MXxLEN)
      CHARACTER*16    KEY
      COMMON /MAWRKC/ KEY
      CHARACTER*8   HIZUKE, JIKAN, SDATE,STIME,EDATE,ETIME
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
          NVERS = 0
          CALL MAxGET(0, 1, MXxLEN, KEY(:MCxKYL), NVERS,NW, IBUF, IRET)
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
        WRITE(LU,220) NEXP, NRUNS, NRUNL
      ELSEIF(MODE.EQ.3) THEN
        LU = MCxPRT
        WRITE(LU,220) NEXP, NRUNS, NRUNL
      ELSEIF(MODE.EQ.4) THEN
        IFORM = 4
        LU    = MCxPRT
        WRITE(LU,240) NEXP, NRUNS, NRUNL
      ELSE
        IFORM = 1
        LU = MCxMPT
        WRITE(LU,210) NEXP, NRUNS, NRUNL
      ENDIF
210   FORMAT(/,
     > ' ============== Run List ========= Exp# ',I5,
     > ' Run #',I6,' to ',I6,' ==',/,
     > '                Last Rn ShftErrRun',
     > '    Start Start   Recycle  Format  ',
     > ' Size DK',/,
     > ' E# Run#  #Rec  Evt#Typ Crew Fg Ql',
     > '    Date   Time VOLSER F# VOLSER F#',
     > '(TRK) ?')
220   FORMAT(/,
     > ' ============== Run List ========= Exp# ',I5,
     > ' Run #',I6,' to ',I6,' =====================',/,
     > '     Processed                    Last Run Shft  Err  Run',
     > '    Start Start    End    End   Recycle    Format   ',
     > ' Size DK',/,
     > '   Date    Time Exp#  Run#  #Rec  Evt#Type Crew Flag Qual',
     > '    Date   Time    Date   Time VOLSER  F# VOLSER  F#',
     > '(TRK)  ?')
240   FORMAT(/,
     > ' ===== Run list very precise format === Exp# ',I5,
     > ' Run #',I6,' to ',I6,' =====================',/,
     > '                   Last Run Shft  Err  Run',
     > '    Start Start       End    End      Recycle    Format   ',
     > ' Size DK',/,
     > ' Exp#  Run#  #Rec  Evt#Type Crew Flag Qual',
     > '    Date   Time       Date   Time    VOLSER  F# VOLSER  F#',
     > '(TRK)  ?')
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
      IF(IRET.NE.0)                           GO TO 900
      IF(IEXP.EQ.IBUF(3).AND.IRUN.EQ.IBUF(4)) GO TO 300
      IEXP = IBUF(3)
      IRUN = IBUF(4)
      IF(NEXP.NE.0) THEN
        IF(IEXP.NE.NEXP)  GO TO 300
        IF(NRUNL.NE.0.AND.IRUN.GT.NRUNL)      GO TO 900
      ENDIF
C
C (3.2) List run information.
C
      CALL MAxSLC(NW, IBUF, IRET)
      IF(IRET.LT.0) GO TO 390
      CALL UCDATE(IBUF(11), SDATE)
      CALL UCTIME(IBUF(12), STIME)
      IF(IFORM.EQ.2) THEN
        CALL UCDATE(IBUF( 1), HIZUKE)
        CALL UCTIME(IBUF( 2), JIKAN)
        CALL UCDATE(IBUF(13), EDATE)
        CALL UCTIME(IBUF(14), ETIME)
        WRITE(LU,330) HIZUKE, JIKAN(1:5),
     >    (IBUF(K1),K1=3,10), SDATE, STIME(1:5),
     >    EDATE, ETIME(1:5), (IBUF(K2),K2=15,22)
      ELSEIF(IFORM.EQ.4) THEN
        CALL UCDATE(IBUF(13), EDATE)
        CALL UCTIME(IBUF(14), ETIME)
        WRITE(LU,335)
     >    (IBUF(K1),K1=3,10), SDATE, STIME,
     >    EDATE, ETIME, (IBUF(K2),K2=15,22)
      ELSE
        WRITE(LU,340)
     >    (IBUF(K1),K1=3,10), SDATE, STIME(1:5),
     >    (IBUF(K2),K2=15,22)
      ENDIF
330   FORMAT(1X,A,1X,A,I5,I6,I6,I6,I4,1X,A,2I5,1X,A,1X,A,
     >       1X,A,1X,A,1X,A4,A2,I4,1X,A4,A2,I4,I5,I3)
335   FORMAT(          I5,I6,I6,I6,I4,1X,A,I7,I3,1X,A,1X,A,
     >       1X,A,1X,A,1X,A4,A2,I4,1X,A4,A2,I4,I5,I3)
340   FORMAT(          I3,I5,I6,I6,I3,1X,A,2I3,1X,A,1X,A,
     >                 1X,A4,A2,I3,1X,A4,A2,I3,I5,I2)
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
