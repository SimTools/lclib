C***********************************************************************
C*
C*  ------------------
C*  Subroutine PRCEVT
C*  ------------------
C*
C*(Function)
C*   Process Event.
C*
C*(Relation)
C*   Called from PRMAIN, RDMAIN, of FULLANL.
C*
C*(Input & Output )
C*   None.
C*
C*(Author)
C*   A.Miyamoto  25-Apr-1987  Modified from R.Itoh's original
C*                            for the Production job.
C*               30-Sep-1987  Change meanings of debug flag.
C*
C***********************************************************************
C*
      SUBROUTINE PRCEVT
C
#include "prjctl.inc"
#include "prrsta.inc"
#include "presta.inc"
#include "prhead.inc"
#include "prjflg.inc"
C
C
C VARIABLES
C
        LOGICAL * 4     OKFLG, EXIFLG, PRTFLG, OUTFLG
C
C
C ==========< Entry Point >=============================================
C
C ----------------------------------------------------------------------
C (1) Reset variables.
C ----------------------------------------------------------------------
C
      CALL UVZERO(62,     NRDREC)
      CALL UVZERO(2+5*32, NCTBGN)
      ICOMER = 1
      IF ( ICOMER .NE. 1 ) RETURN
      EXIFLG = .FALSE.
      OKFLG  = .TRUE.
      OUTFLG = .TRUE.
      PRTFLG = .FALSE.
      IDEBUG = NJBDBG(2, 1)/10
      IEVSEQ = 0
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C (2) Start of Analysis Loop.
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
200   CONTINUE
 
      CALL CLOCKM( NCTBGN )
      CALL PRGEVT( IDREC )
 
      IF( IDREC .EQ. -1000 ) THEN
C
        WRITE(6,210) IEVSEQ, xEXPNO, xRUNNO, xEVTNO, IEVSEQ, xRECTP
210     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to end-of-file of input file.    ',/,
     >' *   at  event sequence #   of ',I8,                           /,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Seq# ',I8,        /,
     >' *        Record Type ',I4,                                    /,
     >' ************************************************************',/,
     >   //)
        GO TO 990
C
      ELSEIF( IDREC .EQ. -1001 ) THEN
C
        WRITE(6,220) IEVSEQ, xEXPNO, xRUNNO, xEVTNO, IEVSEQ, xRECTP
220     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate by the completion of event generation',/,
     >' *   at  event sequence #   of ',I8,                           /,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Seq# ',I8,        /,
     >' *        Record Type ',I4,                                    /,
     >' ************************************************************',/,
     >   //)
        GO TO 990
C
      ELSEIF( IDREC .LT. 0  ) THEN
C
        WRITE(6,230)IDREC,IEVSEQ,xEXPNO, xRUNNO, xEVTNO, IEVSEQ, xRECTP
230     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to TBREAD/TBINIT error..IDREC=',I6,/,
     >' *   at  event sequence #   of ',I8,                           /,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Seq# ',I8,        /,
     >' *        Record Type ',I4,                                    /,
     >' ************************************************************',/,
     >   //)
        GO TO 990
      ENDIF
 
C
C ----------------------------------------------------------------------
C (2.1) Accumulate RECORD statistics.
C ----------------------------------------------------------------------
C
      NRDREC  = NRDREC + 1
      IF( IDREC .GE. 1 .AND. IDREC .LE. 9) THEN
         NSTREC(1, IDREC) = NSTREC(1, IDREC) + 1
      ENDIF
C
C ----------------------------------------------------------------------
C (2.3) Obtain Header block.
C ----------------------------------------------------------------------
C
 
      CALL PRUHED( IDREC )
 
C
C ----------------------------------------------------------------------
C (2.4) Print Event message.
C ----------------------------------------------------------------------
C
 
      IF( IDREC .EQ. 1 ) THEN
        IF( IDEBUG .GE. 1. AND. MOD( IEVSEQ, NDFREQ ) .EQ. 0 ) THEN
          PRTFLG = .TRUE.
        ELSE
          PRTFLG = .FALSE.
        ENDIF
      ELSE
        IF( IDEBUG .GE. 1 ) THEN
          PRTFLG = .TRUE.
        ELSE
          PRTFLG = .FALSE.
        ENDIF
      ENDIF
 
 
C (2.5) Write Event Header message.
C     PRINT *,' NFSTEV, NLSTEV=',NFSTEV, NLSTEV
C
C
      IF( xEXPNO .GT. NLSTEX .OR.
     >   (xEXPNO .LE. NLSTEX .AND. xRUNNO .GT. NLSTRN )) THEN
        WRITE(6,254) xEVTNO, NLSTEX, NLSTRN,
     >               xEXPNO, xRUNNO, xEVTNO, IEVSEQ
254     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to Run # over.                   ',/,
     >' *   at  event #            of ',I8,                           /,
     >' *       requested to stop  at Exp#',I6,' Run#',I6,            /,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
#ifdef UNIX
        KRET = -9998
        CALL PRGEVT(KRET)
#endif
        GO TO 990
      ENDIF
 
      IF(  xEXPNO.LT.NFSTEX .OR.
     >   ( xEXPNO.GE.NFSTEX .AND. xRUNNO.LT.NFSTRN ) ) THEN
        IF( PRTFLG .AND. IDEBUG. GE. 3 ) THEN
          WRITE(6,252) xEXPNO, xRUNNO
252       FORMAT(
     >    ' *** Exp',I5,' Run#',I6,' will be skipped. **',
     >    '****************************************************')
        ENDIF
        GO TO 200
      ENDIF
 
      IF( IDREC .EQ. 1 .AND .xEVTNO.LT.NFSTEV ) THEN
        IF( PRTFLG .AND. IDEBUG. GE. 3 ) THEN
            WRITE(6,250) xRUNNO, xEVTNO, IEVSEQ
250         FORMAT(
     >      ' *** Run',I5,' Evt#',I6,' Seq#',I6,' is skipped. **',
     >      '****************************************************')
        ENDIF
        GO TO 200
      ENDIF
C
      IF( IDREC .EQ. 1 ) IEVSEQ = IEVSEQ + 1
C
      IF( PRTFLG ) THEN
        XCPUTM = FLOAT(NCTBGN) / 1000.
        WRITE(6,260) xRUNNO, xEVTNO, IEVSEQ, IDREC, XCPUTM
260     FORMAT(1H ,//,
     >  ' *** Run',I5,' Evt#',I6,' Seq#',I6,' Rec.ID=',I3,
     >  ' **** at CPU time ',F8.3,' sec.  ***********')
CCC     IF( IDEBUG .GE. 2) CALL PRWURH
      ENDIF
C
C ----------------------------------------------------------------------
C (3) Remove banks from input.
C ----------------------------------------------------------------------
C
      CALL PRRBNK( IDREC , ' ', ' ')
C
C ----------------------------------------------------------------------
C (4) Execute Analysis routine.
C ----------------------------------------------------------------------
C
      CALL FDOEVT( IDREC, NSTREC(1,1), OKFLG, EXIFLG )
C
C
C     OKFLG   EXIFLG
C
C     .TRUE.  .FALSE.    Continue analysis, output results.
C     .FALSE. .FALSE.    Continue analysis, does not out put results.
C     .FALSE. .TRUE.     Terminate analysis.
C
C
      IF ( EXIFLG ) THEN
        WRITE(6,410) xEXPNO, xRUNNO, xEVTNO, IEVSEQ
410     FORMAT(//,
     >' ************************************************************',/,
     >' *   Analysis routine requested to terminate job.           *',/,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
        GO TO 990
      END IF
C
C ----------------------------------------------------------------------
C (5) Output results.
C ----------------------------------------------------------------------
C
C     CALL TBSQEZ ( RECID, IRET )
C     IF ( OKFLG .AND. RECID.EQ.1 ) NREC(11) = NREC(11) + 1
C     IF ( OUTFLG .AND. OKFLG ) THEN
C
 
      IWTERR = 0
      IF( OKFLG .AND. OUTFLG ) THEN
C
        NWTREC = NWTREC + 1
        IF( IDREC .GE. 1 .AND. IDREC .LE. 9) THEN
          NSTREC(6, IDREC) = NSTREC(6, IDREC) + 1
        ENDIF
C
        CALL PRPEVT( IDREC, IWTERR )
      ENDIF
C
C ----------------------------------------------------------------------
C (6) Print Event termination message
C ----------------------------------------------------------------------
C
      CALL CLOCKM( NCTEND )
      EVTCPU = FLOAT( NCTEND - NCTBGN ) / 1000.
      TIMUSD = FLOAT( NCTEND ) /1000.
#ifdef  MSP
      CALL TIMEL ( TIMLFT )
#endif
#ifdef  UNIX
      TIMLFT = 24.0*3600.0
#endif
C
      IF( PRTFLG .AND. IDREC.EQ.1 ) THEN
        WRITE(6,610) TIMUSD, EVTCPU, TIMLFT
610     FORMAT(' *** Event Summary *********************** at CPU ',
     >    'time ',F8.3,' sec.  ***********',/,
     >  5X,' Time for this event = ',F10.3,'sec',/,
     >  5X,' Time left.          = ',F10.3,'sec')
      ENDIF
C
C ----------------------------------------------------------------------
C (7) Swich according to the event end status.
C ----------------------------------------------------------------------
C
      IF( IWTERR .EQ. -2000 ) THEN
        WRITE(6,710) xEXPNO, xRUNNO, xEVTNO, IEVSEQ
710     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to output data size over        *',/,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
#ifdef UNIX
        KRET = -9998
        CALL PRGEVT(KRET)
#endif
        GO TO 990
C
      ELSEIF( IWTERR .LT. 0 ) THEN
        WRITE(6,720) xEXPNO, xRUNNO, xEVTNO, IEVSEQ
720     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to TBWRIT Error.                *',/,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
#ifdef UNIX
        KRET = -9998
        CALL PRGEVT(KRET)
#endif
        GO TO 990
C
      ELSEIF( RTIMLM .GT. 0.1 .AND. TIMLFT .LE. RTIMLM ) THEN
        WRITE(6,730) TIMLFT, RTIMLM,
     >               xEXPNO, xRUNNO, xEVTNO, IEVSEQ
730     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to remaining CPU time limit over.',/,
     >' *   at  remaining CPU time of ',F8.3,' sec.                 ',/,
     >' *       requested to stop  at ',F8.3,' sec                  ',/,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
#ifdef UNIX
        KRET = -9998
        CALL PRGEVT(KRET)
#endif
        GO TO 990
C
      ELSEIF( RCPUMX .GT. 0.1 .AND. TIMLFT .LE. RTIMLM ) THEN
        WRITE(6,740) TIMLFT, TIMUSD,
     >               xEXPNO, xRUNNO, xEVTNO, IEVSEQ
740     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to CPU time limit over           ',/,
     >' *   at  running CPU   time of ',F8.3,' sec.                 ',/,
     >' *       requested to stop  at ',F8.3,' sec                  ',/,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
#ifdef UNIX
        KRET = -9998
        CALL PRGEVT(KRET)
#endif
        GO TO 990
C
      ELSEIF( IEVSEQ .GE. NEVMAX ) THEN
        WRITE(6,750) IEVSEQ, NEVMAX, xEXPNO, xRUNNO, xEVTNO, IEVSEQ
750     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to event sequence # over.        ',/,
     >' *   at  event sequence #   of ',I8,                           /,
     >' *       requested to stop  at ',I8,                           /,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
#ifdef UNIX
        KRET = -9998
        CALL PRGEVT(KRET)
#endif
        GO TO 990
C
      ELSEIF( xEVTNO .GE. NLSTEV ) THEN
        WRITE(6,760) xEVTNO, NLSTEV, xEXPNO, xRUNNO, xEVTNO, IEVSEQ
760     FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to event # over.                 ',/,
     >' *   at  event #            of ',I8,                           /,
     >' *       requested to stop  at ',I8,                           /,
     >' *   Last Exp#',I5,' Run#',I5,' Event#',I8,' Event Seq ',I8    /,
     >' ************************************************************',/,
     >   //)
#ifdef UNIX
        KRET = -9998
        CALL PRGEVT(KRET)
#endif
        GO TO 990
      ENDIF
      GO TO 200
C
C ----------------------------------------------------------------------
C (9) End of PRCEVT
C ----------------------------------------------------------------------
C
990   CONTINUE
      RETURN
      END
