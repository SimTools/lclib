C   02/07/85 907261500  MEMBER NAME  CHARD    (FORT)     M  FORTRAN
C
C
C ********************************************************************
C
C      THIS MEMBER CONTAINS THE FOLLOWING ROUTINES.
C
C      THIS MODULE IS MACHINE DEPENDENT.
C
C      SUBROUTINE CHARD( K,K2 )
C      ENTRY ARGRD( QUESTN,ANSWER )
C      ENTRY LINRD(IFLQQ,IFCRR)
C      ENTRY FLGRD(IFLQQ,IFCRR)
C      ENTRY FLARD(IFLQQ,IFCRR)
C      ENTRY CTZRD(KCTZ)
C      ENTRY CLINSQ( LINEIQ, LENIQ )
C      ENTRY CLLAST( LENGTH )
C      SUBROUTINE CLGETL( QUES,LINE,LAST )
C      SUBROUTINE CLIEOF()
C      SUBROUTINE CLMKPR( QUEST,ANSWER,QUECOM,QUES,LQ )
C      SUBROUTINE CLICMD( LINE1,LAST1 )
C      SUBROUTINE CLWLOG( LINE )
C      INTEGER FUNCTION CLILOG( SWITCH )
C      INTEGER FUNCTION CLILVL*4( /IDUMMY/ )
C      SUBROUTINE CLSTAR( IICOM )
C      INTEGER FUNCTION CLGETS( LUN,BUFFER,LENGTH )
C      SUBROUTINE CLIERR( LEVEL,MESAGE )
C      SUBROUTINE CLSTOP
C      LOGICAL FUNCTION IFATN()
C      SUBROUTINE RSTATN()
C      SUBROUTINE CLHINS( LINE,LENGTH )
C      SUBROUTINE CLHREC( IDIR,LINE,LENGTH )
C      SUBROUTINE CLHALL()
C      SUBROUTINE CLTGET( QUES,LINE,LENGTH )
C
C  Update Historty
C
C  27/JUL/89  A.Shirahashi, PF4 as 'resume execution'
C  16/SEP/89  A.Shirahashi, add subroutine OPNRD
C  18/SEP/89  A.Shirahashi, @% as '%output off' and @# as '%output on'
C
C *********************************************************************
C
C ---------
C   CHARD   ... get one character
C ---------
       SUBROUTINE CHARD( K,K2 )
C
C COMMON
       include '_unitrd.inc'
       include '_flagrd.inc'
       include '_tvdisp.inc'
       include '_clitxt.inc'
       EXTERNAL CLIBLK
C
C ARG
       CHARACTER *  1   K, K2
       CHARACTER * (*)  QUESTN, ANSWER
       INTEGER   *  4   IFLQQ, IFCRR
       CHARACTER * (*)  LINEIQ
       INTEGER   *  4   LENIQ
C
C VAR
       INTEGER  LAST,LTEMP,LEVEL,NLINES,LL,LQ,
     &          KRD,KWR,KCTZ,INLUN,IOPEN,LEN,IERR
       LOGICAL  ASK, ECHO, ECHRD, BATCH, COMAND, IFLOG
C
       CHARACTER * 256  LINE
       CHARACTER * 80   LINE1
       CHARACTER * 80   QTEMP, DSNAME
       CHARACTER CTRL
       PARAMETER (NBFILE = 4)
       CHARACTER INFILE(0:NBFILE)*32
       INTEGER * 4  HELSET, TTCHAN, TTEF, LUNIT(0:NBFILE)
       INTEGER * 4  ISTAT
       CHARACTER * 1   KOLD
C
C FUNC
       INTEGER  LENRD, LKBRD, CLGETS
       LOGICAL  IFTSS, IFATN
C
C BEGIN
#ifdef HIUXF77
       CALL CLIBLK
#endif
       IF( IPNT.GT.LAST ) GOTO 880
       K = LINE(IPNT:IPNT)
       KOLD = K
       IPNT = IPNT + 1
       K2 = LINE(IPNT:IPNT)
C
CCC    IF (IPNT.EQ.2 .AND. K.EQ.'?' .AND. (HELSET.AND.4).EQ.0) THEN
CCC      K = ' '
CCC      CALL GET_HELP
CCC    END IF
C
       IF (K.EQ.'\\') THEN
         IFLQ = 1
         IPNT = 0
       END IF
CCC    IF (K.EQ.'@') GOTO 2
       RETURN
C
880    CONTINUE
       K = CHAR(13)
       K2 = K
       KOLD = K
C
       RETURN
C
C ---------
C   ARGRD   ... get a new line
C ---------
       ENTRY ARGRD( QUESTN,ANSWER )
C
C BEGIN
       QUEST = QUESTN
       ANSWE = ANSWER
C      ... check if attention accepted
       IF( IFATN() ) THEN
         CALL RSTATN()
         IF( LUNPTR .GT. 0 ) CALL CLSTOP()
       END IF
C
C      ... check if the buffer is exausted.
       IF( IPNT.NE.0 .AND. IPNT.GT.LAST .AND. IFCR.NE.0 ) THEN
         IFLQ = 1
         IPNT = 0
       END IF
C
C      ... if buffer is not empty, just return
       IF( IPNT.NE.0 ) RETURN
C
100    CONTINUE
C      ... get one logical line
       CALL CLGETL( LINE,LAST )
       IFLQ = 0
       ICOM = 0
C
C      ... check if this line is EOF
       IF( LAST .LT. 0 ) THEN
         CALL CLIEOF()
         IPNT = 0
         LAST = 0
         IFLQ = 1
         GOTO 100
       END IF
C
200    CONTINUE
       IPNT = 1
C
C      ... parse CLI command
       IF( LAST.GT.0 .AND. LINE(1:1).EQ.'@' ) THEN
         CALL CLICMD( LINE,LAST )
C        ... get next line
         IPNT = 0
         LAST = 0
         IFLQ = 1
         GOTO 100
       END IF
C
CDBG   WRITE( 6,* ) 'ILOG,LUNIN=',ILOG,LUNIN
C@     IF( IFLOG ) CALL CLWLOG( LINE1(1:LAST1) )
C
C      ... return to caller
       RETURN
C
C ---------
C   LINRD   ... define command line flags
C ---------
       ENTRY LINRD(IFLQQ,IFCRR)
C
       IPNT = 0
       IFLQ = IFLQQ
       IFCR = IFCRR
       RETURN
C
C ---------
C   FLGRD
C ---------
       ENTRY FLGRD(IFLQQ,IFCRR)
C
       IFLQ = IFLQQ
       IFCR = IFCRR
       RETURN
C
C ---------
C   FLARD
C ---------
       ENTRY FLARD(IFLQQ,IFCRR)
C
       IFLQQ = IFLQ
       IFCRR = IFCR
       RETURN
C
C..... CHANGE LOGICAL UNITS
C*
C      ENTRY LUNRD(KRD,KWR)
C
C      IRD = KRD
C      IWR = KWR
C      RETURN
C*
C..... DEFINE HANDLING OF ^Z
C
       ENTRY CTZRD(KCTZ)
C
       ICTZ = KCTZ
       RETURN
C
C ----------
C   CLINSQ   ... put string into input queue
C ----------
       ENTRY CLINSQ( LINEIQ, LENIQ )
C
       CALL RSTATN()
C
       LINE(1:LENIQ) = LINEIQ(1:LENIQ)
       LINE(LENIQ+1:) = ' '
       LAST = LENIQ
       IPNT = 1
       IFLQ = 0
C
       GOTO 200
C
C ----------
C   CLLAST
C ----------
       ENTRY CLLAST( LENGTH )
C
C BEGIN
       LENGTH = MAX( LAST+1-IPNT,0 )
       RETURN
       END
C
C ----------
C   CLGETL   ... read one logical line
C ----------
       SUBROUTINE CLGETL( LINE,LAST )
C
C ARG
C      (Ouput)
       CHARACTER * (*)  LINE
       INTEGER   *  4   LAST
C
C COMMON
       include '_unitrd.inc'
       include '_flagrd.inc'
       include '_tvdisp.inc'
       include '_clitxt.inc'
C
C VAR
       LOGICAL   *  4  LASK, LMODE, LECHO, LTSSIN
       CHARACTER * 80  LINE1
       INTEGER   *  4  LAST1
       INTEGER   *  4  AID
       CHARACTER * 80  ANS
C
C FUNC
       INTEGER * 4  CLGETS, LENRD
       LOGICAL * 4  IFTSS
C
C BEGIN
10     CONTINUE
       LUNIN = LUNLST(LUNPTR)
       IF( ICOM .EQ. 1 ) LUNIN = 5
       LTSSIN = IFTSS() .AND. LUNIN.EQ.5
       LASK = IFLQ.NE.0 .AND. LTSSIN
       LMODE = LTVLOC .AND. LTSSIN
       LECHO = .NOT.LTVLOC .AND. .NOT.LTSSIN
C
       LAST = 0
100    CONTINUE
C
       CALL CLMKPR( QUEST,ANSWE,QUECOM,QUES,LQ )
       IF( LASK ) THEN
         IF( LMODE ) THEN
           CALL TVPROM( QUES(1:LQ) )
         ELSE
           CALL PROMPT( 2,QUES(1:LQ),IRET )
         END IF
       END IF
C
       IF( LMODE ) THEN
C%%      CALL TGET( LINE1,80,'E,W',IRET,LAST1 )
         CALL CLTGET( QUES(1:LQ),LINE1,LAST1 )
         IF( LAST1.LT.0 ) GOTO 10
         ISTS = 0
       ELSE
         ISTS = CLGETS( LUNIN,LINE1,LAST1 )
       END IF
       IF( ISTS .LT. 0 ) THEN
         LAST = -1
         RETURN
       END IF
C
       CALL CLMACR( LINE1,LAST1 )
C
       IF( LUNPTR.NE.0 .AND. ISNGL.EQ.1 ) THEN
         IF( LTVLOC ) THEN
           CALL TVEDIT( QUES(1:LQ),LINE1,LAST1,AID )
           IF( LAST1.EQ.0 .AND. LENRD(LINE1).GT.0 ) LAST1=LENRD(LINE1)
         ELSE
           CALL PROMPT( 2,QUES(1:LQ)//' '//LINE1(:LAST1)//' =>',IRET )
           ISTS = CLGETS( 5,ANS,LANS )
           CALL STRUPC( 1,ANS )
           IF( LANS.EQ.0 .OR. ANS(1:1).EQ.'G' ) THEN
           ELSE IF( ANS(1:1).EQ.'S' ) THEN
             GOTO 100
           ELSE IF( ANS(1:1).EQ.'R' ) THEN
             CALL PROMPT( 2,QUES(1:LQ),IRET )
             ISTS = CLGETS( 5,LINE1,LAST1 )
           ELSE IF( ANS(1:1).EQ.'A' ) THEN
             CALL CLSTOP()
             GOTO 10
           END IF
         END IF
       END IF
C
       IF( LECHO .AND. .NOT.(LUNPTR.NE.0 .AND. ISNGL.EQ.1) .AND.
     &     .NOT.(LAST1.GE.2 .AND. LINE1(1:2).EQ.'@*') ) THEN
         LQ = LENRD( QUES )
         WRITE( 6,'(1H ,A,A,A)' ) QUES(1:LQ),' ',LINE1(1:LAST1)
       END IF
C
       IF( INHLOG.EQ.0 .AND. LINE1(1:1).NE.'@' ) THEN
         CALL CLWLOG( LINE1(1:LAST1) )
       END IF
C
       IF( LINE1(LAST1:LAST1) .EQ. '-' ) THEN
         ICON = 1
         LAST1 = LAST1 - 1
       ELSE
         ICON = 0
       END IF
       IF( LAST+LAST1 .GT. LEN(LINE) ) THEN
         PRINT *, '%CLI-E, Too long line, truncated.'
         RETURN
       END IF
       IF( LAST1 .GE. 1 ) THEN
         LINE(LAST+1:LAST+LAST1) = LINE1(1:LAST1)
         LAST = LAST + LAST1
       END IF
       IF( ICON .EQ. 1 ) GOTO 100
C
       CALL CLHINS( LINE,LAST )
C
       RETURN
       END
C
C ----------
C   CLIEOF   ... end of file detected
C ----------
       SUBROUTINE CLIEOF()
C
       include '_unitrd.inc'
       include '_flagrd.inc'
       include '_tvdisp.inc'
       include '_clitxt.inc'
C
C BEGIN
       IF( LUNPTR .EQ. 0 ) THEN
         CALL CLIERR( 1,'End of file on console input, why ?' )
       ELSE
         IF( .NOT.LOPNRD ) THEN
           CALL CFARGP
         END IF
         LOPNRD = .FALSE.
         CALL CLCLOS( LUNLST(LUNPTR),IERR )
         IF( IERR .NE. 0 ) THEN
           CALL CLIERR( 1,'Can''t close command file' )
         ELSE IF( .NOT. LTVLOC ) THEN
           CALL CLMKPR( QUEST,ANSWE,QUECOM,QUES,LQ )
           IF( QUES(LQ:LQ).EQ.' ' ) LQ = LQ - 1
           WRITE( 6,'(1H ,A,A,A)') QUES(1:LQ),' ','(END OF FILE)'
         END IF
         LUNPTR = LUNPTR - 1
       END IF
C
       RETURN
       END
C
C ----------
C   CLMKPR   ... build prompt string
C ----------
       SUBROUTINE CLMKPR( QUEST,ANSWER,QUECOM,QUES,LQ )
C
C ARG
       CHARACTER * (*)  QUEST, ANSWER, QUECOM
       CHARACTER * (*)  QUES
       INTEGER   *  4   LQ
C
C COMMON
       include '_flagrd.inc'
C
C VAR
       CHARACTER * 80  QTEMP
C
C BEGIN
       LQ = LENRD( QUEST )
       IF( QUEST(1:1).EQ.'?' ) THEN
         IF( INDEX(QUEST,'>')+INDEX(QUEST(2:),'?').EQ.0 ) THEN
           QUES = QUEST(2:LQ)//' ? '
         ELSE
           QUES = QUEST(2:LQ)
         END IF
         ANSWER = ' '
       ELSE
         LL = LKBRD( ANSWER,LEN(ANSWER) )
         IF( INDEX( QUEST,'>' )+INDEX( QUEST,':' ) .EQ. 0 ) THEN
           QUES = QUEST(1:LQ)//': '//ANSWER(1:LL)//' ? '
         ELSE
           QUES = QUEST(1:LQ)//' '//ANSWER(1:LL)//' ? '
         END IF
       END IF
C
       LQ = LENRD( QUES )
CVMS
       LQ = LQ + 1
       QUES(LQ:LQ) = ' '
CVMS
       IF( ICOM .EQ. 1 ) THEN
         QTEMP(1:LQ+1) = '*'//QUES(1:LQ)
         QUES(1:LQ+1) = QTEMP(1:LQ+1)
         LQ = LQ + 1
         LC = LENRD( QUECOM )
         IF( LC .GT. 0 ) THEN
           QUES(LQ+1:LQ+LC) = QUECOM(1:LC)
           LQ = LQ + LC
           IF( QUES(LQ:LQ) .NE. ' ' ) THEN
             LQ = LQ + 1
             QUES(LQ:LQ) = ' '
           END IF
         END IF
       END IF
       IF( ICON .EQ. 1 ) THEN
         QTEMP(1:LQ+1) = '_'//QUES(1:LQ)
         QUES(1:LQ+1) = QTEMP(1:LQ+1)
         LQ = LQ + 1
       END IF
C
       RETURN
       END
C
C ----------
C   CLICMD   ... Execute CLI Local Command
C ----------
       SUBROUTINE CLICMD( LINE1,LAST1 )
C
C ARG
       CHARACTER * (*)  LINE1
       INTEGER   *  4   LAST1
C
C COMMON
       include '_flagrd.inc'
       include '_unitrd.inc'
       include '_clitxt.inc'
C
C VAR
       LOGICAL   *  4  LOGON
       CHARACTER * 80  DSNAME
       INTEGER   *  4  LDSN, IERR
       CHARACTER * 80  LINEU
C
C BEGIN
       LINEU = LINE1
CRI       CALL STRUPC( 80,LINEU )
       LOGON = .FALSE.
C
C%%    IF( ICOM .EQ. 1 ) THEN
C%%      CALL CLIERR( 2,'Unexpected CLI command' )
C%%    ELSE IF( LINE1(2:2) .EQ. '(' ) THEN
       IF( LINE1(2:2) .EQ. '(' ) THEN
C          ... Open Log File
         IF( ILOG .EQ. 0 ) THEN
CMSP           CALL CLOPEN( LINE1(3:LAST1),KLUN,'W',IERR )
CVMS
           CALL CLOPEN( LINE1(3:LAST1),KLUN,'W',IERR,'.com' )
CVMS
           IF( IERR .EQ. 0 ) ILOG = 1
         ELSE
           CALL CLIERR( 2,'Log file already opened' )
         END IF
       ELSE IF( LINE1(2:2) .EQ. ')' ) THEN
C          ... Close Log File
         CALL CLCLOS( KLUN,IERR )
         IF( IERR .NE. 0 ) THEN
           CALL CLIERR( 2,'Can''t close log file' )
         END IF
         ILOG = 0
       ELSE IF( LINE1(2:2) .EQ. '*' ) THEN
C          ... Inquire String from Console in Command File
         IF( LUNPTR.EQ.0 ) THEN
           CALL CLIERR( 1,'Inquire in direct mode, why ?' )
         ELSE
           IF( LAST1 .GE. 3 ) THEN
             QUECOM = LINE1(3:LAST1)
           ELSE
             QUECOM = ' '
           END IF
           ICOM = 1
         END IF
       ELSE IF( LINE1(2:2).EQ.'<' ) THEN
C          ... suspend/resume the execuation of the command procedure
           IF( LPTRSV.EQ.0 ) THEN
             LPTRSV = LUNPTR
             LUNPTR = 0
           ELSE
             LUNPTR = LPTRSV
             LPTRSV = 0
           END IF
       ELSE IF( LINE1(2:2).EQ.'\\' .OR. LINE1(2:2).EQ.'$' ) THEN
C          ... Execute TSS Command
         CALL IPFCMD( IRET1,IRET2,LINE1(3:LAST1),LAST1-2 )
         LOGON = .TRUE.
       ELSE IF( LINE1(2:2).EQ.'!' .OR. LINE1(2:2).EQ.'|' ) THEN
C          ... Comment
C          ... Do Nothing
         LOGON = .TRUE.
       ELSE IF( LINE1(2:2) .EQ. '>' ) THEN
C          ... Redirect standard output to file
         LOGON = .TRUE.
         CLOSE( 6 )
         IF( LAST1.EQ.2 ) THEN
CMSP           OPEN( UNIT=6,FILE='*' )
CVMS
           OPEN( UNIT=6,FILE='SYS$OUTPUT',STATUS='UNKNOWN' )
CVMS
         ELSE IF( LINE1(3:LAST1).EQ.'*' .OR.
     &            LINEU(3:LAST1).EQ.'TERMINAL' ) THEN
CMSP           OPEN( UNIT=6,FILE='*' )
CVMS
           OPEN( UNIT=6,FILE='SYS$OUTPUT',STATUS='UNKNOWN' )
CVMS
         ELSE IF( LINEU(3:LAST1).EQ.'NULLFILE' ) THEN
CMSP           OPEN( UNIT=6,FILE='NULLFILE' )
CVMS
           OPEN( UNIT=6,FILE='NLA0:',STATUS='UNKNOWN' )
CVMS
         ELSE
CMSP           CALL CLOPEN( LINEU(3:LAST1),6,'W',IERR )
CVMS
           CALL CLOPEN( LINEU(3:LAST1),6,'W',IERR,'.LIS' )
CVMS
           IF( IERR .NE. 0 ) THEN
CMSP             OPEN( UNIT=6,FILE='*' )
CVMS
             OPEN( UNIT=6,FILE='SYS$OUTPUT',STATUS='UNKNOWN' )
CVMS
             CALL CLIERR( 2,'Can''t redirect standard output' )
             LOGON = .FALSE.
           END IF
         END IF
       ELSE IF( LINE1(2:2) .EQ. '?' ) THEN
C          ... Debug On/Off
         IDEBUG = .NOT. IDEBUG
         LOGON = .TRUE.
       ELSE IF( LINE1(2:2) .EQ. '#' ) THEN
C         ... preprocessor command / output on
         IF( LINEU(3:8).EQ.'DEFINE' ) THEN
           CALL CLMACI( LINE1(10:LAST1) )
         ELSE IF( LAST1.EQ.2 ) THEN
CMSP           OPEN( UNIT=6,FILE='*' )
CVMS
           OPEN( UNIT=6,FILE='SYS$OUTPUT',STATUS='NEW' )
CVMS
           WRITE( 6,'(1H ,A)' ) '%output on'
         END IF
       ELSE IF( LINE1(2:2) .EQ. '%' ) THEN
         WRITE( 6,'(1H ,A)' ) '%output off'
CMSP         OPEN( UNIT=6,FILE='NULLFILE' )
CVMS
         OPEN( UNIT=6,FILE='NLA0:',STATUS='NEW' )
CVMS
       ELSE
C          ... Redirect command input to indirect file
         IF( LINE1(2:2).EQ.'@' ) THEN
C            ... Single step execution
           DSNAME = LINE1(3:LAST1)
           LAST1 = LAST1 - 1
           LINE1(2:LAST1) = DSNAME
           ISNGL = 1
         ELSE
           ISNGL = 0
         END IF
         IF( LUNPTR .GE. MAXNST ) THEN
           CALL CLIERR( 2,'Command files nests too deep' )
         ELSE
           LOGON = .TRUE.
           LUNPTR = LUNPTR + 1
           CALL CFARGS( LINE1(2:LAST1),DSNAME,LDSN )
CMSP           CALL CLOPEN( DSNAME(1:LDSN),LUNLST(LUNPTR),'R',IERR )
CVMS
           CALL CLOPEN(DSNAME(1:LDSN),LUNLST(LUNPTR),'R',IERR,'.COM')
CVMS
           IF( IERR .NE. 0 ) THEN
             IF( LUNPTR .GT. 0 ) THEN
               LUNPTR = LUNPTR - 1
             END IF
             LOGON = .FALSE.
           END IF
         END IF
       END IF
C        ... write log if necessary
       IF( LOGON .AND. INHLOG.EQ.0 ) THEN
         CALL CLWLOG( LINE1(1:LAST1) )
       END IF
C
       RETURN
       END
C
C ----------
C   CLWLOG   ... write on log file
C ----------
       SUBROUTINE CLWLOG( LINE )
C
C ARG
       CHARACTER * (*)  LINE
C
C COMMON
       include '_unitrd.inc'
       include '_flagrd.inc'
C
C BEGIN
       IF( ILOG.EQ.1 .AND. LUNIN.EQ.5 .AND. ICOM.EQ.0 ) THEN
         WRITE( KLUN,'(A)' ) LINE
       END IF
C
       RETURN
       END
C
C ----------
C   CLILOG
C ----------
       INTEGER FUNCTION CLILOG( SWITCH )
C
C ARG
C      (Input)
       INTEGER * 4  SWITCH
C
C COMMON
       include '_unitrd.inc'
       include '_flagrd.inc'
C
C BEGIN
       IF( SWITCH .EQ. 0 ) THEN
         INHLOG = 1
       ELSE
         INHLOG = 0
       END IF

       IF( ( ILOG.EQ.1 ) .AND. ( LUNPTR.EQ.0 .OR. ICOM.EQ.1 ) ) THEN
         CLILOG = 1
       ELSE
         CLILOG = 0
       END IF
       RETURN
       END
C
C ----------
C   CLILVL
C ----------
       INTEGER FUNCTION CLILVL*4( IDUMMY )
C
       include '_unitrd.inc'
       include '_flagrd.inc'
C
       CLILVL = LUNPTR
       RETURN
       END
C
C ----------
C   CLSTAR
C ----------
       SUBROUTINE CLSTAR( IICOM )
C
C ARG
       INTEGER * 4  IICOM
C
C COMMON
       include '_flagrd.inc'
       include '_clitxt.inc'
C
C BEGIN
       ICOM = IICOM
       QUECOM = ' '
       RETURN
       END
C
C ---------
C   OPNRD
C ---------
      SUBROUTINE OPNRD(FILE)
C
C ARG
      CHARACTER * (*)  FILE
C
C COMMON
      include '_flagrd.inc'
      include '_unitrd.inc'
C
C BEGIN
      IF( LUNPTR .GE. MAXNST ) THEN
        CALL CLIERR( 2,'Command files nests too deep' )
        RETURN
      END IF
C
      ISNGL = 0
      LUNPTR = LUNPTR + 1
      LFIL = LENRD( FILE )
CMSP      CALL CLOPEN( FILE(1:LFIL),LUNLST(LUNPTR),'R',IERR )
CVMS
      CALL CLOPEN( FILE(1:LFIL),LUNLST(LUNPTR),'R',IERR,'.COM' )
CVMS
      IF( IERR .NE. 0 ) THEN
        IF( LUNPTR .GT. 0 ) THEN
          LUNPTR = LUNPTR - 1
        END IF
      ELSE
        LOPNRD = .TRUE.
      END IF
C
      RETURN
      END
C
C ----------
C   CLTGET   .... get a line from console with history/recall
C ----------
       SUBROUTINE CLTGET( QUES,LINE,LENGTH )
C
C ARG
       CHARACTER * (*)  QUES, LINE
       INTEGER   *  4   LENGTH
C
C CONST
       include '_key_def.inc'
C
C COMMON
       include '_unitrd.inc'
C
C VAR
       INTEGER   *  4  AID, CSRPOS(2), NFIELD, FLDPOS(2)
       CHARACTER * 80  BUFFER
C
C BEGIN
       LINE = ' '
       LENGTH = 0
       CALL TVINPT( AID,CSRPOS,1,NFIELD,FLDPOS,LENGTH,LINE )
C
       CALL CLHREC( 0,LINE,LENGTH )
100    CONTINUE
       IF( AID.EQ.PF1 ) THEN
C        ... recall previous
         CALL CLHREC( -1,LINE,LENGTH )
       ELSE IF( AID.EQ.PF2 ) THEN
C        ... recall next
         CALL CLHREC( +1,LINE,LENGTH )
       ELSE IF( AID.EQ.PF3 ) THEN
C        ... recall all
         CALL CLHALL()
         LENGTH = 0
       ELSE IF( AID.EQ.PF4 ) THEN
C        ... resume execution
         IF( LPTRSV.NE.0 ) THEN
           LUNPTR = LPTRSV
           LPTRSV = 0
         END IF
         LENGTH = -1
         RETURN
       ELSE
         RETURN
       END IF
C
       CALL TVEDIT( QUES,LINE,LENGTH,AID )
       GOTO 100
C
       END
C
C ----------
C   CLHINS   .... history/install
C ----------
       SUBROUTINE CLHINS( LINE,LENGTH )
C
C ARG
       CHARACTER * (*)  LINE
       INTEGER * 4  LENGTH
C
C CONST
       PARAMETER( MAXHIS = 20 )
C
C COMMON
       COMMON / CLHIS1 / HISBUF( 0:MAXHIS-1 )
       CHARACTER * 80  HISBUF
       COMMON / CLHIS2 / IHEAD, ITAIL, NENT
       INTEGER * 4  IHEAD, ITAIL, NENT
C
C BEGIN
       IF( IHEAD.EQ.ITAIL .AND. NENT.GE.MAXHIS ) THEN
         ITAIL = MOD( ITAIL+1,MAXHIS )
       ELSE
         NENT = NENT + 1
       END IF
       HISBUF( IHEAD ) = LINE(:LENGTH)
       IHEAD = MOD( IHEAD+1,MAXHIS )
C
       RETURN
       END
C
C ----------
C   CLHREC   .... history/recall
C ----------
       SUBROUTINE CLHREC( IDIR,LINE,LENGTH )
C
C ARG
       INTEGER   *  4  IDIR, LENGTH
       CHARACTER * (*) LINE
C
C CONST
       PARAMETER( MAXHIS = 20 )
C
C COMMON
       COMMON / CLHIS1 / HISBUF( 0:MAXHIS-1 )
       CHARACTER * 80  HISBUF
       COMMON / CLHIS2 / IHEAD, ITAIL, NENT
       INTEGER * 4  IHEAD, ITAIL, NENT
C
C BEGIN
       IF( IDIR.EQ.0 ) THEN
         IRECAL = IHEAD
         NRECAL = 0
         RETURN
       END IF
C
       IF( NENT.EQ.0 ) THEN
         LENGTH = 0
         RETURN
       END IF
C
       IF( IDIR.LT.0 ) THEN
         IF( NRECAL.LT.NENT ) THEN
           NRECAL = NRECAL + 1
           IRECAL = IRECAL - 1
           IF( IRECAL.LT.0 ) IRECAL = MAXHIS-1
         END IF
         LINE = HISBUF( IRECAL )
         LENGTH = LENRD( LINE )
       ELSE IF( IDIR.GT.0 ) THEN
         IF( NRECAL.EQ.0 ) THEN
           LENGTH = 0
           RETURN
         ELSE IF( NRECAL.GT.1 ) THEN
           NRECAL = NRECAL - 1
           IRECAL = MOD( IRECAL+1,MAXHIS )
         END IF
         LINE = HISBUF( IRECAL )
         LENGTH = LENRD( LINE )
       END IF
C
       RETURN
       END
C
C ----------
C   CLHALL   .... recall/all
C ----------
       SUBROUTINE CLHALL()
C
C CONST
       PARAMETER( MAXHIS = 20 )
C
C COMMON
       COMMON / CLHIS1 / HISBUF( 0:MAXHIS-1 )
       CHARACTER * 80  HISBUF
       COMMON / CLHIS2 / IHEAD, ITAIL, NENT
       INTEGER * 4  IHEAD, ITAIL, NENT
C
C BEGIN
       IF( NENT.EQ.0 ) RETURN
       CALL TVCLR
       PRINT *
C
       J = ITAIL
       DO 100 I = 1, NENT
         L = LENRD( HISBUF( J ) )
         PRINT '(1X,I3,1X,A)', I, HISBUF( J )(:L)
         J = MOD( J+1,MAXHIS )
100    CONTINUE
C
       RETURN
       END
C
C ----------
C   CLGETS   .... read a line from input stream
C ----------
       INTEGER FUNCTION CLGETS( LUN,BUFFER,LENGTH )
C
C ARG
       INTEGER * 4  LUN
       CHARACTER * ( * )  BUFFER
       INTEGER * 4  LENGTH
C
C VAR
       CHARACTER * 80  ARGS
       INTEGER   *  4  LARGS, RECLEN
C
C COMMON
       include '_cliio.inc'
C
C BEGIN
C      ... read one line
       IF( ITRML.GT.0 ) THEN
         L = MIN( ITRML, LEN(BUFFER) )
       ELSE
         L = LEN(BUFFER)
       END IF
10     CONTINUE
       READ( LUN,'(A)',END=800 ) BUFFER(:L)
C      ... deprive trailing blnaks
       RECLEN = 72
       DO 110 LENGTH = RECLEN,1,-1
         IF( BUFFER(LENGTH:LENGTH) .NE. ' ' ) GOTO 120
110    CONTINUE
       LENGTH = 0
120    CONTINUE
       BUFFER(LENGTH+1:) = ' '
C
C      ... replace arguments
       I = 0
200    CONTINUE
         I = I + 1
         IF( I .GE. LENGTH ) GOTO 210
         IF( BUFFER(I:I) .NE. '%' ) GOTO 200
         IARG = INDEX( '123456789',BUFFER(I+1:I+1) )
         IF( IARG .EQ. 0 ) GOTO 200
         CALL CFARGI( IARG,ARGS,LARGS )
         ARGS(LARGS+1:) = BUFFER(I+2:LENGTH)
         BUFFER(I:) = ARGS(:LARGS+LENGTH-I-1)
         LENGTH = LENGTH + LARGS - 2
         I = I + LARGS - 1
       GOTO 200
210    CONTINUE
C
C      ... deprive inline comment
       IF( LENGTH .GT. 0 ) THEN
         JAE1 = INDEX( BUFFER(1:LENGTH),'@!' )
         JAE2 = INDEX( BUFFER(1:LENGTH),'@|' )
         JAE = MAX( JAE1,JAE2 )
         IF( JAE .GT. 0 ) THEN
           BUFFER(JAE:) = ' '
           LENGTH = JAE - 1
           IF( LENGTH .EQ. 0 ) GOTO 10
         END IF
         JE1= INDEX( BUFFER(1:LENGTH),'!' )
         JE2= INDEX( BUFFER(1:LENGTH),'|' )
         JE = MAX( JE1,JE2 )
         IF( JE .GT. 0 ) THEN
           BUFFER(JE:) = ' '
           LENGTH = JE - 1
           IF( LENGTH .EQ. 0 ) GOTO 10
         END IF
       END IF
C
C      ... for debug
C      WRITE( 6,* ) 'CLGETS:',LENGTH,' <',BUFFER(1:LENGTH),'>'
C
       LENGTH = LENRD( BUFFER )
       CLGETS = 0
       RETURN
800    CLGETS = -1
       RETURN
       END
C
       SUBROUTINE CLTRML( LENGTH )
C
C ARG
       INTEGER * 4  LENGTH
C
C COMMON
       include '_cliio.inc'
C
C BEGIN
       ITRML = LENGTH
       RETURN
       END
C
C ----------
C   CLSTOP   ... abort command procedure
C ----------
       SUBROUTINE CLSTOP
C
C COMMON
       include '_unitrd.inc'
       include '_flagrd.inc'
C
C BEGIN
C     ... close all command files
      DO 100 J = 1, LUNPTR
        CALL CLCLOS( LUNLST(J),IERR )
100   CONTINUE
C
C     ... restore input stream
      LUNPTR = 0
CMSP      OPEN( UNIT=5,FILE='*' )
CVMS
      OPEN( UNIT=5,FILE='SYS$INPUT',STATUS='UNKNOWN' )
CVMS
C     ... discard remaining command line
C%    CALL LINRD( 1,1 )
      IPNT = 0
      IFLQ = 1
      IFCR = 1
C
      RETURN
      END
C
C
C      LOGICAL FUNCTION IFATN()
C
C      INCLUDE '_COMODE.INC'
C
C      IF( ICOMER .EQ. 1 ) THEN
C        IFATN = .FALSE.
C      ELSE
C        IFATN = .TRUE.
C      END IF
C      RETURN
C      END
C
C
C      SUBROUTINE RSTATN()
C
C      INCLUDE '_COMODE.INC'
C
C      ICOMER = 1
C      RETURN
C      END
C
C
#ifdef HIUXF77
	SUBROUTINE CLIBLK
#else
      BLOCK DATA CLIBLK
#endif
C
C COMMON
       include '_unitrd.inc'
C
      DATA IRD / 5 /, IWR / 6 /, TLUN / 88 /, KLUN / 89 /
      DATA LUNPTR / 0 /
      DATA LUNLST / 5,81,82,83,84,85,86,87,0 /
C
       include '_flagrd.inc'
C
      DATA IFLQ / 1 /, IFCR / 1 /
      DATA ICOM / 0 /, ILOG / 0 /, INHLOG / 0 /
      DATA IPNT / 0 /, IFF / 0 /, ICTZ / 1 /
      DATA IDEBUG / .FALSE. /
      DATA ICON / 0 /
C
       include '_cliio.inc'
C
      DATA IUNGET / 0 /
      DATA IEOF / 0 /
C
      END
