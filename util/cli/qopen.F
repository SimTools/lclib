C   04/12/86 810282314  MEMBER NAME  QOPEN    (FORT)     M  FORTRAN
C
C THIS MODULE IS HEAVILY MACHINE DEPENDENT.
C
CC*********************************************************************
C*
C*  LOGICAL FUNCTION QOPEN( PROMPT,LUN,DSNAME,MODE,FRM,STS,
C*                          /ICONT/,/SELECT/ )
C*
C*
C*  (Purpose) Inquire filename on console and open it
C*
C*  (Input)  CHARACTER * (*)  PROMPT   prompt string, CLI convention
C*           INTEGER   *   4  LUN      logical unit number
C*                                     if LUN < 0, QOPEN will NOT open
C*                                     specified dataset actually.
C*           CHARACTER * (*)  MODE     'READ', 'WRITE', 'BOTH'
C*           CHARACTER * (*)  FRM      'UNFORMATTED', 'FORMATTED'
C*           CHARACTER * (*)  STS      'OLD', 'NEW', 'SHR', 'UNKNOWN'
C*
C*  (Input/Output)
C*           CHARACTER * 54   DSNAME   (In)  default dataset name
C*                                     (Out) name of opened dataset
C*           INTEGER   *  4   ICONT    Context switch
C*                                     must be 0 for first call
C*
C*  (Output)
C*           CHARACTER * (*)  SELECT   Command string used to select
C*                                     this dataset.
C*
C*  (Function Value) .True. for successful completion
C*                   .False. for can't open
C*
C*  (Screen mode)  If first character of PROMPT or DSNAME is '$',
C*
C*  (Relation)  Calls XOPEN, YOPEN
C*
C*  (Author)  A.Shirahahsi, Univ. of Tokyo
C*  (Date)    4-Dec-1986
C*            5-Sep-1987, full screen version
C*           13-Mar-1988, FB format
C*           20-Sep-1988, return SELECT
C*           29-Sep-1988, VBS format
C*
CC**********************************************************************
C
      LOGICAL FUNCTION QOPEN( PROMPT,LUN,DSNAME,MODE,FRM,STS )
C
C ARG
C     (Input)
      CHARACTER * (*)  PROMPT
      INTEGER   *   4  LUN
      CHARACTER * (*)  MODE, FRM, STS
C     (Input/Output)
      CHARACTER * (*)  DSNAME
      INTEGER   *   4  ICONT
C
C VAR
      INTEGER * 4  ILOG, IMODE, LDS
      CHARACTER *  6  SSEL
      SAVE  IMODE
C
C FUNC
      INTEGER * 4  CLILOG
      LOGICAL * 4  XOPEN, YOPEN
C
C BEGIN
      NARGC = 6
      GOTO 1
C
      ENTRY QOPENL( PROMPT,LUN,DSNAME,MODE,FRM,STS,ICONT )
C
      NARGC = 7
      GOTO 1
C
1     CONTINUE
C
      IF( NARGC .GE. 7 ) THEN
        IICONT = ICONT
      ELSE
        IICONT = 0
      END IF
C
C     ... have CLI to stop logging
      ILOG = CLILOG( 0 )
C
      SSEL = ' '
C
      IF( IICONT .EQ. 0 ) THEN
        IF( PROMPT(1:1).NE.'$' ) THEN
          CALL TXTRD( PROMPT,DSNAME )
          ILOG = CLILOG( 0 )
          IF( DSNAME(1:1).NE.'$' ) THEN
            IMODE = 0
            QOPEN = XOPEN( LUN,DSNAME,MODE,FRM,STS )
          ELSE
            IMODE = 1
            QOPEN = YOPEN( LUN,DSNAME,MODE,FRM,STS,IICONT,SSEL )
          END IF
        ELSE
          IMODE = 1
          QOPEN = YOPEN( LUN,DSNAME,MODE,FRM,STS,IICONT,SSEL )
        END IF
      ELSE
        IF( IMODE .EQ. 0 ) THEN
          QOPEN = .FALSE.
        ELSE
          QOPEN = YOPEN( LUN,DSNAME,MODE,FRM,STS,IICONT,SSEL )
        END IF
      END IF
C
      IF( ILOG.EQ.1 ) THEN
        LDS = LENRD( DSNAME )
        CALL CLWLOG( DSNAME(:LDS) )
      END IF
C
C     ... restore CLI logging mode
      ILOG = CLILOG( 1 )
C
      IF( NARGC .GE. 7 ) THEN
        IF( IMODE .EQ. 0 ) THEN
          ICONT = 1
        ELSE
          ICONT = 2
        END IF
      END IF
C
CMSP      IF( NARGC .GE. 8 ) THEN
CMSP        SELECT = SSEL
CMSP      END IF
C
      RETURN
      END
C
CC**********************************************************************
C
C  LOGICAL FUNCTION XOPEN(LUN,DSNAME,MODE,FRM,STS )
C
C  (Purpose) Open dataset
C            When '*' is included in the filename, datasets are searched
C            that match the given name and inquire on the console which
C            dataset to be selected.
C
C (Input)    LUN     logical unit number
C                    if LUN < 0, XOPEN will NOT open the dataset
C            MODE    'READ' / 'WRITE'
C            FRM     'UNFORMATTED' / 'FORMATTED'
C            STS     'OLD' / 'NEW' / 'SHR' / 'UNKNOWN'
C (In/Out)   DSNAME  (In)  dataset name to be opened
C                    (Out) dataset name to have been opened
C (Output)   Function return value  .True.  = Succesful Completion
C                                   .False. = Can't Open
C
C (Author)   A.Shirahasi, Univ. of Tokyo
C (Date)     18-Nov-1986
C (Update)   3-Dec-1986, bug fixed
C            4-Dec-1986, change for QOPEN
C
CC**********************************************************************
C
        LOGICAL FUNCTION XOPEN( LUN,DSNAME,MODE,FRM,STS )
C
C ARG
        INTEGER   * 4    LUN
        CHARACTER * (*)  DSNAME, MODE, FRM, STS
C
C CONST
        CHARACTER * 1  SQ
        PARAMETER( SQ = '''' )
C
C VAR
        CHARACTER * 54  FULNAM
C
C FUNC
        LOGICAL * 4  OPENIT
C
C BEGIN
        IF( LUN.LT.0 ) THEN
          XOPEN = .TRUE.
          RETURN
        END IF
C
CMSP        CALL ERRSAV( 131,ERR131 )
CMSP        CALL ERRSET( 131,256,-1,1 )
C
        LDS = LENRD( DSNAME )
        IF( LDS .EQ. 0 ) GOTO 9000
C        CALL STRUPC( LDS,DSNAME )
C
        FULNAM = ' '
        IF( DSNAME(1:1) .EQ. '.' ) THEN
#if defined(G77_521_OR_GREATER)
          IDUSER = GETUID()
          CALL UVCOPY(IDUSER,FULNAM,1)
#else
          CALL GETUID( FULNAM(1:4) )
#endif
          FULNAM(5:) = DSNAME
        ELSE
          FULNAM = DSNAME
        END IF
        JS = INDEX( FULNAM,'*' )
        IF( JS .GT. 0 ) THEN
CMSP          CALL CLISTC( FULNAM,JS-1,IERR )
CVMS
          CALL CLISTC( FULNAM,LENRD(FULNAM),IERR )
CVMS
          IF( IERR .NE. 0 ) GOTO 9000
        END IF
C
        IF( OPENIT( LUN,FULNAM,MODE,FRM,STS ) ) THEN
          DSNAME = FULNAM
          XOPEN = .TRUE.
          GOTO 9900
        END IF
C
9000    XOPEN = .FALSE.
        GOTO 9900
C
9900    CONTINUE
CMSP        CALL ERRSTR( 131,ERR131 )
        RETURN
        END
C
C
        SUBROUTINE CLISTC( NAME,N,IERR )
C
C ARG
        CHARACTER * (*)  NAME
        INTEGER          N, IERR
C
C CONST
        INTEGER * 4  MAXDSN
        PARAMETER( MAXDSN = 200 )
C
C VAR
        CHARACTER * 44  DSNAME( MAXDSN )
C
C BEGIN
      CALL FINDIT( NAME,N,DSNAME,MAXDSN,NO )
      WRITE( 6,'(1H )' )
      DO 10 J = 1, NO
        WRITE( 6,'(1H ,I3,1H ,A44)' ) J,DSNAME(J)
10    CONTINUE
      WRITE( 6,'(1H )' )
C     ... always inquire on console
      CALL CLSTAR( 1 )
      CALL INTRD( '?Select number',K )
      IF( K .LE. 0 .OR. K .GT. NO ) GOTO 9000
      NAME = ' '
      NAME(1:44) = DSNAME(K)
      IERR = 0
      RETURN
9000  IERR = -1
      RETURN
      END
C
C
      SUBROUTINE FINDIT( NAME,N,DSNAME,MAXDSN,NDSN )
C
C ARG
C     (Input)
      CHARACTER * (*)  NAME
      INTEGER   * 4    N, MAXDSN
C     (Output)
      CHARACTER * (*)  DSNAME( MAXDSN )
      INTEGER   * 4    NDSN
C
C VAR
      CHARACTER * 44   PREFIX
C
C BEGIN
      IF( N .GT. 0 ) THEN
C        CALL STRUPC( N,NAME )
CMSP        IF( NAME(N:N) .EQ. '*' ) THEN
CMSP          N = N - 1
CMSP        END IF
      END IF
C
CMSP      IF( N .EQ. 0 ) THEN
CMSP        L = 4
CMSP        CALL GETUID( PREFIX(1:L) )
CMSP      ELSE
CMSP        IF( NAME(1:1) .EQ. '.' ) THEN
CMSP          L = 4
CMSP          CALL GETUID( PREFIX(1:L) )
CMSP        ELSE
CMSP          L = 0
CMSP        END IF
CMSP        DO 20 J = N, 1, -1
CMSP          IF( NAME(J:J) .EQ. '.' ) THEN
CMSP            M = J - 1
CMSP            GOTO 30
CMSP          END IF
CMSP20      CONTINUE
CMSP        M = N
CMSP30      CONTINUE
CMSP        IF( M .GE. 1 ) THEN
CMSP          PREFIX(L+1:L+M) = NAME(1:M)
CMSP        END IF
CMSP        L = L + M
CMSP      END IF
CVMS
      L = N
      PREFIX(:L) = NAME(:L)
CVMS
      NO = MAXDSN
      PREFIX(L+1:) = ' '
      CALL LISTC( PREFIX,DSNAME,NO )
      IF( NO .GT. MAXDSN ) THEN
        PRINT *, '%QOPEN-W, Too many datasets'
        NO = MAXDSN
      END IF
      IF( NO .EQ. 0 ) GOTO 9000
C
CMSP      IF( N .GT. 0 .AND. NAME(N:N) .NE. '.' ) THEN
CMSP        J = 1
CMSP        K = 0
CMSP40      CONTINUE
CMSP          IF( M+1.GT.N ) THEN
CMSP            K = K + 1
CMSP            DSNAME(K) = DSNAME(J)
CMSP          ELSE IF( DSNAME(J)(L+1:L+N-M) .EQ. NAME(M+1:N) ) THEN
C%%         PRINT *, 'L+1,L+N-M,DSNAME=',L+1,L+N-M,DSNAME(J)(L+1:L+N-M)
C%%         PRINT *, 'M+1,N,NAME=',M+1,N,NAME(M+1:N)
CMSP    	        K = K + 1
CMSP            DSNAME(K) = DSNAME(J)
CMSP          END IF
CMSP          J = J + 1
CMSP          IF( J .GT. NO ) GOTO 50
CMSP        GOTO 40
CMSP50      CONTINUE
CMSP        NO = K
CMSP        IF( NO .EQ. 0 ) GOTO 9000
CMSP      END IF
C
      NDSN = NO
      RETURN
C
9000  NDSN = 0
      RETURN
C
      END
C
C
      LOGICAL FUNCTION YOPEN( LUN,DSNAME,MODE,FRM,STS,ICONT,SELECT )
C
C ARG
      INTEGER   * 4    LUN, ICONT
      CHARACTER * (*)  DSNAME, MODE, FRM, STS, SELECT
C
C CONST
      INTEGER * 4  MAXDSN
      PARAMETER( MAXDSN = 200 )
C
C VAR
      CHARACTER * 44  DSN, DSNS( MAXDSN )
      CHARACTER * 54  FULNAM, DSLIST( MAXDSN )
      INTEGER   *  4  NSEL, ISEL( MAXDSN ), KSEL
      CHARACTER *  6  SSEL( MAXDSN )
      SAVE  DSNS, NSEL, ISEL, KSEL
      CHARACTER *  8  MEMSEL( 100 )
C
C FUNC
      LOGICAL * 4  OPENIT
CMSP      INTEGER * 4  DSNCHK
C
C BEGIN
      IF( ICONT .EQ. 0 ) THEN
        IF( DSNAME(1:1) .EQ. '$' ) THEN
          N = LENRD( DSNAME ) - 1
          IF( N.GT.0 ) THEN
            DSN = DSNAME(2:N+1)
          ELSE
            DSN = ' '
          END IF
        ELSE
          N = LENRD( DSNAME )
          DSN = DSNAME(:N)
        END IF
        CALL FINDIT( DSN,N,DSNS,MAXDSN,NDSN )
        IF( NDSN.EQ.1 .AND. DSN.EQ.DSNS(1) ) THEN
          DSLIST(1) = DSNS(1)
          ISEL(1) = 1
          NSEL = 1
          KSEL = 1
        ELSE
          CALL TVCLR
          CALL XXMENU( DSNS,NDSN,NSEL,ISEL,SSEL )
          CALL TVCLR
          DO 100 I = 1, NSEL
            DSLIST(I) = DSNS(ISEL(I))
            ISEL(I) = I
100       CONTINUE
          NMAX = NSEL
          KSEL = 1
        END IF
      END IF
C
      IF( KSEL .GT. NSEL ) THEN
        YOPEN = .FALSE.
        SELECT = ' '
      ELSE
110     FULNAM = DSLIST(ISEL(KSEL))
        LDSN = LENRD( FULNAM )
CMSP        IDSN = DSNCHK( ''''//FULNAM(:LDSN)//'''' )
CMSP        IF( IDSN.EQ.1 ) THEN
CMSP          NMEM = 100
CMSP          CALL CLISTD( FULNAM, '$', NMEM, MEMSEL )
CMSP          IF( NMEM.EQ.0 ) THEN
CMSP            YOPEN = .FALSE.
CMSP            RETURN
CMSP          END IF
CMSP          DO 120 I = NSEL, KSEL+1, -1
CMSP            ISEL(I+NMEM-1) = ISEL(I)
CMSP120       CONTINUE
CMSP          DO 130 I = 1, NMEM
CMSP            NMAX = NMAX + 1
CMSP            ISEL(KSEL+I-1) = NMAX
CMSP            LMEM = LENRD( MEMSEL(I) )
CMSP            DSLIST(NMAX) = FULNAM(:LDSN)//'('//MEMSEL(I)(:LMEM)//')'
CMSP130       CONTINUE
CMSP          NSEL = NSEL+NMEM-1
CMSP          GOTO 110
CMSP        END IF
        YOPEN = OPENIT( LUN,FULNAM,MODE,FRM,STS )
        DSNAME = FULNAM
        SELECT = SSEL(KSEL)
        KSEL = KSEL + 1
      END IF
C
      RETURN
      END
C
C
      SUBROUTINE XXMENU( DSNS,NDSN,NSEL,ISEL,SSEL )
C
C ARG
C     (Input)
      CHARACTER * (*)  DSNS( NDSN )
C     (Output)
      INTEGER   *  4   NSEL, ISEL( * )
      CHARACTER *  6   SSEL( * )
C
C CONST
       include '_key_def.inc'
      INTEGER * 4  MAXDSN, MAXSIZ
      PARAMETER( MAXDSN = 200, MAXSIZ = 25 )
C
C VAR
      INTEGER   * 4  ROWS(2), COLS(2), NN(2)
      CHARACTER * 6  CMDS(MAXDSN)
C
      EQUIVALENCE( ITOP,NN(1) )
      EQUIVALENCE( IEND,NN(2) )
C
      INTEGER   * 4  AID, CSRPOS(2), NFIELD
      INTEGER   * 4  FLDPOS(2,MAXSIZ), LENGTH(MAXSIZ), MODIFY(MAXSIZ)
      CHARACTER * 6  BUFFER(MAXSIZ)
      CHARACTER * 1  C
C
      DATA ROWS / 5,23 /
      DATA COLS / 1,8 /
      DATA NROW / 19 /
C
C BEGIN
      ITOP = 1
      IEND = MIN( NROW,NDSN )
      DO 100 I = 1, MAXSIZ
        MODIFY(I) = 2
100   CONTINUE
      DO 110 I = 1, NDSN
        CMDS(I) = '......'
110   CONTINUE
C
      CALL XXDRAW
C
200   CONTINUE
      CALL TVLOC1( 5,2 )
      CALL TVMENU( ROWS,COLS,CMDS,DSNS,NDSN,NN,MODIFY )
C
      CALL TVINPT( AID,CSRPOS,MAXSIZ,NFIELD,FLDPOS,LENGTH,BUFFER )
C
      IF( NFIELD .EQ. 0 ) THEN
        CALL TVSCRL( AID,ITOP,IEND,NDSN,NROW,IRET )
      ELSE
        DO 300 I = 1, NFIELD
          IROW = FLDPOS(1,I)
          MODIFY(IROW) = IOR( MODIFY(IROW),1 )
          IH = IROW - ROWS(1) + NN(1)
          C = BUFFER(I)(1:1)
          IF( C.EQ.'X' .OR. C.EQ.'x' .OR. C.EQ.' ' .OR. C.EQ.'.' ) THEN
            CMDS(IH) = '......'
          ELSE
            CMDS(IH) = BUFFER(I)(1:6)
            CALL STRUPC( 6,CMDS(IH) )
            J = INDEX( CMDS(IH),'.' )
            IF( J.GT.0 ) THEN
              CMDS(IH)(J:) = ' '
            END IF
          END IF
300     CONTINUE
      END IF
C
      IF( AID.NE.PF3 ) GOTO 200
C
      NSEL = 0
      DO 400 I = 1, NDSN
        IF( CMDS(I)(1:1) .NE. '.' ) THEN
          NSEL = NSEL + 1
          ISEL(NSEL) = I
          SSEL(NSEL) = CMDS(I)
        END IF
400   CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE XXDRAW
C
C VAR
      CHARACTER * 78  LINES, GUIDE1, GUIDE2
      DATA GUIDE1
     & / '  SELECT DATASET(S) ==> PUT "S" IN COL 1 AND PRESS "PF3"'/
      DATA GUIDE2
     & / '  PF7/PF8 FOR PREV/NEXT PAGE,  PF12/PF24 FOR SCROLL UP/DOWN' /
      CHARACTER * 2000  TVBUF
C
C BEGIN
      WRITE( LINES,'(78A1)' ) ('-',I=1,78)
      CALL TVPUTL( 'WCC',TVBUF,LTV )
      CALL TVPUTL( 'PH',TVBUF,LTV,1,1,LINES )
      CALL TVPUTL( 'PH',TVBUF,LTV,2,1,GUIDE1 )
      CALL TVPUTL( 'PH',TVBUF,LTV,3,1,GUIDE2 )
      CALL TVPUTL( 'PH',TVBUF,LTV,4,1,LINES )
      CALL TVPUTL( 'PH',TVBUF,LTV,24,1,LINES )
      CALL TVPUTL( 'PUT',TVBUF,LTV )
C
      RETURN
      END
C
C
      LOGICAL FUNCTION OPENIT( LUN,DSN,MODE,FRM,STS )
C
C (Update)
C     10-Feb-88, Ask member name for PO file without member name.
C                Note: DSN may be overwritten in this routine.
C     28-Sep-88, Can create a new member in an old PO dataset.
C
C ARG
C     (Input)
      INTEGER * 4  LUN
      CHARACTER * (*)  DSN, MODE, FRM, STS
C
C CONST
      CHARACTER * 1  SQ
      PARAMETER( SQ = '''' )
C
C VAR
      CHARACTER * 54  DSNAME
      CHARACTER *  8  MEMBER, MEMSEL
      CHARACTER *  5  KMODE
      CHARACTER * 11  KFRM
      CHARACTER *  7  KSTS
      LOGICAL         LEXIST
      INTEGER   *  4  LN, LP
      CHARACTER * 72  PROMPT
      LOGICAL         YES
C
      DATA NSPACE / 2 /
C
C FUNC
      LOGICAL * 4  IFTSS
      INTEGER * 4  DSNCHK
C
C BEGIN
      IF( LUN .LT. 0 ) THEN
        OPENIT = .TRUE.
        RETURN
      END IF
C
      DSNAME = DSN
      LDSN = LENRD( DSNAME )
CMSP      IDSN = DSNCHK( ''''//DSNAME(:LDSN)//'''' )
CMSP      IF( IDSN.EQ.1 ) THEN
CMSP        CALL TXTRD( '?Member name',MEMBER )
CMSP        CALL STRUPC( 8,MEMBER )
CMSP        LMEM = LENRD( MEMBER )
CMSP        IF( LMEM.EQ.0 ) THEN
CMSP          OPENIT = .FALSE.
CMSP          RETURN
CMSP        END IF
CMSP        IF( MEMBER(:LMEM).EQ.'$'.OR.INDEX(MEMBER(:LMEM),'*').GT.0 ) THEN
CMSP          NSEL = 1
CMSP          CALL CLISTD( DSNAME(:LDSN),MEMBER,NSEL,MEMSEL )
CMSP          IF( NSEL.EQ.0 ) GOTO 9000
CMSP        ELSE
CMSP          MEMSEL = MEMBER
CMSP        END IF
CMSP        LMEM = LENRD( MEMSEL )
CMSP        DSNAME(LDSN+1:) = '('//MEMSEL(:LMEM)//')'
CMSP        DSN = DSNAME
CMSP      END IF
C
      IF( MODE(1:1).EQ.'R' .OR. MODE(1:1).EQ.'r' ) THEN
        KMODE = 'READ'
      ELSE IF( MODE(1:1).EQ.'W' .OR. MODE(1:1).EQ.'w' ) THEN
        KMODE = 'WRITE'
      ELSE IF( MODE(1:1).EQ.'B' .OR. MODE(1:1).EQ.'b' ) THEN
        KMODE = 'BOTH'
      ELSE
        GOTO 9000
      END IF
C
      LFRM = LENRD( FRM )
      IF( FRM(1:1).EQ.'U' .OR. FRM(1:1).EQ.'u' ) THEN
        KFRM = 'UNFORMATTED'
      ELSE IF( FRM(1:1).EQ.'F' .OR. FRM(1:1).EQ.'f' ) THEN
        KFRM = 'FORMATTED'
      ELSE IF( FRM(:LFRM).EQ.'FB' .OR. FRM(:LFRM).EQ.'VBS' ) THEN
C       ... FB/VBS format
      ELSE
        GOTO 9000
      END IF
C
      IF( STS(1:1).EQ.'N' .OR. STS(1:1).EQ.'n' ) THEN
        KSTS = 'NEW'
      ELSE IF( STS(1:1).EQ.'O' .OR. STS(1:1).EQ.'o' ) THEN
        KSTS = 'OLD'
      ELSE IF( STS(1:1).EQ.'S' .OR. STS(1:1).EQ.'s' ) THEN
        KSTS = 'SHR'
      ELSE IF( STS(1:1).EQ.'U' .OR. STS(1:1).EQ.'u' ) THEN
        KSTS = 'UNKNOWN'
      ELSE
        GOTO 9000
      END IF
C
      IF( (IDSN.EQ.1 .OR. IDSN.EQ.3) .AND. KSTS(1:3).EQ.'NEW' ) THEN
        KSTS = 'OLD'
      END IF
C
      IF( KMODE(1:1).EQ.'R' ) THEN
        INQUIRE( FILE=DSNAME,EXIST=LEXIST,ERR=9000 )
        IF( .NOT.LEXIST ) GOTO 9000
CMSP        OPEN( UNIT=LUN,FILE=DSNAME,
CMSP     &        ACTION=KMODE,FORM=KFRM,STATUS=KSTS,ERR=9000 )
CVMS
        IF( KMODE(1:1).EQ.'R' ) THEN
#if defined(HIUXF77) || defined(MACHTEN_PPC) || defined(AIX)
          OPEN( UNIT=LUN,FILE=DSNAME,
     &          FORM=KFRM,STATUS=KSTS,ERR=9000 )
#else
          OPEN( UNIT=LUN,FILE=DSNAME,
     &          READONLY,FORM=KFRM,STATUS=KSTS,ERR=9000 )
#endif
        ELSE
          OPEN( UNIT=LUN,FILE=DSNAME,
     &          FORM=KFRM,STATUS=KSTS,ERR=9000 )
        END IF
CVMS
      ELSE IF( KMODE(1:1).EQ.'W' .OR. KMODE(1:1).EQ.'B' ) THEN
CMSP        INQUIRE( FILE=DSNAME,EXIST=LEXIST,ERR=9000 )
CMSP        IF( LEXIST .AND. IFTSS() ) THEN
CMSP          LN = LENRD( DSNAME )
CMSP          PROMPT = SQ//DSNAME(1:LN)//SQ//' already exists, delete ?'
CMSP          LP = LENRD( PROMPT )
CMSP          YES = .FALSE.
CMSP          CALL AFFIRM( PROMPT(1:LP),YES )
CMSP          IF( YES ) THEN
CMSP            CALL IPFCMD( IRET1,IRET2,
CMSP     &                   'DELETE '//SQ//DSNAME(1:LN)//SQ,LN+9 )
CMSP            IF( IRET1.NE.0 .OR. IRET2.NE.0 ) GOTO 9000
CMSP          ELSE
CMSP            GOTO 9000
CMSP          ENDIF
CMSP        END IF
CMSP        IF( FRM(:LFRM).EQ.'FB' .AND. INDEX(DSNAME,'(').EQ.0 ) THEN
CMSP          CALL INTRD( 'Allocation size (TRK)',NSPACE )
CMSP          CALL UALCPS( LUN, DSNAME, 'NEW', 'FB', NSPACE, NRET )
CMSP          IF( NRET.NE.0 ) GOTO 9000
CMSP        ELSE IF( FRM(:LFRM).EQ.'VBS' .AND. INDEX(DSNAME,'(').EQ.0 ) THEN
CMSP          CALL INTRD( 'Allocation size (TRK)',NSPACE )
CMSP          CALL UALCPS( LUN, DSNAME, 'NEW', 'VBS', NSPACE, NRET )
CMSP          IF( NRET.NE.0 ) GOTO 9000
CMSP        ELSE
CMSP          OPEN( UNIT=LUN,FILE=DSNAME,
CMSP     &          ACTION=KMODE,FORM=KFRM,STATUS=KSTS,ERR=9000 )
CVMS
	  IF( KMODE(1:1).EQ.'R' ) THEN
#if defined(HIUXF77) || defined(MACHTEN_PPC) || defined(AIX)
            OPEN( UNIT=LUN,FILE=DSNAME,
     &            FORM=KFRM,STATUS=KSTS,ERR=9000 )
#else
            OPEN( UNIT=LUN,FILE=DSNAME,
     &            READONLY,FORM=KFRM,STATUS=KSTS,ERR=9000 )
#endif
          ELSE
            OPEN( UNIT=LUN,FILE=DSNAME,
     &            FORM=KFRM,STATUS=KSTS,ERR=9000 )
          END IF
CVMS
CMSP        END IF
      END IF
C
      OPENIT = .TRUE.
      RETURN
C
9000  OPENIT = .FALSE.
      PRINT *, DSNAME
      RETURN
      END
C
C
CMSP      SUBROUTINE CLISTD( DSNAME, MEMBER, NSEL, MEMSEL )
C
CMSP      CHARACTER * (*)  DSNAME
CMSP      CHARACTER * (*)  MEMBER
CMSP      CHARACTER * 8  MEMSEL(*)
CMSP      INTEGER * 4  NSEL
C
CMSP      CHARACTER * 44  DSN
CMSP      CHARACTER *  8  MEM, MEMLST(1000)
CMSP      CHARACTER *  1  CHOICE(1000)
CMSP
CMSP      NMAX = NSEL
CMSP      N = 1000
CMSP      DSN = DSNAME
CMSP      CALL LISTD( DSN, MEMLST, N, IRET )
CMSP      LMEM = LENRD( MEMBER )
CMSP      IF( MEMBER(:LMEM).EQ.'$' ) THEN
CMSP        MEM = '*'
CMSP      ELSE IF( MEMBER(1:1).EQ.'$' ) THEN
CMSP        MEM = MEMBER(2:)
CMSP      ELSE
CMSP        MEM = MEMBER
CMSP      END IF
CMSP      I = INDEX( MEM,'*' )
CMSP      IF( I.GE.2 ) THEN
CMSP        J = 0
CMSP        DO 100 K = 1, N
CMSP          IF( MEMLST(K)(:I-1).EQ.MEM(:I-1) ) THEN
CMSP            J = J + 1
CMSP            MEMLST(J) = MEMLST(K)
CMSP          END IF
CMSP100     CONTINUE
CMSP        N = J
CMSP      END IF
CMSP      IF( MEMBER(1:1).EQ.'$' ) THEN
CMSP        CALL CLSMEM( DSN, MEMLST, N, CHOICE )
CMSP        NSEL = 0
CMSP        DO 200 I = 1, N
CMSP          IF( CHOICE(I).EQ.'S' .AND. NSEL.LT.NMAX ) THEN
CMSP            NSEL = NSEL + 1
CMSP            MEMSEL(NSEL) = MEMLST(I)
CMSP          END IF
CMSP200     CONTINUE
CMSP      ELSE
CMSP        N1 = N / 6
CMSP        N2 = N - N1 * 6
CMSP        DO 210 I = 0, N1-1
CMSP          PRINT '(6(1X,I3,1X,A8))', ((I*6+J,MEMLST(I*6+J)),J=1,6)
CMSP210     CONTINUE
CMSP        PRINT '(6(1X,I3,1X,A8))', ((N1*6+J,MEMLST(N1*6+J)),J=1,N2)
CMSP        CALL INTRD( '?Select number',K )
CMSP        IF( K.LE.0 .OR. K.GT.N ) GOTO 9000
CMSP        NSEL = 1
CMSP        MEMSEL(NSEL) = MEMLST(K)
CMSP      END IF
CMSP      RETURN
CMSP
CMSP9000  CONTINUE
CMSP      NSEL = 0
CMSP      RETURN
CMSP      END
