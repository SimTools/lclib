C   04/12/86 708271734  MEMBER NAME  QOPEN0   (FORT)     M  FORTRAN
CC*********************************************************************
C*
C*  LOGICAL FUNCTION QOPEN( PROMPT,LUN,DSNAME,MODE,FRM,STS )
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
C*           CHARACTER * 44   DSNAME   (In)  default dataset name
C*                                     (Out) name of opened dataset
C*
C*  (Function Value) .True. for successful completion
C*                   .False. for can't open
C*
C*  (Relation)  Calls XOPEN
C*
C*  (Author)  A.Shirahahsi, Univ. of Tokyo
C*  (Date)    4-Dec-1986
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
C
C VAR
      INTEGER * 4  ILOG
      INTEGER * 4  LDS
C
C FUNC
      INTEGER * 4  CLILOG
      LOGICAL      XOPEN
C
C BEGIN
C     ... have CLI to stop logging
      ILOG = CLILOG( 0 )
C
C     ... inquire dataset name
      CALL TXTRD( PROMPT,DSNAME )
      ILOG = CLILOG( 0 )
C
      QOPEN = XOPEN( LUN,DSNAME,MODE,FRM,STS )
C
      IF( ILOG.EQ.1 ) THEN
        LDS = LENRD( DSNAME )
        CALL CLWLOG( DSNAME(:LDS) )
      END IF
C
C     ... restore CLI logging mode
      ILOG = CLILOG( 1 )
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
        CHARACTER * 44  FULNAM
        CHARACTER *  5  KMODE
        CHARACTER * 11  KFRM
        CHARACTER *  7  KSTS
        LOGICAL         LEXIST
        INTEGER   *  4  ERR131(2)
        INTEGER   *  4  LN, LP, LDS
        CHARACTER * 72  PROMPT
        LOGICAL         YES
C
C FUNC
        LOGICAL * 4  IFTSS
C
C
C BEGIN
        IF( LUN.LT.0 ) THEN
          XOPEN = .TRUE.
          RETURN
        END IF
C
        CALL ERRSAV( 131,ERR131 )
        CALL ERRSET( 131,256,-1,1 )
C
        LDS = LENRD( DSNAME )
        IF( LDS .EQ. 0 ) GOTO 9000
        CALL STRUPC( LDS,DSNAME )
C
        FULNAM = ' '
        IF( DSNAME(1:1) .EQ. '.' ) THEN
          CALL GETUID( FULNAM(1:4) )
          FULNAM(5:) = DSNAME
        ELSE
          FULNAM = DSNAME
        END IF
        JS = INDEX( FULNAM,'*' )
        IF( JS .GT. 0 ) THEN
          CALL XLISTC( FULNAM,JS-1,IERR )
          IF( IERR .NE. 0 ) GOTO 9000
        END IF
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
        IF( FRM(1:1).EQ.'U' .OR. FRM(1:1).EQ.'u' ) THEN
          KFRM = 'UNFORMATTED'
        ELSE IF( FRM(1:1).EQ.'F' .OR. FRM(1:1).EQ.'f' ) THEN
          KFRM = 'FORMATTED'
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
        IF( KMODE(1:1).EQ.'R' ) THEN
          INQUIRE( FILE=FULNAM,EXIST=LEXIST,ERR=9000 )
          IF( .NOT.LEXIST ) GOTO 9000
          OPEN( UNIT=LUN,FILE=FULNAM,
     &          ACTION=KMODE,FORM=KFRM,STATUS=KSTS,ERR=9000 )
        ELSE IF( KMODE(1:1).EQ.'W' .OR. KMODE(1:1).EQ.'B' ) THEN
          INQUIRE( FILE=FULNAM,EXIST=LEXIST,ERR=9000 )
          IF( LEXIST .AND. IFTSS() ) THEN
            LN = LENRD( FULNAM )
            PROMPT = SQ//FULNAM(1:LN)//SQ//' Already Exists, Delete ?'
            LP = LENRD( PROMPT )
            YES = .FALSE.
            CALL AFFIRM( PROMPT(1:LP),YES )
            IF( YES ) THEN
              CALL IPFCMD( IRET1,IRET2,
     &                     'DELETE '//SQ//FULNAM(1:LN)//SQ,LN+9 )
              IF( IRET1.NE.0 .OR. IRET2.NE.0 ) GOTO 9000
            ELSE
              GOTO 9000
            ENDIF
          END IF
          OPEN( UNIT=LUN,FILE=FULNAM,
     &          ACTION=KMODE,FORM=KFRM,STATUS=KSTS,ERR=9000 )
        END IF
C
        DSNAME = FULNAM
        XOPEN = .TRUE.
        GOTO 9900
C
9000    XOPEN = .FALSE.
        GOTO 9900
C
9900    CALL ERRSTR( 131,ERR131 )
        RETURN
        END
C
C
        SUBROUTINE XLISTC( NAME,N,IERR )
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
        CHARACTER * 44  PREFIX, DSNAME( MAXDSN )
C
C BEGIN
      IF( N .EQ. 0 ) THEN
        L = 4
        CALL GETUID( PREFIX(1:L) )
      ELSE
        IF( NAME(1:1) .EQ. '.' ) THEN
          L = 4
          CALL GETUID( PREFIX(1:L) )
        ELSE
          L = 0
        END IF
        DO 20 J = N, 1, -1
          IF( NAME(J:J) .EQ. '.' ) THEN
            M = J - 1
            GOTO 30
          END IF
20      CONTINUE
        M = N
30      CONTINUE
        IF( M .GE. 1 ) THEN
          PREFIX(L+1:L+M) = NAME(1:M)
        END IF
        L = L + M
      END IF
C
      NO = MAXDSN
      PREFIX(L+1:) = ' '
      CALL LISTC( PREFIX,DSNAME,NO )
      IF( NO .GT. MAXDSN ) THEN
        PRINT *, '%QOPEN-W, Too Many Datasets'
        NO = MAXDSN
      END IF
      IF( NO .EQ. 0 ) GOTO 9000
C
      IF( N .GT. 0 .AND. NAME(N:N) .NE. '.' ) THEN
        J = 1
        K = 0
40      CONTINUE
          IF( DSNAME(J)(L+1:L+N-M) .EQ. NAME(M+1:N) ) THEN
C%%         PRINT *, 'L+1,L+N-M,DSNAME=',L+1,L+N-M,DSNAME(J)(L+1:L+N-M)
C%%         PRINT *, 'M+1,N,NAME=',M+1,N,NAME(M+1:N)
            K = K + 1
            DSNAME(K) = DSNAME(J)
          END IF
          J = J + 1
          IF( J .GT. NO ) GOTO 50
        GOTO 40
50      CONTINUE
        NO = K
        IF( NO .EQ. 0 ) GOTO 9000
      END IF
C
      WRITE( 6,'(1H )' )
      DO 10 J = 1, NO
        WRITE( 6,'(1H ,I3,1H ,A44)' ) J,DSNAME(J)
10    CONTINUE
      WRITE( 6,'(1H )' )
C     ... always inquire on console
      CALL CLSTAR( 1 )
      CALL INTRD( '?Select Number',K )
      IF( K .LE. 0 .OR. K .GT. NO ) GOTO 9000
      NAME(1:44) = DSNAME(K)
      IERR = 0
      RETURN
9000  IERR = -1
      RETURN
      END
