C   10/11/86 810250148  MEMBER NAME  CLOPEN   (FORT)     M  FORTRAN
C+
C (Description) Open Command File for CLI
C (Author     ) A.Shirahashi, Univ. of Tokyo
C (Date       )  6-Nov-1986
C (History    )  3-Dec-1986
C
C THIS MODULE IS MACHINE DEPENDENT.
C-
CMSP       SUBROUTINE CLOPEN( NAME,LUN,MODE,IERR )
CVMS
       SUBROUTINE CLOPEN( NAME,LUN,MODE,IERR,DEFNAM )
CVMS
C
C ARG
C      (Input)
       CHARACTER * ( * )  NAME, MODE
       INTEGER  LUN
CVMS
       CHARACTER * ( * )  DEFNAM
CVMS
C      (Output)
       INTEGER  IERR
C
C CONST
       CHARACTER * 1  SQ
       PARAMETER( SQ = '''' )
C
C VAR
       CHARACTER * 80  NAME1
       CHARACTER *  4  USERID
       CHARACTER * 64  FULNAM
       CHARACTER * 72  PROMPT
       CHARACTER *  8  MEMBER
       INTEGER  L1, L2, L, LNAME, LP
       LOGICAL * 4  LEXIST, YES
CMSP       INTEGER * 4  ERR131(2)
C
C FUNC
       LOGICAL * 4  IFTSS
       INTEGER * 4  DSNCHK
C
C BEGIN
CMSP       CALL GETUID( USERID )
       LNAME = LENRD( NAME )
       IF( LNAME .EQ. 0 ) GOTO 911
       NAME1(:LNAME) = NAME(:LNAME)
       NAME1(LNAME+1:) = ' '
CRI       CALL STRUPC( LNAME,NAME1 )
CMSP       L1 = INDEX( NAME1(1:LNAME),'(' )
CMSP       L2 = INDEX( NAME1(1:LNAME),'.' )
CMSP       IF( (L1 .GT. 0) .OR. (L2 .GT. 0) ) THEN
CMSP         .... DS Name specified
CMSP         IF( NAME1(1:1) .EQ. '.' ) THEN
CMSP           L = 4 + LNAME
CMSP           FULNAM(1:L) = USERID//NAME1(1:LNAME)
CMSP         ELSE
CMSP           L = LNAME
CMSP           FULNAM(1:L) = NAME1(1:LNAME)
CMSP         END IF
CMSP       ELSE
CMSP         .... Member Name specified
CMSP         L = 16 + LNAME
CMSP         FULNAM(1:L) = USERID//'.CLI.CLIST('//NAME1(1:LNAME)//')'
CMSP       END IF
CVMS
       L = LNAME
       FULNAM(:L) = NAME1(:LNAME)
       IF( INDEX(FULNAM(:L),'.').EQ.0 .AND. LENRD(DEFNAM).GT.0 ) THEN
         FULNAM(L+1:) = DEFNAM
         L = L + LENRD(DEFNAM)
       END IF              
CVMS 
C
C      ... for debug
C      WRITE( 6,* ) 'FULNAM:<',FULNAM(1:L),'>'
C      WRITE( 6,* ) 'DEFNAM:<',DEFNAM,'>'
C
C      CALL ERRSAV( 131,ERR131 )
C      CALL ERRSET( 131,256,-1,1 )
C
       IF( MODE(1:1) .EQ. 'R' .OR. MODE(1:1) .EQ. 'r' ) THEN
CMSP         IDSN = DSNCHK( ''''//FULNAM(:L)//'''' )
CMSP         IF( IDSN.EQ.1 ) THEN
CMSP           CALL TXTRD( '?Member name',MEMBER )
CMSP           CALL STRUPC( 8,MEMBER )
CMSP           LMEM = LENRD( MEMBER )
CMSP           IF( LMEM.EQ.0 ) GOTO 901
CMSP           IF( INDEX( MEMBER(:LMEM),'*' ).GT.0 ) THEN
CMSP             CALL CLISTD( FULNAM(:L),MEMBER,IERR )
CMSP             IF( IERR.LT.0 ) GOTO 901
CMSP             LMEM = LENRD( MEMBER )
CMSP           END IF
CMSP           FULNAM(L+1:) = '('//MEMBER(:LMEM)//')'
CMSP           L = L + LMEM + 2
CMSP         END IF
         INQUIRE( FILE=FULNAM(1:L),EXIST=LEXIST,ERR=901 )
         IF( .NOT. LEXIST ) GOTO 901
C%%      IF( IFTSS() ) THEN
C%%        CALL CLALOC( LUN,FULNAM(1:L),'SHR USING(IN)',IRET )
C%%        IF( IRET.NE.0 ) GOTO 901
C%%      ELSE
CMSP         OPEN( UNIT=LUN,FILE=FULNAM(1:L),ERR=901,STATUS='SHR',
CMSP     &         ACTION='READ' )
CVMS
#if defined(HIUXF77) || defined(MACHTEN_PPC) || defined(AIX)
         OPEN( UNIT=LUN,FILE=FULNAM(1:L),ERR=901,STATUS='OLD')
#else
         OPEN( UNIT=LUN,FILE=FULNAM(1:L),ERR=901,STATUS='OLD',
     &         READONLY )
#endif
CVMS
C%%      END IF
       ELSE IF( MODE(1:1) .EQ. 'W' .OR. MODE(1:1) .EQ. 'w' ) THEN
C%%      IDSN = DSNCHK( ''''//FULNAM(
CMSP           INQUIRE( FILE=FULNAM(1:L),EXIST=LEXIST,ERR=901 )
CMSP           IF( LEXIST .AND. IFTSS() ) THEN
CMSP           PROMPT = SQ//FULNAM(1:L)//SQ//' already exists, overwrite ?'
CMSP           YES = .FALSE.
CMSP           CALL AFFIRM( PROMPT,YES )
CMSP           IF( .NOT. YES ) GOTO 901
CMSP         END IF
CMSP         OPEN( UNIT=LUN,FILE=FULNAM(1:L),ACTION='WRITE',
CMSP     &         FORM='FORMATTED',STATUS='SHR',ERR=901 )
CVMS
         OPEN( UNIT=LUN,FILE=FULNAM(1:L),
     &         FORM='FORMATTED',STATUS='NEW',ERR=901 )
CVMS
       ELSE
         GOTO 902
       END IF
C
C .... Sucessful Completion
       IERR = 0
       GOTO 1000
C .... File Open Failure
901    CALL CLIERR( 2,'Can''t open '''//FULNAM(:L)//'''' )
       IERR = -1
       GOTO 1000
911    CALL CLIERR( 2,'Invalid filename specification' )
       IERR = -1
       GOTO 1000
C .... Access Mode Error
902    IERR = -2
       GOTO 1000
C
1000   CONTINUE
C      CALL ERRSTR( 131,ERR131 )
       RETURN
C
       END
C
C
       SUBROUTINE CLALOC( LUN,DSNAME,OPTION,IRET )
C
C VAR
       INTEGER   *  4   LUN
       CHARACTER * (*)  DSNAME, OPTION
       INTEGER   *  4   IRET
C
C VAR
       CHARACTER * 256  CMDLIN
C
C BEGIN
       LDSN = LENRD( DSNAME )
       LOPT = LENRD( OPTION )
       WRITE( CMDLIN,1000 ) LUN,DSNAME(:LDSN),OPTION(:LOPT)
1000   FORMAT( 'ALLOC DD(FT',I2.2,'F001) DSN(''',A,''') ',A )
       LCMD = LENRD( CMDLIN )
       CALL IPFCMD( IRET1,IRET2,CMDLIN,LCMD )
       IRET = IRET1 + IRET2
       IF( IRET.NE.0 ) RETURN
       OPEN( UNIT=LUN,ACCESS='SEQUENTIAL',STATUS='UNKNOWN',
     &       FORM='FORMATTED' )
       RETURN
       END

