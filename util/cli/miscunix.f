C
C Miscellaneous I/O routines for VAX/VMS
C
      LOGICAL  FUNCTION  IFTSS()
C
      include 'jpidef.inc'
C      
CVMS      CALL LIB$GETJPI( JPI$_MODE,,,IRET )
C
CVMS      IF( IRET .EQ. JPI$K_INTERACTIVE ) THEN
CVMS        IFTSS = .TRUE.
CVMS      ELSE
CVMS        IFTSS = .FALSE.
CVMS      END IF
CUNIX
      IFTSS = .TRUE.
CUNIX
C
      RETURN
      END
C
C
      SUBROUTINE IPFCMD( IRET1, IRET2, COMAND, LCMD )
C
C ARG
      INTEGER  IRET1, IRET2, LCMD
      CHARACTER * (*)  COMAND
C
C BEGIN
CVMS      CALL LIB$SPAWN( COMAND(:LCMD),,,,,IRET1 )
      IRET2 = IRET1
      RETURN
      END
C
C
      SUBROUTINE PROMPT( MODE, STRING, IRET )
C
C ARG
      INTEGER  MODE, IRET
      CHARACTER * (*)  STRING
C
C BEGIN
CCC      WRITE( *,'(1H$A)' ) STRING
C for HP Fortran
      WRITE( *,'($,A)' ) STRING
C
      RETURN
      END
C
C
C      SUBROUTINE STRUPC( LEN, STRING )
C
C ARG
C      INTEGER  LEN
C      CHARACTER * (*)  STRING
C
C BEGIN
CVMS      CALL STR$UPCASE( STRING,STRING(:LEN) )
C      RETURN
C      END
C
C
      SUBROUTINE TPUT( BUFFER,LENGTH,MODE,IRET )
C
C ARG
      CHARACTER * (*)  BUFFER, MODE
      INTEGER  LENGTH, IRET
C
C BEGIN
      WRITE(*,*) '*TPUT'
      RETURN
      END
C
C
      SUBROUTINE TGET( BUFFER, LBUF, MODE, IRET, LRET )
C
C ARG
      CHARACTER * (*)  BUFFER, MODE
      INTEGER  LBUF, IRET, LRET
C
C BEGIN
      WRITE(*,*) '*TGET'
      RETURN
      END
C
C
      LOGICAL  FUNCTION  IFATN()
C
      IFATN = .FALSE.
      RETURN
      END
C
C
      SUBROUTINE  RSTATN()
C
      RETURN
      END
C
C
      SUBROUTINE  LISTC( INPUT,OUTPUT,NO )
C
C ARG
      INTEGER * 4      NO
      CHARACTER * (*)  INPUT, OUTPUT(1)
C
C VAR
      INTEGER * 4  ICONT
C
C BEGIN
      ICONT = 0
      NMAX = NO
      NO = 0
100   CONTINUE
      IF( NO.GE.NMAX ) GOTO 900
CVMS      ISTAT = LIB$FIND_FILE( INPUT,OUTPUT(NO+1),ICONT )
      If( .NOT. BTEST( ISTAT,0 ) ) GOTO 900
      NO = NO + 1
      GOTO 100
900   CONTINUE
CVMS      CALL LIB$FIND_FILE_END( ICONT )
      RETURN
      END
C
C Help library
C
      SUBROUTINE DISABLE_HELP
C
      RETURN
      END
C
      SUBROUTINE ENABLE_HELP
C
      RETURN
      END
C
      SUBROUTINE GET_HELP
C
      RETURN
      END
C
      SUBROUTINE SET_HELP_FILE(FILE)
C
C ARG
      CHARACTER * (*)  FILE
C
C BEGIN
      RETURN
      END
C
      SUBROUTINE SET_HELP_KEY1(KEY)
C
C ARG
      CHARACTER * (*)  KEY
C
C BEGIN
      RETURN
      END
C
      SUBROUTINE SET_NO_HELP
C
      RETURN
      END
C
C Misc. library
C
      SUBROUTINE FILTYPE(FILE,EXT)
C
C ARG
      CHARACTER * (*)  FILE, EXT
C
C BEGIN
      RETURN
      END
C
      SUBROUTINE FORDCLI
C
      RETURN
      END
C
      SUBROUTINE SYMBOL_SUBSTITUTION(LINE,LAST,LENGTH,IERR)
C
C ARG
      CHARACTER * (*)  LINE
      INTEGER  LAST, LENGTH, IERR
C
C BEGIN
      RETURN
      END
C
      SUBROUTINE GETVERSION(FILE)
C
C ARG
      CHARACTER * (*)  FILE
C
C BEGIN
      RETURN
      END
C
      SUBROUTINE MSGER(STATUS,MESAGE)
C
C ARG
      INTEGER  STATUS
      CHARACTER * (*)  MESAGE
C
C BEGIN
      RETURN
      END
C C
C       SUBROUTINE DATIME(DATE)
C C
C C ARG 
C       CHARACTER * (*)  DATE
C C
C C BEGIN
C       RETURN
C       END
C
      SUBROUTINE CLI_STATUS_RETURN(STATUS)
C
C ARG
      INTEGER STATUS
C
C BEGIN
      RETURN
      END
