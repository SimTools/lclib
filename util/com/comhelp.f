C   12/11/86 611251943  MEMBER NAME  COMHELP  (FORT)     M  FORTRAN
C+++++ This is COMHELP.FOR             B. Gabioud Nov 1982
C
       SUBROUTINE COMHLP(NVAL,NAMES,HELP)
       CHARACTER*(*) NAMES(NVAL), HELP(NVAL)
c___   include 'pep4_com_lib(comode)'
       include 'comode.inc'
C@@@   EXTERNAL HELP_LINE, LIB$GET_INPUT
C
C..... TYPE HELP INFORMATION
C
       IF (HELP(1)(1:1).EQ.'$'.OR.HELP(1)(1:1).EQ.'\') THEN
         CALL CGETHL ( HELP(1)(2:) )
C        PRINT *, '%COM-F-HLPFIL, External Help is not supported yet.'
       ELSE
         DO 10 J = 1,NVAL
           LH = LENRD(HELP(J))
           IF (NAMES(J).NE.' ' .OR. NAMES(J-1).NE.' ' .OR.
     &           HELP(J).NE.' ' .OR. HELP(J-1).NE.' ')
     &         WRITE (LUNCOM,104) NAMES(J),HELP(J)(1:LH)
10       CONTINUE
       END IF
       WRITE (LUNCOM,*)
104    FORMAT (1X,A,2X,A)
       RETURN
       END
C
C----------
C HLPLIB
C----------
C
       SUBROUTINE HLPLIB ( DSN )
C
C ARGUMENTS
C
       CHARACTER * (*)   DSN
C
C COMMON
C
       COMMON / COM$HL / HLPDSN, LHLP
       CHARACTER * 40    HLPDSN
       INTEGER * 4       LHLP
C
C VARIABLES
C
       LOGICAL * 4       STATUS
C
C FUNCTIONS
C
       INTEGER * 4       LKBRD
C
C MAIN
C
C      CALL DSNCHK ( DSN, STATUS )
       INQUIRE ( FILE=DSN, EXIST=STATUS )
       IF ( .NOT. STATUS ) THEN
         PRINT *, '%COM-F-NOHLP, Help Dataset not found'
         PRINT *, 'STATUS = ', STATUS
         PRINT *, 'DSN = ', DSN
       ELSE
         HLPDSN = DSN
         LHLP = LKBRD ( HLPDSN, 0 )
       END IF
C
       RETURN
       END
C
C----------
C CGETHL
C----------
C
       SUBROUTINE CGETHL ( HELP )
C
C ARGUMENTS
C
       CHARACTER * (*)  HELP
C
C COMMON
C
       COMMON / COM$HL / HLPDSN, LHLP
       CHARACTER * 40    HLPDSN
       INTEGER * 4       LHLP
C
C VARIABLES
C
       LOGICAL * 4       STATUS
       CHARACTER * 80    MEMBER
       INTEGER * 4       LMEM
       CHARACTER * 72    LINBUF
C
C FUNCTIONS
C
       INTEGER * 4       LKBRD
C
C MAIN
C
       LMEM = LKBRD ( HELP, 0 )
       MEMBER = HLPDSN(1:LHLP)//'('//HELP(1:LMEM)//')'
C      CALL DSNCHK ( MEMBER, STATUS )
       INQUIRE ( FILE=MEMBER, EXIST=STATUS )
       IF ( .NOT. STATUS ) THEN
         PRINT *, '%COM-F-NOHLP, Help not found for the item'
       ELSE
C         OPEN ( UNIT=91, FILE=MEMBER, STATUS='OLD', FORM='FORMATTED',
C     #          ACTION='READ' )
         OPEN ( UNIT=91, FILE=MEMBER, STATUS='OLD', FORM='FORMATTED' )
10       CONTINUE
           READ ( 91, '(A)', ERR=99, END=99 ) LINBUF
           WRITE ( 6, * ) LINBUF
         GOTO 10
99       CONTINUE
         CLOSE ( UNIT=91 )
       END IF
       RETURN
       END
