C   11/11/86 612122030  MEMBER NAME  INQUIRE  (FORT)     M  FORTRAN
C+++++ THIS IS INQUIRE.FOR
C
       SUBROUTINE INQUIR(QUEST,NVAL,NAMES,HELP,NREPLY,LORD)
C
C@@    IMPLICIT NONE
C
c      include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 NVAL,NREPLY,LORD(0:32)
       CHARACTER*(*) NAMES(NVAL),HELP(NVAL),QUEST
       INTEGER*4 IREPLY(32),LABEL,NLEN,INC,LPRMPT,J
       INTEGER*4 IFLQ,IFCR,LENRD
       LOGICAL*4 NANSW
       CHARACTER*50 RESP,BESP
       CHARACTER FOR*12,PROMPT*72
C
C..... ASK THE USER TO SELECT ONE OR MORE OPTIONS
C
       IF (ICOMER.LT.0) GOTO LABEL
       IF (LUNCOM.EQ.0) CALL INICOM ('BUG')
       NLEN = LEN(NAMES(1))
 3     RESP = ' '
       PROMPT = '?'//COMPRM//'> '
       LPRMPT = 6
       LORD(0) = 0
C
C..... TYPE THE OPTIONS
C
 1     CONTINUE
C       IF ( .NOT. BATCH ) CALL CM$CLR
       WRITE (LUNCOM,100) COMPRM, QUEST
 100   FORMAT (' '/' ',A,':  ',A/' ')
       IF (RESP.EQ.'?') THEN
         CALL COMHLP(NVAL,NAMES,HELP)
       END IF
       IF (RESP.NE.'?' .OR. HELP(1)(1:1).EQ.'$') THEN
         INC = MAX(1,MIN(9,72/(NLEN+2)))
         WRITE (LUNCOM,105) NAMES
 105     FORMAT (4(1X,A,1X))
       END IF
C
C..... GET A KEYWORD
C
       NANSW = .FALSE.
       CALL LINRD(1,1)
 2     CALL FLARD(IFLQ,IFCR)
       IF (NREPLY.GT.0) THEN
         IF (IFLQ.NE.0) WRITE (LUNCOM,102) COMPRM,NREPLY
 102     FORMAT (' '/' ',A,': Select',I3,' Option')
 80      ASSIGN 80 TO LABEL
         CALL COMKEY(PROMPT(1:LPRMPT),RESP)
         IF (ICOMER.NE.1) RETURN
       ELSE
         IF (IFLQ.NE.0) WRITE (LUNCOM,103) COMPRM,-NREPLY
 103     FORMAT (' '/' ',A,': Select',I3,' or More, Then OK')
 81      ASSIGN 81 TO LABEL
         CALL COMKEY(PROMPT(1:LPRMPT),RESP)
         IF (ICOMER.GT.1 .AND. LPRMPT.GT.6) GOTO 3
         IF (ICOMER.NE.1) RETURN
         IF ((RESP.EQ.'OK' .OR. RESP.EQ.'ok')
     &       .AND. LORD(0).GE.-NREPLY) THEN
           CALL LINRD(1,1)
           RETURN
         END IF
       END IF
       IF (RESP.EQ.' ' .AND. NANSW) THEN
         NANSW = .FALSE.
         CALL LINRD(1,1)
         GOTO 2
       END IF
       IF (RESP.EQ.' ' .OR. RESP.EQ.'?') GOTO 1
       CALL FLGRD(0,0)
       BESP = RESP
       CALL COMTCH(NAMES,NVAL,RESP(1:NLEN),J)
       IF (J.LE.0) THEN
         CALL LINRD(1,1)
         IF (ICOMER.EQ.3)  RETURN
         GOTO 2
       END IF
       LORD(0) = LORD(0)+1
       LORD(LORD(0)) = J
       IF (LORD(0).EQ.NREPLY) THEN
         CALL LINRD(1,1)
         RETURN
       END IF
C
C..... NEXT KEYWORD
C
       NANSW = .TRUE.
       PROMPT = PROMPT(1:LPRMPT)//BESP
       LPRMPT = LPRMPT+1+LENRD(BESP)
       GOTO 2
       END
C=====
       SUBROUTINE INQUR2(QUEST,NVAL,NAMES,HELP,NREPLY,LANSWR)
C@@    IMPLICIT NONE
c      include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 NVAL,NREPLY,LANSWR
       CHARACTER*(*) QUEST,NAMES(NVAL),HELP(NVAL)
       INTEGER*4 LORD(0:32),J
C
C..... SAME AS INQUIRE, RETURNS A BIT PATTERN
C
       CALL INQUIR(QUEST,NVAL,NAMES,HELP,NREPLY,LORD)
       IF (ICOMER.NE.1) RETURN
       LANSWR = 0
       IF (LORD(0).EQ.0) RETURN
       DO 10 J = 1,LORD(0)
         LANSWR = IOR ( LANSWR, ISHFT(1,LORD(J)-1) )
10     CONTINUE
       END
C=====
       SUBROUTINE CHOSIT(QUEST,NVAL,NAMES,HELP,LANSWR)
C@@    IMPLICIT NONE
c      include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 NVAL,LANSWR
       CHARACTER*(*) QUEST,NAMES(NVAL),HELP(NVAL)
       INTEGER*4 LORD(0:1)
C
C..... SAME AS INQUIRE, FOR ONE VALUE ONLY
C
       CALL INQUIR(QUEST,NVAL,NAMES,HELP,1,LORD)
       IF (ICOMER.NE.1) RETURN
       LANSWR = LORD(1)
       END
