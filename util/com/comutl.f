C   11/11/86 708161309  MEMBER NAME  COMUTL   (FORT)     M  FORTRAN
C+++++ THIS IS COMUTL.FOR
C
       SUBROUTINE COMKEY(QUEST,ANSWER)
C@@    IMPLICIT NONE
       CHARACTER*(*) QUEST,ANSWER
c      include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 LABEL
C
C..... GET A KEYWORD
C
       IF (ICOMER.LT.0) GOTO LABEL
80     ASSIGN 80 TO LABEL
       CALL GETTXT(QUEST,ANSWER)
       IF (ICOMER.NE.1) RETURN
C@@@   CALL STR$UPCASE(ANSWER,ANSWER)
       CALL STRUPC( LEN(ANSWER),ANSWER )
       ICOMER = 1
       IF (ANSWER.EQ.'HELP') ANSWER = '?'
       END
C=====
       SUBROUTINE GETTXT(QUEST,ANSWER)
C@@    IMPLICIT NONE
       CHARACTER*(*) QUEST, ANSWER
c      include 'comode.inc'
       include 'comode.inc'
C
C..... ASK A QUESTION
C
       IF (.NOT.RLTIME) ICOMER = 1
C@@@   CALL DISABLE_HELP
       CALL TXTRD(QUEST,ANSWER)
C@@@   CALL ENABLE_HELP
       END
C=====
       SUBROUTINE COMTCH(NAMES,NVAL,RESP,ITEM)
C@@    IMPLICIT NONE
       INTEGER*4 NVAL,ITEM
       CHARACTER*(*) NAMES(NVAL),RESP
c      include 'comode.inc'
       include 'comode.inc'
C
C..... LOOK IF KEYWORD IS IN THE LIST
C
       CALL AMBRD(NAMES,NVAL,RESP,ITEM)
C@@@   IF (ITEM.EQ.0) WRITE (LUNCOM,*) 'COM: NO SUCH ANSWER: '//RESP
       IF (ITEM.EQ.0) WRITE (LUNCOM,*) 'COM: No Such Answer: ', RESP
C@@@   IF (ITEM.LT.0) WRITE (LUNCOM,*) 'COM: AMBIGUOUS ANSWER: '//RESP
       IF (ITEM.LT.0) WRITE (LUNCOM,*) 'COM: Ambiguous Answer: ', RESP
C@@@   IF (ITEM.LE.0 .AND. BATCH)  ICOMER = 3
       IF (ITEM.LE.0 .AND. BATCH)  THEN
         PRINT *, '%COM-F, COM fatal error. Execution terminated.'
CMSP          CALL EXIT
         STOP
       END IF
       END
C=====
       SUBROUTINE INICOM (PROMPT)
C@@    IMPLICIT NONE
c      include 'comode.inc'
       include 'comode.inc'
       CHARACTER*(*) PROMPT
C
C..... DEFINE PROMPT AND LOGICAL UNIT
C
       COMPRM = PROMPT
       LUNCOM = 6
C@@@   CALL PROCESS_MODE(BATCH)
       CALL PRCMOD (BATCH)
       CALL LINRD(1,1)
C@@@   CALL SET_CTRLC(0)
       CALL SETATN
       END
