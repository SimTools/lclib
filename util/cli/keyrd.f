C   03/07/85 706172220  MEMBER NAME  KEYRD    (FORT)     M  FORTRAN
C
       SUBROUTINE KEYRD(MODE,PROMPT,CWORD,CNAME,NNAME,IT)
C
       include '_unitrd.inc'
       CHARACTER*(*) CWORD, CNAME(NNAME), PROMPT
       CHARACTER FOR*8,LINE*72,WORK*80
       LOGICAL ICOM
C
C..... Get a keyword with a list of possible answers
C
       LP = LEN(PROMPT)
       WORK = PROMPT
       IL = LEN(CWORD)
       ICOM = MODE.GE.0
       IF (ICOM) CALL LINRD(1,ABS(MODE))
10     IF (ICOM) CWORD = ' '
       IF (ICOM .AND. PROMPT(1:1).NE.'?') THEN
         CALL TXTRD('?'//WORK(1:LP),CWORD)
       ELSE
         CALL TXTRD(PROMPT,CWORD)
       END IF
       IF (CWORD(1:1).EQ.' ') THEN
         CALL LINRD(1,ABS(MODE))
         GOTO 10
       END IF
C
C..... Check help, validity
C
       IF (CWORD(1:1).EQ.'?') GOTO 30
       CALL AMBRD(CNAME,NNAME,CWORD,IT)
       IF (IT.GT.0) THEN
         RETURN
       END IF
       WORK = CWORD
       IF( IT.EQ.0 ) CALL CLIERR( 2,'No Such Keyword: '//WORK )
       IF( IT.LT.0 ) CALL CLIERR( 1,'Ambiguous Keyword: '//WORK )
       CALL LINRD(1,ABS(MODE))
       GOTO 10
C
30     CONTINUE
       WRITE (IWR,107)
107    FORMAT (' CLI: Available Keywords:'/)
       INC = MIN(9,72/(IL+4))
       WRITE (FOR,104) INC
104    FORMAT ('(',I1,'(4X,A))')
       WRITE (IWR,FOR) CNAME
       WRITE (IWR,*)
       CALL LINRD(1,ABS(MODE))
       GOTO 10
C
       END
