C   03/07/85 705301416  MEMBER NAME  TITRD    (FORT)     M  FORTRAN
C
        SUBROUTINE TITRD(QUEST,ANSWER)
C
        CHARACTER ANSWER*(*),QUEST*(*),K,K2
        CHARACTER*80 CASE,CASEQ
        COMMON /CASE/ LCASE
        LOGICAL LCASE
C
C BEGIN
        IF (QUEST(1:1).EQ.'?') ANSWER = ' '
        LENQ = LENRD(QUEST)
        LENA = LENRD(ANSWER)
C       ... Begin Delete by A.Shirahashi, 10-Dec-1986
C        NCH = LEN(ANSWER)
C3       ICH = 0
C        IF (LCASE) CALL LINRD(1,1)
C       ... End Delete
        CALL ARGRD(QUEST,ANSWER)
C       ... Begin Insert by A.Shirahashi, 10-Dec-1986
        ICH = 0
10      CONTINUE
          CALL CHARD( K,K2 )
          IF( K.EQ.CHAR(13) ) GOTO 20
          ICH = ICH + 1
          ANSWER(ICH:ICH) = K
        GOTO 10
20      CONTINUE
        IF( ICH.GT.0 ) ANSWER(ICH+1:) = ' '
C       ... End Insert
        RETURN
        END
