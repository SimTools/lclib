C   03/07/85 705301412  MEMBER NAME  LOGRD    (FORT)     M  FORTRAN
C
       SUBROUTINE LOGRD(QUEST,L)
C
C ARG
       LOGICAL L
       CHARACTER QUEST*(*)
C
C VAR
       CHARACTER M,WORK*80
C
C COMMON
       include '_unitrd.inc'
       INTEGER*4 IRD, IWR
C
C BEGIN
       LQ = LEN(QUEST)
       WORK = QUEST
C
C..... accept logical value
C
CCC    Help_save = Help_set
1      M = 'N'
       IF (L) M = 'Y'
       CALL TXTRD(WORK(1:LQ)//' (Y/N)',M)
CCC    Help_set = Help_save
       CALL STRUPC(1,M)
       IF (M.EQ.'Y' .OR. M.EQ.'T' .OR. M.EQ.'1') GOTO 3
       IF (M.EQ.'N' .OR. M.EQ.'F' .OR. M.EQ.'0') GOTO 2
       CALL LINRD(1,1)
       GOTO 1
2      L = .FALSE.
       RETURN
3      L = .TRUE.
       RETURN
       END
