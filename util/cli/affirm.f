C   03/07/85 810250153  MEMBER NAME  AFFIRM   (FORT)     M  FORTRAN
C
       SUBROUTINE AFFIRM( QUEST,L )
C
C .... Show prompt on console and inquire yes/no
C      3-DEC-1986, A.Shirahashi
C
C ARG
       LOGICAL   *  4   L
       CHARACTER * (*)  QUEST
C
C COMMON
       include '_tvdisp.inc'
C
C VAR
       CHARACTER *  1  M
       CHARACTER * 80  WORKS
       INTEGER   *  4  LQ, IRET
C
C BEGIN
       LQ = LENRD( QUEST )
       WORKS = QUEST
10     CONTINUE
       IF( L ) THEN
         M = 'Y'
       ELSE
         M = 'N'
       END IF
       IF( LTVLOC ) THEN
         CALL TVPROM( WORKS(1:LQ)//' (Y/N): '//M//' ? ' )
         CALL TGET( M,1,'E,W',IRET,LENGTH )
       ELSE
         CALL PROMPT( 2,WORKS(1:LQ)//' (Y/N): '//M//' ? ',IRET )
         READ( 5,'(A1)' ) M
       END IF
       IF( M.EQ.' ' ) THEN
C        ... return default
       ELSE
     & IF( M.EQ.'Y'.OR.M.EQ.'y'.OR.M.EQ.'T'.OR.M.EQ.'t'.OR.M.EQ.'1')
     & THEN
         L = .TRUE.
       ELSE
     & IF( M.EQ.'N'.OR.M.EQ.'n'.OR.M.EQ.'F'.OR.M.EQ.'f'.OR.M.EQ.'0')
     & THEN
         L = .FALSE.
       ELSE
         GOTO 10
       END IF
       RETURN
       END
