C
C ----------
C   CLIERR   ... print error message
C ----------
       SUBROUTINE CLIERR( LEVEL,MESAGE )
C
C ARG
C      (Input)
       INTEGER * 4  LEVEL
       CHARACTER * ( * )  MESAGE
C
C COMMON
       include '_unitrd.inc'
C
C VAR
       CHARACTER * 8  PREFIX
       LOGICAL   * 4  YES
C
C FUNC
       LOGICAL * 4  IFTSS
C
C BEGIN
       IF( LEVEL .EQ. 0 ) THEN
         PREFIX = '%CLI-I, '
       ELSE IF( LEVEL .EQ. 1 ) THEN
         PREFIX = '%CLI-W, '
       ELSE IF( LEVEL .EQ. 2 ) THEN
         PREFIX = '%CLI-E, '
       END IF
C
       LMSG = LENRD( MESAGE )
CMSP       OPEN( UNIT=TLUN,FILE='*' )
CVMS
       OPEN( UNIT=TLUN,FILE='SYS$OUTPUT',STATUS='UNKNOWN' )
CVMS
CMSP       WRITE( TLUN,'(A,A)' ) PREFIX,MESAGE(1:LMSG)
CVMS
       WRITE( TLUN,'(1H ,A,A)' ) PREFIX,MESAGE(:LMSG)
CVMS
       CLOSE( UNIT=TLUN )
C
       IF( LEVEL.LE.0 ) RETURN
       IF( .NOT.IFTSS() .OR. LUNIN.EQ.5 .OR. LUNPTR.EQ.0 ) RETURN
C
       YES = .TRUE.
       CALL AFFIRM( 'Continue to execute command procedure ?', YES )
       IF( .NOT.YES ) CALL CLSTOP()
C
       RETURN
       END
