C   05/07/85 902141349  MEMBER NAME  TXTRD    (FORT)     M  FORTRAN
C
      SUBROUTINE TXTRD( QUEST,ANSWER )
C
C ARG
      CHARACTER * (*)  QUEST, ANSWER
C
C CONST
      CHARACTER * 1  QUOTE
      PARAMETER( QUOTE = '''' )
C
C COMMON
       include '_flagrd.inc'
C
C VAR
      CHARACTER K, K2
      LOGICAL GROUP
C
C  Change text (terminator = ',=<sp><cr>')
C
      IF( QUEST(1:1) .EQ. '?' ) ANSWER = ' '
      NCH = LEN( ANSWER )
3     CONTINUE
      ICH = 0
      CALL ARGRD( QUEST,ANSWER )
      GROUP = .FALSE.
1     CONTINUE
      CALL CHARD( K,K2 )
      IF( K.EQ.'\\' ) GOTO 3
      IF( K.EQ.CHAR(13) ) GOTO 2
      IF( K.EQ.QUOTE ) THEN
        IF( ICH.EQ.0 ) THEN
          GROUP = .TRUE.
          GOTO 1
        ELSE IF( GROUP ) THEN
          GROUP = .FALSE.
          GOTO 1
        ENDIF
      ELSE IF( .NOT.GROUP ) THEN
        IF( K.EQ.' ' ) THEN
          IF( ICH.EQ.0 ) THEN
            GOTO 1
          ELSE
            GOTO 4
          ENDIF
        ELSE
          IF( K.EQ.',' .OR. K.EQ.'=' ) GOTO 2
        ENDIF
      ENDIF
      ICH = ICH + 1
      IF( ICH.LE.NCH ) ANSWER(ICH:ICH) = K
      GOTO 1
4     IF( K2.NE.' ' ) GOTO 2
      CALL CHARD( K,K2 )
      GOTO 4
2     IF( ICH.GT.0 .AND. ICH.LT.NCH ) THEN
        ICH = ICH + 1
        ANSWER(ICH:NCH) = ' '
      END IF
      RETURN
      END
