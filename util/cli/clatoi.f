C   05/07/89            MEMBER NAME  CLATOI   (FORT)     M  FORTRAN
C
C Name: CLATOI
C Description: Convert string to integer value
C Author: A.Shirahashi, KEK
C
      SUBROUTINE CLATOI( STRING,VALUE )
C
C ARG
      CHARACTER * (*)  STRING
      INTEGER  VALUE
C
C VAR
      INTEGER  SIGN
      CHARACTER * 1  C
C
C BEGIN
      L = LEN( STRING )
      SIGN = 1
      VALUE = 0
      K = 0
100   CONTINUE
      K = K + 1
      IF( K.GT.L ) GOTO 9000
      IF( STRING(K:K).EQ.' ' ) GOTO 100
      IF( STRING(K:K).EQ.'+' ) THEN
        SIGN = 1
        K = K + 1
      ELSE IF( STRING(K:K).EQ.'-' ) THEN
        SIGN = -1
        K = K + 1
      END IF
110   IF( K.GT.L ) GOTO 9000
      IF( STRING(K:K).GE.'0'.AND.STRING(K:K).LE.'9' ) THEN
        VALUE = VALUE * 10 + ICHAR(STRING(K:K)) - ICHAR('0')
        K = K + 1
        GOTO 110
      END IF
C
9000  VALUE = SIGN * VALUE
      RETURN
      END
