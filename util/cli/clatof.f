C   05/07/89 907051451  MEMBER NAME  CLATOF   (FORT)     M  FORTRAN
C
C Name: CLATOF
C Description: Convert string to real value
C Author: A.Shirahashi, KEK
C
      SUBROUTINE CLATOF( STRING,VALUE )
C
C ARG
      CHARACTER * (*)  STRING
      REAL * 4  VALUE
C
C VAR
      REAL * 4  BSIGN, BODY, FPOWER
      CHARACTER * 1  C
C
C BEGIN
      L = LEN( STRING )
      IBSIGN = 1
      BODY = 0.0E0
      IPSIGN = 1
      IPOWER = 0
      K = 0
100   CONTINUE
      K = K + 1
      IF( K.GT.L ) GOTO 9000
      IF( STRING(K:K).EQ.' ' ) GOTO 100
      IF( STRING(K:K).EQ.'+' ) THEN
        IBSIGN = 1
        K = K + 1
      ELSE IF( STRING(K:K).EQ.'-' ) THEN
        IBSIGN = -1
        K = K + 1
      END IF
110   IF( K.GT.L ) GOTO 9000
      IF( STRING(K:K).GE.'0'.AND.STRING(K:K).LE.'9' ) THEN
        BODY = BODY * 10.0E0 + FLOAT( ICHAR(STRING(K:K))-ICHAR('0'))
        K = K + 1
        GOTO 110
      END IF
      IF( STRING(K:K).EQ.' ' ) GOTO 9000
      IF( STRING(K:K).NE.'.' ) GOTO 200
      FPOWER = 1.0E0
120   K = K + 1
      IF( K.GT.L ) GOTO 9000
      IF( STRING(K:K).GE.'0'.AND.STRING(K:K).LE.'9' ) THEN
        FPOWER = FPOWER / 10.0E0
        BODY = BODY + FLOAT( ICHAR(STRING(K:K))-ICHAR('0') ) * FPOWER
        GOTO 120
      END IF
      IF( STRING(K:K).EQ.' ' ) GOTO 9000
200   CONTINUE
      C = STRING(K:K)
      IF( C.NE.'D'.AND.C.NE.'E'.AND.C.NE.'d'.AND.C.NE.'e' ) GOTO 9000
210   K = K + 1
      IF( K.GT.L ) GOTO 9000
      IF( STRING(K:K).EQ.'+' ) THEN
        IPSIGN = 1
        K = K + 1
      ELSE IF( STRING(K:K).EQ.'-' ) THEN
        IPSIGN = -1
        K = K + 1
      END IF
220   IF( K.GT.L ) GOTO 9000
      IF( STRING(K:K).GE.'0'.AND.STRING(K:K).LE.'9' ) THEN
        IPOWER = IPOWER * 10 + ICHAR(STRING(K:K)) - ICHAR('0')
        K = K + 1
        GOTO 220
      END IF
C
9000  VALUE = IBSIGN * BODY * ( 10 ** FLOAT( IPSIGN * IPOWER ) )
C     PRINT *,'IBSIGN,BODY,IPSIGN,IPOWER=',
C    &         IBSIGN,BODY,IPSIGN,IPOWER
      RETURN
      END
