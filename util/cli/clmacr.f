C   27/07/88 902141357  MEMBER NAME  CLMACR   (FORT)     M  FORTRAN
C
C CLI Macro Support Routines
C
C     by A.Shirahashi, Univ. of Tokyo
C
C ********
C  CLMACR   Macro Replace
C ********
      SUBROUTINE CLMACR( STRING,LENGTH )
C
C ARG
      CHARACTER * (*)  STRING
      INTEGER * 4  LENGTH
C
C COMMON
      COMMON / CLMAC1 / NMAC
      INTEGER * 4  NMAC
C
C ARGS
      CHARACTER * 80  SUBST, OUTPUT
C
C BEGIN
      IF( NMAC.LE.0 ) RETURN
      N = 0
      J = 0
100   CONTINUE
      N = N + 1
      IF( N.GT.LENGTH ) GOTO 200
      IF( STRING(N:N).EQ.' ' ) THEN
        J = J + 1
        OUTPUT(J:J) = ' '
        GOTO 100
      END IF
      M = MIN( INDEX( STRING(N:),' ' )+N-2,LENGTH )
      L = M - N + 1
      L1 = L
      CALL CLMACL( STRING(N:M),SUBST,L1 )
      IF( L1.LT.0 ) THEN
        OUTPUT(J+1:J+L) = STRING(N:M)
        J = J+L
      ELSE IF( L1.GT.0 ) THEN
        OUTPUT(J+1:J+L1) = SUBST(:L1)
        J = J+L1
      END IF
      N = M
      GOTO 100
200   CONTINUE
      LENGTH = J
      STRING = OUTPUT(:LENGTH)
C     PRINT *, '/',OUTPUT(:LENGTH),'/'
      RETURN
      END
C
C ********
C  CLMACL  Macro Lookup
C ********
      SUBROUTINE CLMACL( INPUT,OUTPUT,LENGTH )
C
C ARG
      CHARACTER * (*)  INPUT, OUTPUT
      INTEGER * 4      LENGTH
C
C COMMON
      COMMON / CLMAC1 / NMAC,LKMAC(100),LMMAC(100)
      COMMON / CLMAC2 / MACKEY(100),MACBUF(100)
      INTEGER   *  4  NMAC, LKMAC, LMMAC
      CHARACTER * 16  MACKEY
      CHARACTER * 80  MACBUF
C
C
C BEGIN
      DO 100 I = NMAC, 1, -1
        IF( LENGTH.NE.LKMAC(I) ) GOTO 100
        IF( INPUT(:LENGTH).EQ.MACKEY(I)(:LENGTH) ) THEN
          OUTPUT = MACBUF(I)(:LMMAC(I))
          LENGTH = LMMAC(I)
          RETURN
        END IF
100   CONTINUE
C
      LENGTH = -1
      RETURN
      END
C
C ********
C  CLMACI  Macro Install
C ********
      SUBROUTINE CLMACI( STRING )
C
C ARG
      CHARACTER * (*) STRING
C
C COMMON
      COMMON / CLMAC1 / NMAC,LKMAC(100),LMMAC(100)
      COMMON / CLMAC2 / MACKEY(100),MACBUF(100)
      INTEGER   *  4  NMAC, LKMAC, LMMAC
      CHARACTER * 16  MACKEY
      CHARACTER * 80  MACBUF
C
C BEGIN
      I = 0
100   CONTINUE
      I = I + 1
      IF( STRING(I:I).EQ.' ' ) GOTO 100
200   J = MIN( INDEX( STRING(I:),' ' )+I-2,LEN(STRING) )
      NMAC = NMAC + 1
      MACKEY(NMAC) = STRING(I:J)
      LKMAC(NMAC) = J-I+1
      L = LEN(STRING)-J-1
      MACBUF(NMAC) = STRING(J+2:)
      LMMAC(NMAC) = L
C
C     PRINT *, STRING
C     PRINT *, NMAC
C     PRINT *, LKMAC(NMAC),':',MACKEY(NMAC)(:LKMAC(NMAC))
C     PRINT *, LMMAC(NMAC),':',MACBUF(NMAC)(:LMMAC(NMAC))
      RETURN
      END
