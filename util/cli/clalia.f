C   27/07/88 902162140  MEMBER NAME  CLALIA   (FORT)     M  FORTRAN
C
C CLI Alias Support Routines
C
C     by A.Shirahashi, Univ. of Tokyo
C
C ********
C  CLALIR  Alias Replace
C ********
      SUBROUTINE CLALIR( INPUT,OUTPUT,LENGTH )
C
C ARG
      CHARACTER * (*)  INPUT, OUTPUT
      INTEGER * 4      LENGTH
C
C VAR
      INTEGER * 4  NARGS, MARGS, LARGS(10)
      CHARACTER * 16  CARGS(10)
      CHARACTER * 80  BUFFER
C
C BEGIN
      CALL CLALIL( INPUT,OUTPUT,LENGTH )
      IF( LENGTH.LT.0 ) RETURN
C
      NARGS = 0
      MARGS = 0
      DO 100 I = 1, LENGTH
        IF( OUTPUT(I:I).EQ.'%' .OR. OUTPUT(I:I).EQ.'#' ) THEN
          NARGS = NARGS + 1
          READ( OUTPUT(I+1:I+1),'(I1)' ) IARGS
          MARGS = MAX( MARGS,IARGS )
        END IF
100   CONTINUE
      IF( NARGS.EQ.0 ) RETURN
C
      DO 200 I = 1, MARGS
        CALL TXTRD( '?',CARGS(I) )
        LARGS(I) = LENRD( CARGS(I) )
200   CONTINUE
C
      I = 1
      J = 1
300   CONTINUE
        IF( OUTPUT(I:I).EQ.'%' .OR. OUTPUT(I:I).EQ.'#' ) THEN
          READ( OUTPUT(I+1:I+1),'(I1)' ) IARGS
          BUFFER(J:J+LARGS(IARGS)-1) = CARGS(IARGS)(:LARGS(IARGS))
          I = I + 2
          J = J + LARGS(IARGS)
        ELSE
          BUFFER(J:J) = OUTPUT(I:I)
          I = I + 1
          J = J + 1
        END IF
      IF( I.LE.LENGTH ) GOTO 300
C
      LENGTH = J - 1
      OUTPUT = ' '
      OUTPUT(:LENGTH) = BUFFER(:LENGTH)
C     PRINT *, 'LENGTH=',LENGTH
C     PRINT *, 'BUFFER=',BUFFER(:LENGTH)
C
      RETURN
      END
C
C ********
C  CLALIL  Alias Lookup
C ********
      SUBROUTINE CLALIL( INPUT,OUTPUT,LENGTH )
C
C ARG
      CHARACTER * (*)  INPUT, OUTPUT
      INTEGER * 4      LENGTH
C
C COMMON
      COMMON / CLALI1 / NMAC,LKMAC(100),LMMAC(100)
      COMMON / CLALI2 / MACKEY(100),MACBUF(100)
      INTEGER   *  4  NMAC, LKMAC, LMMAC
      CHARACTER * 16  MACKEY
      CHARACTER * 80  MACBUF
C
C
C BEGIN
      DO 100 I = NMAC, 1, -1
C       PRINT *, LENGTH,LKMAC(I)
C       PRINT *, '/',INPUT(:LENGTH),'/',MACKEY(I)(:LENGTH),'/'
        IF( LENGTH.NE.LKMAC(I) ) GOTO 100
        CALL STRUPC( LENGTH,INPUT )
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
C  CLALII  Alias Install
C ********
      SUBROUTINE CLALII( STRING )
C
C ARG
      CHARACTER * (*) STRING
C
C COMMON
      COMMON / CLALI1 / NMAC,LKMAC(100),LMMAC(100)
      COMMON / CLALI2 / MACKEY(100),MACBUF(100)
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
      CALL STRUPC( LKMAC(NMAC),MACKEY(NMAC) )
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
