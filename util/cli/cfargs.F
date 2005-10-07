C   31/03/87 806250022  MEMBER NAME  CFARGS   (FORT)     M  FORTRAN
C+
C (File       ) CFARGS
C (Description) Routines to handle command procedure arguments
C (Author     ) A.Shirahashi, Univ. of Tokyo
C (Date       ) 31-Mar-87
C-
      SUBROUTINE CFARGS( LINE,DSNAME,LDSN )
C
C ARG
C     (Input)
      CHARACTER * (*)  LINE
C     (Output)
      CHARACTER * (*)  DSNAME
      INTEGER   *  4   LDSN
C
C COMMON
       include '_cliarg.inc'
      EXTERNAL CLARGB
C
C VAR
      LOGICAL * 4  INPAR
C
C BEGIN
#if defined(HIUXF77) || defined(__GFORTRAN__)
      CALL CLARGB
#endif
      LENGTH = LEN( LINE )
C     ... skip blanks
      DO 100 I = 1, LENGTH
        IF( LINE(I:I) .NE. ' ' ) GOTO 110
100   CONTINUE
      LDSN = 0
      GOTO 220
110   CONTINUE
C
C     ... get dataset name
      DO 200 J = I, LENGTH
        IF( LINE(J:J) .EQ. ' ' ) GOTO 210
200   CONTINUE
      J = LENGTH + 1
210   CONTINUE
      LDSN = J - I
      DSNAME = LINE(I:J-1)
220   CONTINUE
C
C     ... save current context on stack
      DO 230 L = 1, NARGS
        CALL CLPUSH( IARGS( L ) )
        CALL CLPUSH( LARGS( L ) )
230   CONTINUE
      CALL CLPUSH( NARGS )
      CALL CLPUSH( IARGP )
C
C     ... get argument(s)
      NARGS = 0
300   CONTINUE
        IF( J .GT. LENGTH ) GOTO 800
        IF( LINE(J:J) .NE. ' ' ) GOTO 310
        J = J + 1
      GOTO 300
310   CONTINUE
C
      K = J
      INPAR = .FALSE.
400   CONTINUE
        IF( K .GT. LENGTH ) GOTO 410
        IF( LINE(K:K).EQ.' ' .AND. .NOT.INPAR ) GOTO 410
        IF( LINE(K:K).EQ.'''' ) INPAR = .NOT.INPAR
        K = K + 1
      GOTO 400
410   CONTINUE
C
      J1 = J
      K1 = K
      IF( LINE(J1:J1).EQ.'''' .AND. LINE(K1-1:K1-1).EQ.'''' ) THEN
        J1 = J1 + 1
        K1 = K1 - 1
      END IF
      NARGS = NARGS + 1
      LARGS( NARGS ) = K1 - J1
      IARGS( NARGS ) = IARGP + 1
      ARGBUF( IARGP+1 : IARGP+LARGS( NARGS ) ) = LINE(J1:K1-1)
      IARGP = IARGP + LARGS( NARGS )
      J = K
C%%   PRINT *, 'ARG(',NARGS,')= ',ARGBUF(IARGS(NARGS):IARGS(NARGS)
C%%  &                              +LARGS(NARGS)-1),'!'
      GOTO 300
C
800   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CFARGP
C
C COMMON
       include '_cliarg.inc'
C
C BEGIN
C     ... restore previous context from stack
      CALL CLPOP( IARGP )
      CALL CLPOP( NARGS )
      DO 100 I = NARGS, 1, -1
        CALL CLPOP( LARGS( I ) )
        CALL CLPOP( IARGS( I ) )
100   CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE CFARGI( I,STRING,LENGTH )
C
C ARG
C     (Input)
      INTEGER * 4  I
C     (Output)
      CHARACTER * (*)  STRING
      INTEGER   *  4   LENGTH
C
C COMMON
       include '_cliarg.inc'
C
C BEGIN
      IF( I .GT. NARGS ) THEN
        LENGTH = 0
        RETURN
      END IF
C
      LENGTH = LARGS( I )
      STRING = ARGBUF( IARGS(I) : IARGS(I)+LENGTH-1 )
      RETURN
      END
C
C
      SUBROUTINE CLPUSH( IVAR )
C
C ARG
C     (Input)
      INTEGER * 4  IVAR
C
C COMMON
       include '_cstack.inc'
      EXTERNAL STACKB
C
C BEGIN
#if defined(HIUXF77) || defined(__GFORTRAN__)
      CALL STACKB
#endif
      IF( STACKP .GE. MAXSTK ) THEN
        PRINT *, '%CLI-E, Stack Overflow'
        RETURN
      END IF
C
      STACKP = STACKP + 1
      ISTACK( STACKP ) = IVAR
      RETURN
      END
C
C
      SUBROUTINE CLPOP( IVAR )
C
C ARG
C     (Output)
      INTEGER * 4  IVAR
C
C COMMON
       include '_cstack.inc'
C
C BEGIN
      IF( STACKP .LE. 0 ) THEN
        PRINT *, '%CLI-E, Stack Underflow'
        RETURN
      END IF
C
      IVAR = ISTACK( STACKP )
      STACKP = STACKP - 1
      RETURN
      END
C
C
#if defined(HIUXF77) || defined(__GFORTRAN__)
      SUBROUTINE CLARGB
#else
      BLOCK DATA CLARGB
#endif
C
       include '_cliarg.inc'
C
      DATA NARGS / 0 /
      DATA IARGP / 0 /
C
      END
C
C
#if defined(HIUXF77) || defined(__GFORTRAN__)
      SUBROUTINE STACKB
#else
      BLOCK DATA STACKB
#endif
C
       include '_cstack.inc'
C
      DATA STACKP / 0 /
C
      END
