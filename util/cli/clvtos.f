C   19/06/88 806230010  MEMBER NAME  CLVTOS   (FORT)     M  FORTRAN
C
C CLVTOS : Convert value to string
C
C          by A.Shirahashi, Univ. of Tokyo
C
       SUBROUTINE CLVTOS( VAL,STR,NW,ND )
C
C ARG
       REAL      *  4   VAL
       CHARACTER * (*)  STR
       INTEGER   *  4   NW, ND
C
C VAR
       CHARACTER * 16  FORM
C
C FUNC
       INTEGER * 4  CLVFMT
C
C BEGIN
       STR = ' '
       LS = CLVFMT( 'G',LEN(STR),ND,VAL,STR )
       IE = INDEX( STR,'E' )
       IF( IE .EQ. 0 ) IE = LS + 1
       I = IE - 1
       NI = I
100    CONTINUE
         IF( STR(I:I) .NE. '0' ) GOTO 110
         I = I - 1
       GOTO 100
110    CONTINUE
       IF( STR(I:I+1) .EQ. '.0' ) I = I + 1
       NZ = NI - I
       IF( IE .GT. LS ) THEN
         STR = STR(1:I)
         IF( STR(I:I) .EQ. '.' ) THEN
           LS = CLVFMT( 'F',LEN(STR),1,VAL,STR )
         END IF
         RETURN
       END IF
       READ( STR(IE+1:LS),* ) NP
       NE = I - INDEX( STR,'.' )
       IF( NP.GT.0 .AND. NP.LE.+4 ) THEN
         LS = CLVFMT( 'F',LEN(STR),MAX(1,ND-NP-NZ),VAL,STR )
       ELSE IF( NP.EQ.0 ) THEN
         STR = STR(1:I)
       ELSE IF( NP.LT.0 .AND. NP.GE.-4 ) THEN
         LS = CLVFMT( 'F',LEN(STR),MAX(1,ND-NP-NZ),VAL,STR )
       ELSE
         STR = STR(1:I)//STR(IE:)
       END IF
       RETURN
       END
C
C
       INTEGER FUNCTION CLVFMT( EDIT,NW,ND,VAL,STR )
C
C ARG
       CHARACTER * (*)  EDIT
       INTEGER   *  4   NW,ND
       REAL      *  4   VAL
       CHARACTER * (*)  STR
C
C VAR
       CHARACTER * 16  FORM
       CHARACTER * 16  STRING
C
C FUNC
       INTEGER * 4  LKBRD
C
C BEGIN
       STR = ' '
       IF( EDIT.EQ.'G' ) THEN
         IF( VAL.GE.0.1 .AND. VAL.LT.10000.0 ) THEN
           WRITE( FORM,100 ) ND
100        FORMAT( '(F16.',I1,')' )
           WRITE( STRING,FORM ) VAL
         ELSE
           WRITE( FORM,110 ) ND
110        FORMAT( '(E16.',I1,')' )
           WRITE( STRING,FORM ) VAL
         END IF
       ELSE IF( EDIT.EQ.'F' ) THEN
           WRITE( FORM,120 ) ND
120        FORMAT( '(F16.',I1,')' )
         WRITE( STRING,FORM ) VAL
       END IF
       L = LKBRD( STRING,0 )
       STR(:L) = STRING(:L)
       CLVFMT = L
       RETURN
       END
