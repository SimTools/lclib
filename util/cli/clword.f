C   01/07/88 810151649  MEMBER NAME  CLWORD   (FORT)     M  FORTRAN
C
C CLWORD: divede the string 'S' into the words delimiter by any one
C         character in the string 'D' into the array of strings 'W'.
C
C Written by A.Shirahashi, Univ. of Tokyo
C
        SUBROUTINE CLWORD( S,D,N,W )
C
C ARG
        CHARACTER * (*)  S, D
        CHARACTER * (*)  W(*)
        INTEGER * 4  N
C
C BEGIN
        L = LEN( S )
        I = 0
        N = 0
100     CONTINUE
        I = I + 1
        IF( I.GT.L ) RETURN
        IF( INDEX( D,S(I:I) ).NE.0 ) GOTO 100
        J = I
300     CONTINUE
        I = I + 1
        IF( I.GT.L ) GOTO 310
        IF( INDEX( D,S(I:I) ).EQ.0 ) GOTO 300
310     N = N + 1
        W(N) = S(J:I-1)
        GOTO 100
C
        END
