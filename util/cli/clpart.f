C   01/07/88 810151643  MEMBER NAME  CLPART   (FORT)     M  FORTRAN
C
C CLPART: return the 'N'-th word in the string 'S' into 'W'.
C
C Written by A.Shirahashi, Univ. of Tokyo
C
        SUBROUTINE CLPART( S,N,W )
C
C ARG
        CHARACTER * (*)  S
        CHARACTER * (*)  W
        INTEGER * 4  N
C
C CONST
        CHARACTER * 1  D
        PARAMETER( D = ' ' )
C
C BEGIN
        W = ' '
        L = LEN( S )
        I = 0
        N1 = 0
100     CONTINUE
        I = I + 1
        IF( I.GT.L ) RETURN
        IF( INDEX( D,S(I:I) ).NE.0 ) GOTO 100
        J = I
300     CONTINUE
        I = I + 1
        IF( I.GT.L ) GOTO 310
        IF( INDEX( D,S(I:I) ).EQ.0 ) GOTO 300
310     N1 = N1 + 1
        IF( N1.EQ.N ) THEN
          W = S(J:I-1)
          RETURN
        END IF
        GOTO 100
C
        END
