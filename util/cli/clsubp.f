C   01/07/88 810151647  MEMBER NAME  CLSUBP   (FORT)     M  FORTRAN
C
C CLSUBP: return the offset of the first character of 'N'-th word
C         in the string 'S' into 'I'.
C
C Written by A.Shirahashi, Univ. of Tokyo
C
        SUBROUTINE CLSUBP( S,N,I )
C
C ARG
        CHARACTER * (*)  S
        INTEGER * 4  N, I
C
C CONST
        CHARACTER * 1  D
        PARAMETER( D = ' ' )
C
C BEGIN
        I = 0
        L = LEN( S )
        I1 = 0
        N1 = 0
100     CONTINUE
        I1 = I1 + 1
        IF( I1.GT.L ) RETURN
        IF( INDEX( D,S(I1:I1) ).NE.0 ) GOTO 100
        J = I1
300     CONTINUE
        I1 = I1 + 1
        IF( I1.GT.L ) GOTO 310
        IF( INDEX( D,S(I1:I1) ).EQ.0 ) GOTO 300
310     N1 = N1 + 1
        IF( N1.EQ.N ) THEN
          I = J
          RETURN
        END IF
        GOTO 100
C
        END
