C   02/07/85 507032140 MEMBER NAME  LKBRD    (DISPLAY)     FORTRAN
       FUNCTION LKBRD(STR,N)
       CHARACTER*(*) STR
C
C..... SUPPRESS UNUSEFULL BLANKS
C
       L = LEN(STR)
       M = 0
       K = N
       DO 10 I = 1,L
         IF (STR(I:I).EQ.' ') THEN
           IF (K.LT.N) THEN
             K = K+1
             M = M+1
             STR(M:M) = STR(I:I)
           END IF
         ELSE
           K = 0
           M = M+1
           STR(M:M) = STR(I:I)
         END IF
10     CONTINUE
       IF ( M+1 .LE. L ) STR(M+1:L) = ' '
       LKBRD = MAX(0,M-K)
       RETURN
       END
