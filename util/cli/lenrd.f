C   03/07/85 507032140 MEMBER NAME  LENRD    (DISPLAY)     FORTRAN
       FUNCTION LENRD(CH)
       CHARACTER*(*) CH
C
C..... GET NON-BLANK LENGTH OF CHARACTER
C
       L = LEN(CH)
       M = L
       DO 1 I = 1,L
         IF (CH(M:M).NE.' ') GOTO 2
         M = M-1
1      CONTINUE
2      LENRD = M
       RETURN
       END
