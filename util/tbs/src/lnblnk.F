c+
c File : lnblnk.F
c Description : LNBLNK(Sun) Substitution
c
c Author : Ryosuke Itoh, Physics Division, KEK
c Date : 10 - Feb - 1994
c Note : This code was grabbed from CLI's LENRD
c-
       FUNCTION LNBLNK(CH)
       CHARACTER*(*) CH
C
C..... GET NON-BLANK LENGTH OF CHARACTER
C
       L = LEN(CH)
       M = L
       DO 1 I = 1,L
         IF (CH(M:M).NE.' ') GOTO 2
         M = M-1
 1     CONTINUE
 2     LNBLNK = M
       RETURN
       END
