C* 
C**********************************************************************
C* 
C* ---------------------------=====================
C* Subroutine CTOKEN( STRING, NTOK, TOKEN, LTOKEN )
C* ---------------------------=====================
C* 
C*(Function)
C*   Devide input string to several token.  Separator is given by SEP.
C*   Heading and trailing blanks are not
C* 
C*(Input)
C*     STRING  ... Character*(*) string containing tokend.
C* 
C*(Output)
C*     NTOK    ... Number of token.
C*     TOKEN   ... Character*(*) array containing token.
C*     LTOKEN  ... Integer*4 array of length of each token.
C* 
C*(Author)
C*     A. Miyamoto  26-June-1985
C* 
C***********************************************************************
C* 
      SUBROUTINE CTOKEN( STRING, NTOK, TOKEN, LTOKEN )
C  
      CHARACTER*(*) STRING, TOKEN(*)
      INTEGER*4     LTOKEN(*)
C  
      LSTR = LEN(STRING)
      NTOK = 0
      DO 100 IP = 1, LSTR
         IF(STRING(IP:IP).NE.' '.OR.STRING(IP:IP).EQ.',') GO TO 200
100   CONTINUE
      RETURN
C  
C  
C  
200   CONTINUE
      IT   = IP - 1
      DO 300 WHILE ( IT .LT. LSTR )
         IT = IT + 1
         IF(STRING(IT:IT).EQ.' '.OR.STRING(IT:IT).EQ.',') GO TO 300
            NTOK = NTOK + 1
            L    = 1
            TOKEN(NTOK)(L:L) = STRING(IT:IT)
            IT   = IT + 1
            DO 320 I = IT, LSTR
               IF(STRING(I:I).EQ.' '.OR.STRING(I:I).EQ.',') GO TO 330
                  L = L + 1
                  TOKEN(NTOK)(L:L) = STRING(I:I)
320         CONTINUE
330         CONTINUE
         IT = I
         LTOKEN(NTOK) = L
300   CONTINUE
C  
C  
C  
      RETURN
      END
