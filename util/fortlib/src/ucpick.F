C*--------------------------------------------------------------------
C* 
C* -----------------------------------------==========
C*    Subroutine UCPICK(STRING, SEP1, SEP2, ARG, LARG)
C* -----------------------------------------==========
C* 
C*(Function)
C*   Pick up the characters between SEP1 and SEP2 in the input STRING.
C*  For example,
C*     STRING = This is very usefull and beautifull program.
C*     SEP1   = is
C*     SEP2   = and
C*   then
C*     ARG    = very usefull.
C*  or
C*     STRING = TPC LEVEL(2) DEBUG(3)
C*     SEP1   = LEVEL(
C*     SEP2   = )
C*   then
C*     ARG    = 2
C* 
C*(Input)
C*   STRING : Input character string from where ARG is searched.
C*   SEP1   : First separator.
C*   SEP2   : Second separator.
C* 
C*(Output)
C*   ARG    : Character string between SEP1 and SEP2, blanks are removed
C*   LARG   : Effective length of ARG, with heading and trailing blanks
C*           removed.
C*         = -1, when separators are not found.
C* 
C*(Author)
C*   A. Miyamoto  25-Dec-1985.   Original version.
C* 
C*--------------------------------------------------------------------
C* 
      SUBROUTINE UCPICK(STRING, SEP1, SEP2, ARG, LARG)
C* 
      CHARACTER*(*) STRING, SEP1, SEP2, ARG
C  
      LSTR   =  LEN(STRING)
      LSEP1  =  LEN(SEP1)
      LSEP2  =  LEN(SEP2)
C  
      ISEP1  = INDEX(STRING,SEP1)
      IF(ISEP1.EQ.0.OR.ISEP1+LSEP1.GT.LSTR) GO TO 900
      ISEP2  = INDEX(STRING(ISEP1+LSEP1:),SEP2)
      IF(ISEP2.EQ.0)                        GO TO 900
C  
      LARG = ISEP2 - 1
      IF(LARG.LE.0) GO TO 9999
      ARG    = STRING(ISEP1+LSEP1:ISEP1+LSEP1+ISEP2-2)
C  
C    Remove heading and trailling blanks.
C  
      DO 100 IS = 1, LARG
        IF(ARG(IS:IS).NE.' ') GO TO 110
100   CONTINUE
      LARG = 0
      GO TO 9999
C  
110   CONTINUE
      DO 120 IE = LARG, IS+1, -1
        IF(ARG(IE:IE).NE.' ') GO TO 130
120   CONTINUE
C  
130   CONTINUE
      DO 140 I = IS, IE
        IP = I - IS + 1
        ARG(IP:IP) = ARG(I:I)
140   CONTINUE
      LARG = IP
C  
9999  CONTINUE
      RETURN
C  
900   LARG = -1
      RETURN
      END
