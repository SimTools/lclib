C*--------------------------------------------------------------------
C* 
C* --------------------------
C*    Function NUCRMV(STRING)
C* --------------------------
C* 
C*(Function)
C*   Remove heading and trailing blanks in the input STRING.
C*  For example,
C*     STRING = '    This is a pen.    '
C*   then output is,
C*     STRING = 'This is a pen.'
C*     NUCRMV = 14
C* 
C*(Input)
C*   STRING : Character string.
C* 
C*(Output)
C*   STRING : String after removed specified character.
C*   NUCRMV : Effective character length.
C* 
C*(Author)
C*   A. Miyamoto  25-Dec-1985.   Original version.
C* 
C*--------------------------------------------------------------------
C* 
      FUNCTION NUCRMV(STRING)
C  
      CHARACTER*(*) STRING
C  
      LSTR   =  LEN(STRING)
C  
C    Remove heading and trailling blanks.
C  
      DO 100 IS = 1, LSTR
        IF(STRING(IS:IS).NE.' ') GO TO 110
100   CONTINUE
      NUCRMV = 0
      RETURN
C  
110   CONTINUE
      DO 120 IE = LSTR, IS+1, -1
        IF(STRING(IE:IE).NE.' ') GO TO 130
120   CONTINUE
      IE = LSTR
C  
130   CONTINUE
      DO 140 I = IS, IE
        IP = I - IS + 1
        STRING(IP:IP) = STRING(I:I)
140   CONTINUE
      NUCRMV = IP
      IF(IP.LT.LSTR) STRING(IP+1:) = ' '
C  
      RETURN
      END
