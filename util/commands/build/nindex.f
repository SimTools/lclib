C* 
C**********************************************************************
C* 
C* ----------------------------------
C* Function NINDEX( STRING, TARGET )
C* ----------------------------------
C* 
C*(Function)
C*   Returns the position of a character not same as a TARGET string.
C*   If all characters are same as target string, NINDEX=0 is returned.
C* 
C*(Input)
C*     STRING  ... Character*(*) string to be tested.
C*     TARGET  ... The position of a character which is different with
C*               target string is returned.
C* 
C*(Author)
C*     A. Miyamoto  26-June-1985
C* 
C***********************************************************************
C* 
      FUNCTION NINDEX( STRING, TARGET )
C  
      CHARACTER*(*) STRING, TARGET
C  
      LSTR = LEN(STRING)
      LTARG = LEN(TARGET)
C  
      DO 100 I = 1, LSTR
        IF(STRING(I:I+LTARG-1).NE.TARGET(1:LTARG)) GO TO 200
100   CONTINUE
      NINDEX = 0
      RETURN
200   CONTINUE
      NINDEX = I
      RETURN
      END
