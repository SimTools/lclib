C***************************************************************
C*
C* ---------------------------------------------
C*   Integer FUNCTION RINDEX*4( STRING, TARGET )
C* ---------------------------------------------
C*
C*(Function)
C*  Returns the position of the last occurence of the
C*  target characters in the STRING.
C*(Input)
C*  STRING : String where the last occurence of the target
C*           characters are searched for.
C*  TARGET : 
C*(Output)
C*  RINDEX : Location of the target characters in the STRING.
C*          = 0 when not found.
C*(Author)
C*  A. Miyamoto   22-May-1994  Original version.
C*
C****************************************************************
C*
      INTEGER FUNCTION RINDEX*4(STRING, TARGET)
      CHARACTER*(*) STRING, TARGET
C
      LSTR = LEN(STRING)
      LTRG = LEN(TARGET)
      DO 100 I = LSTR-LTAR+1, 1, -1
         IF( STRING(I:I+LTRG-1).EQ.TARGET(1:LTRG)) THEN
            RINDEX = I
            RETURN
         ENDIF
 100  CONTINUE
      RINDEX = 0
      RETURN
      END
