C****************************************************
C*
C*   --------------------------=======
C*   INTEGER FUNCTION*4 LNBLNK(STRING)
C*   --------------------------=======
C*
C*(Function)
C*    Returns position of  last non-blank character.
C*(Input)
C*    STRING : Character String.
C*(Output)
C*    LNBLNK : LPosition of last non-blank characters.
C*(Author)
C*
C***************************************************
C
      INTEGER FUNCTION LNBLNK*4( STRING)
      CHARACTER STRING*(*)
      
      LSTR = LEN(STRING)
      DO 100 L = LSTR, 1, -1
         ICODE = ICHAR(STRING(L:L))
         IF( ICODE.GT.32.AND.ICODE.LT.127 ) THEN
            LNBLNK = L
            RETURN
         ENDIF
 100  CONTINUE
      LNBLNK = 0
      RETURN
      END
