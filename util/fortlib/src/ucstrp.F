C*--------------------------------------------------------------------
C* 
C* -----------------------------------=================
C*    Subroutine UCSTRP(Input, Option, Output, Length )
C* -----------------------------------=================
C* 
C*(Function)
C*   Remove the Heading, in between blanks from
C*  the Input string and output together with the valid character
C*  Length.
C* 
C*(Input)
C* INPUT    : Any length character string.
C* OPTION   : Specify the mode of removing. (Character*1)
C*        = 'H'          Remove Heading Blanks.
C*        = ' ' or 'B'   Remove both heading and inbetween blanks.
C* 
C*(Output)
C* Output  : Input string after removing the heading or inbetwen
C*            blanks.
C* LENGTH  : The valid character length after removing the blanks.
C* 
C*(Author)
C* A. Miyamoto   8-Aug-1984.
C* 
C*--------------------------------------------------------------------
C* 
      SUBROUTINE UCSTRP(INPUT, OPTION, OUTPUT, LENGTH)
C* 
      CHARACTER*(*) INPUT, OUTPUT
      CHARACTER*1   OPTION
C  
      LIN = LEN(INPUT)
      OUTPUT = ' '
C* 
C*(2)  Remove the heading blanks.
C* 
      LOUT = 0
      DO 200 IHEAD = 1, LIN
        IF(INPUT(IHEAD:IHEAD).NE.' ') GO TO 220
200   CONTINUE
      LENGTH = 0
      RETURN
C  
220   CONTINUE
      IF(OPTION.EQ.'H') THEN
        OUTPUT = INPUT(IHEAD:LIN)
        LENGTH = LIN - IHEAD + 1
        RETURN
      ENDIF
C* 
C*(3) If Option = ' ' or 'B' then remove in between blanks.
C  
      IOUT  = 0
      DO 300 I = IHEAD, LIN-1
        IF(OPTION.NE.'H'.AND.
     >    (INPUT(I:I).NE.' '.OR.INPUT(I:I+1).NE.'  ') ) THEN
          IOUT = IOUT + 1
          OUTPUT(IOUT:IOUT) = INPUT(I:I)
        ENDIF
300   CONTINUE
      IF(OPTION.NE.'H'.AND.INPUT(LIN:LIN).NE.' ') THEN
        IOUT = IOUT + 1
        OUTPUT(IOUT:IOUT) = INPUT(LIN:LIN)
      ENDIF
      LENGTH = IOUT
C* 
C*(4)  Terination
C  
      RETURN
      END
