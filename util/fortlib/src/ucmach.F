C*--------------------------------------------------------------------
C* 
C* --------------------------------------------------======
C*    Subroutine UCMACH(Tststr, Noref, Refstr, IMODE, IRTN)
C* --------------------------------------------------======
C* 
C*(Function)
C*     Seraches the string matched with the input Tststr.
C* 
C*(Input)
C* TSTSTR   : The test string.
C* NOREF    : The number of the refference string.
C* REFSTR   : The character array of NOREF element, containing
C*            the refference string.
C* IMODE    : Match test mode.
C*      = 1 : Returns the pointer to the REFSTR firstly matched with
C*           TSTSTR.
C*      = 2 : IRTN = 0, if more than two string can be matched.
C*      = 3 : IRTN = 0, if more than two string can be matched.
C*           and also print error message.
C*(Output)
C* IRTN    : return code.
C*     =-1 ... No match found.
C*     > 0 ... pointer to the refference string, with which string
C*           matches the test string.
C* 
C*(Author)
C* A. Miyamoto  25-Dec-1985   Original version.
C* 
C*--------------------------------------------------------------------
C* 
      SUBROUTINE UCMACH(TSTSTR, NOREF, REFSTR, IMODE, IRTN)
C* 
      CHARACTER*(*) TSTSTR, REFSTR(NOREF)
C* 
C*(1)  Check the obvious error, the first character of tststr is blank
C* 
      LTST  = LEN(TSTSTR)
      LREF  = LEN(REFSTR(1))
      IF(TSTSTR(1:1).EQ.' '.OR.LTST.GT.LREF) THEN
        IRTN  = -1
        RETURN
      ENDIF
C* 
C*(2)
C* 
      IFOUND = 0
      ILAST  = 0
      DO 200 I= 1, NOREF
        IF(REFSTR(I)(1:LTST).EQ.TSTSTR) THEN
          IRTN = I
          IF(IMODE.EQ.1) GO TO 300
          IFOUND = IFOUND + 1
          IF(IMODE.EQ.3) THEN
            IF(IFOUND.EQ.2) THEN
              PRINT *,'%UCMACH..',TSTSTR,' ambiguously matched with',
     >                  REFSTR(ILAST)
            ELSEIF(IFOUND.GE.2) THEN
              PRINT *,'%UCMACH..',TSTSTR,' ambiguously matched with',
     >                  REFSTR(IRTN)
            ENDIF
            ILAST = IRTN
          ENDIF
        ENDIF
200   CONTINUE
      IF(IMODE.EQ.1) IRTN = -1
C  
      IF(IFOUND.GT.1) IRTN = 0
300   CONTINUE
      RETURN
      END
