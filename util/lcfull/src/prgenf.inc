C*
      INTEGER*4 NUMGEN, IUMGEN
      COMMON /PRGENF/ NUMGEN, IUMGEN
C*
C*    NUMGEN : # of event to be generated.
C*    IUMGEN : Current # of generated record.
C*
C*       Event generation will end after generating NUMGEN+2 records.
C*         ( including begin_Run and End_Run records.)
C* (1) NUMGEN is set to non zero value in PRDBIN, when GEN command
C*     is entered.
C* (2) IUMGEN is set to zero in PRJOBI, and accumulated in PRGEVT
C*     when TBINIT is called.
C*
