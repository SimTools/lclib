CC*********************************************************************C
C*                                                                     *
C*---------------------------------==============                      *
C* Subroutine USTRNM( STRING, SEP, NONUM, NUMLST)                      *
C*---------------------------------==============                      *
C*                                                                     *
C*(Purpose)                                                            *
C*    Decode input string to Number list.                              *
C*                                                                     *
C*      N1a:N1b, N2a:N2b, N3a:N3b, ...............                     *
C*                                                                     *
C*      In this case SEP = ':'                                         *
C*                                                                     *
C*(Input)                                                              *
C*   STRING : Input character string.                                  *
C*   SEP    : Separator.                                               *
C*                                                                     *
C*(Output)                                                             *
C*   NONUM  : # of value list.                                         *
C*          = -1, when error in the format.                            *
C*   NUMLST : Decoded number array.                                    *
C*                                                                     *
C*(Example)                                                            *
C*                                                                     *
C*   CALL USTRNM('4, 5-20, 30-90, 100-123', '-', NONUM, NUMLST)        *
C*                                                                     *
C*   will return with                                                  *
C*     NONUM = 4                                                       *
C*     NUMLST(1,1) =  4 , (2,1) =   4                                  *
C*           (1,2) =  5 , (2,2) =  20                                  *
C*           (1,3) = 30 , (2,3) =  90                                  *
C*           (1,4) =100 , (2,4) = 123                                  *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto   29-Sep-1987   Original version.                    *
C*                                                                     *
CC**********************************************************************
 
      SUBROUTINE USTRNM( STRING, SEP, NONUM, NUMLST )
 
      CHARACTER*(*) STRING
      CHARACTER*1   SEP
      INTEGER*4     NUMLST(2, *)
 
C===========================<< Entry point >>==========================
 
C
C ---------------------------------------------------------------------
C (1) Initialization and remove trailling blanks.
C ---------------------------------------------------------------------
C
      LSTR  = LEN(STRING)
      DO 100 I = LSTR, 1, -1
        IF( STRING(I:I) .NE. ' ' ) GO TO 110
100   CONTINUE
      NONUM = 0
      RETURN
110   CONTINUE
      LSTR  = I
      CALL ERRSET(215, 1, 0, 0, 0, 215)
C
C ---------------------------------------------------------------------
C (2) Loop till end of last character
C ---------------------------------------------------------------------
C
      IPS    = 1
      IPE    = 0
      IEXSEP = 0
      NONUM  = 0
200   CONTINUE
      IPE   = IPE + 1
      IF( IPE .GT. LSTR ) GO TO 400
      IF( STRING(IPE:IPE) .EQ. SEP ) GO TO 300
      IF( STRING(IPE:IPE) .EQ. ',' ) GO TO 400
      GO TO 200
C
300   CONTINUE
      IEXSEP = 1
      IF( IPE-1 .LT. IPS ) THEN
        NUMPS = 0
      ELSE
        READ( STRING(IPS:IPE-1), * , IOSTAT=IOS) NUMPS
      ENDIF
      IPS    = IPE + 1
      GO TO 200
C
400   CONTINUE
      IF( IPE-1 .LT. IPS ) THEN
        NUMPE = 0
      ELSE
        IF( STRING(IPS:IPE-1) .EQ. ' ' ) THEN
          NUMPE = 0
        ELSE
          READ( STRING(IPS:IPE-1), * ,IOSTAT=IOS) NUMPE
        ENDIF
      ENDIF
      IPS    = IPE + 1
C
500   CONTINUE
      NONUM = NONUM + 1
      IF( IEXSEP .EQ. 0 ) NUMPS = NUMPE
      NUMLST(1, NONUM) = NUMPS
      NUMLST(2, NONUM) = NUMPE
      IEXSEP = 0
      IF( IPE .LE. LSTR ) GO TO 200
C
800   CONTINUE
      CALL ERRSET(215,256, 0, 0, 0, 215)
      RETURN
      END
