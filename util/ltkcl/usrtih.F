CC**********************************************************************
C*
C*    -----------------------------------------======
C*    SUBROUTINE USRTIH(NREC, LREC, KEY, IARY , IPNT)
C*    -----------------------------------------======
C*
C*(Function)
C*    Quick sorter of an interger array.
C*
C*(Input)
C*    NREC    :  Number of records.
C*    LREC    :  Record size.
C*    KEY     :  Key element pointer in a record.
C*    IARY    :  Integer array to be sortted.
C*
C*(Output)
C*    IPNT    :  Pointer containing the results.
C*
C*(Author)
C*
C*    Hirofumi Fujii    Jan 12, 1983
C*    A. Miyamoto         6-Sep-1985, modified the arguments.
C*
CC**********************************************************************
 
      SUBROUTINE USRTIH(NREC, LREC, KEY, IARY, IPNT)
C
      INTEGER*2   IARY(LREC, NREC), IPNT(NREC), KEYVAL
      PARAMETER  (ISTKSZ = 32 ,
     >            ILEFT  = 1,
     >            IRIGHT = 2 )
      INTEGER     ISTACK(2, ISTKSZ)
C
C*
C*(1)  Initialize.
C*
      DO 100 I = 1, NREC
         IPNT(I) = I
100   CONTINUE
      ISP   = 1
      ISTACK(ILEFT ,ISP) = 1
      ISTACK(IRIGHT,ISP) = NREC
C*
C*(2)  Get the most left and most right position from the stack.
C*
200   CONTINUE
        NLEFT  = ISTACK(ILEFT, ISP)
        NRIGHT = ISTACK(IRIGHT,ISP)
        ISP    = ISP - 1
C*
C*(3)  Determine the range to be sortted.
C*
300     CONTINUE
          IL   = NLEFT
          IR   = NRIGHT
          IM   = (IL+IR)/2
          KEYVAL = IARY(KEY, IPNT(IM))
C*
C*(4)   Swap the data with respect to the KEYVAL.
C*
400       CONTINUE
410         IF(IARY(KEY,IPNT(IL)).GE.KEYVAL) GO TO 420
              IL = IL + 1
              IF(IL.GT.NREC)
     >        PRINT *,'Warning in USORTI  IL greater than NREC...'
 
              GO TO 410
420         IF(IARY(KEY,IPNT(IR)).LE.KEYVAL) GO TO 430
              IR = IR - 1
              IF(IR.LE.0)
     >        PRINT *,'Warning in USORTI  IR less than 0....'
              GO TO 420
C
430         CONTINUE
C
C    Swap data.
C
            IF(IL.LE.IR) THEN
              ISAVE = IPNT(IL)
              IPNT(IL) = IPNT(IR)
              IPNT(IR) = ISAVE
              IL = IL + 1
              IR = IR - 1
            END IF
 
            IF(IL.LE.IR) GO TO 400
 
C*
C*(5)  Find out the next region to be sorted.
C*
            IF(IR-NLEFT.LT.NRIGHT-IL) THEN
              IF(IL.LT.NRIGHT) THEN
                ISP = ISP + 1
                ISTACK(ILEFT ,ISP) = IL
                ISTACK(IRIGHT,ISP) = NRIGHT
              ENDIF
              NRIGHT = IR
            ELSE
              IF(NLEFT.LT.IR) THEN
                ISP = ISP + 1
                ISTACK(ILEFT ,ISP) = NLEFT
                ISTACK(IRIGHT,ISP) = IR
              ENDIF
              NLEFT = IL
            ENDIF
 
          IF(NLEFT.LT.NRIGHT) GO TO 300
 
        IF(ISP.GT.0) GO TO 200
 
      RETURN
      END
