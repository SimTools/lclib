C*********************************************************************
C* 
C*    -----------------------------------------======
C*    SUBROUTINE USORTC(NREC, N1ST, NLST, CBUF, IPNT)
C*    -----------------------------------------======
C* 
C*(Function)
C*    Sort character array.
C* 
C*(Input)
C*    NREC  : Number of records.
C*    N1ST  : Position of first character to be sorted.
C*    NLST  : Position of last character to be sorted.
C*          Input array is sorted with respect to the characters
C*          in the range between N1ST and NLST.
C*    CBUF  : Character array to be sorted.
C* 
C*(Output)
C*    IPNT  : Sorted pointer array.
C* 
C*(Author)
C* 
C*    A. Miyamoto  25-Sep-1984
C* 
C*********************************************************************
C* 
      SUBROUTINE USORTC(NREC, N1ST, NLST, CBUF, IPNT)
C  
      CHARACTER*(*) CBUF(NREC)
      CHARACTER*128 KEYVAL
      INTEGER*4     IPNT(NREC)
C  
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
      LKEY  = NLST - N1ST + 1
      IF(LKEY.GT.128) THEN
        PRINT *,'Error in USORTC .. Character length greater than 128.'
        STOP
      ENDIF
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
          KEYVAL = CBUF(IPNT(IM))(N1ST:NLST)
   
C* 
C*(4)   Swap the data with respect to the KEYVAL.
C* 
400       CONTINUE
410         IF(CBUF(IPNT(IL))(N1ST:NLST).GE.KEYVAL(1:LKEY)) GO TO 420
              IL = IL + 1
              IF(IL.GT.NREC)
     >        PRINT *,'Warning USORTC  IL greater than NREC...'
   
              GO TO 410
420         IF(CBUF(IPNT(IR))(N1ST:NLST).LE.KEYVAL(1:LKEY)) GO TO 430
              IR = IR - 1
              IF(IR.LE.0)
     >        PRINT *,'Warning USORTC IR less than 0....'
              GO TO 420
C  
430         CONTINUE
   
            IF(IL.LE.IR) THEN
C  
C    Swap data
C  
              ISAVE    = IPNT(IL)
              IPNT(IL) = IPNT(IR)
              IPNT(IR) = ISAVE
              IL = IL + 1
              IR = IR - 1
            END IF
C  
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
