C   04/06/93 401252144  MEMBER NAME  TBxR8U   (FORT)     M  FORTRAN
C+
C FILE : TBxR8U
C Description : Convert R4 data into IEEE format
C
C Author : Kouichi Nagai TOPAZ, Kobe Univ.
C Date : 05 - Jul - 1993
C-
C
      SUBROUTINE TBxR8U ( BUF, NW )
C
C ARG
C
      INTEGER * 4      BUF ( 2, 1 )
      INTEGER * 4      NW
C
C CONS
C
      INTEGER*4 BITMSK( 2, 0:2 )
#ifdef  MSP
      DATA BITMSK/Z00000000, 00000000,
#endif
#ifdef  MSP
     +            Z80000000, 00000000,
#endif
#ifdef  MSP
     +            Z7F000000, 00000000/
#endif
#ifdef  UNIX
      DATA BITMSK/x'00000000', x'00000000',
#endif
#ifdef  UNIX
     +            x'80000000', x'00000000',
#endif
#ifdef  UNIX
     +            x'7F000000', x'00000000'/
#endif
C
C VAR
C
      INTEGER*4 CONVR8(2),IR8IN(2),KFRAC(3)
C
C MAIN
C
      DO 100 IBUF=1,NW
C       PRINT 99,' BUF ', BUF(1,IBUF), BUF(2,IBUF)
        IR8IN(1)=BUF(1,IBUF)
        IR8IN(2)=BUF(2,IBUF)
C=== INITIALIZE OUTPUT BUFFER.
        CONVR8(1)=BITMSK(1,0)
        CONVR8(2)=BITMSK(2,0)
        IF (IR8IN(1).EQ.0.AND.IR8IN(1).EQ.0)GOTO 98
C=== SET SIGN BIT.
        CONVR8(1)=IAND(IR8IN(1),BITMSK(1,1))
C=== Look exponential bits.
        KEXP=ISHFT(IAND(IR8IN(1),BITMSK(1,2)),-24)-64
C       PRINT *, 'KEXP=' , KEXP
C=== Look frac1 (A PART EQUAL TO OR GREATER THAN 1.0)
        KF=14-KEXP
C       PRINT *,' KF = ' , KF
        KS=4*KF
C       PRINT *,' KS = ' , KS
        DO 14 I=23,20,-1
          IF(BTEST(IR8IN(1),I)) GOTO 10
  14    CONTINUE
        PRINT *,'#### Conversion error in R*8 conversion ####'
        PRINT 99,' BUF ', BUF(1,IBUF), BUF(2,IBUF)
  10    NEXP=I-KS+32
        IFU=32-I
        IMV=I-19
        KFRAC(1)=ISHFT(IR8IN(1),IFU)
        KFRAC(1)=ISHFT(KFRAC(1),-12)
        IFV=IFU-12
        KFRAC(2)=ISHFT(IR8IN(2),IFV)
C       PRINT *, ' I,IFU,IFV,IMV: ',I,IFU,IFV,IMV
        DO 17 KMV=1,IMV
          IF(BTEST(IR8IN(1),KMV-1)) KFRAC(3)=IBSET(KFRAC(3),31-IMV+KMV)
  17    CONTINUE
        KFRAC(2)=IOR(KFRAC(2),KFRAC(3))
        CONVR8(2)=KFRAC(2)
        CONVR8(1)=IOR(KFRAC(1),CONVR8(1))
        IEXP=1023+NEXP
        IEXP=ISHFT(IEXP,20)
        CONVR8(1)=IOR(IEXP,CONVR8(1))
  98    BUF(1,IBUF)=CONVR8(1)
        BUF(2,IBUF)=CONVR8(2)
  100 CONTINUE
      RETURN
99    FORMAT (1H , A5,Z8,2X,Z8 )
      END
