C   26/05/87 705271117  MEMBER NAME  TRGTYP   (FORT)        FORTRAN
C   28/11/87 K.Fujii    Renamed: PRTRGT ---> PRTRG1
C                       PRTRG1 is for EXP #<6 using old TPC tirgger.
      SUBROUTINE PRTRG1(TYP,TIME,IDEB)
C INPUT
      INTEGER IDEB
C         IDEB=1 PRINT OUT RESULT
C  WORD12
C  32 --> 1 IDC R-PHI LATCHED DATA
C  WORD34
C  32 --> 1 IDC R-PHI LEVELED DATA
C  WORD56
C  32 --> 1 IDC R-Z LEVELED DATA
C  WORD7
C  1      BTOF BACK TO BACK
C  2      BTOF SINGLE
C  3      R-PHI HIGH
C  4      R-PHI LOW
C  5      LUM
C  6      RANDOM
C  7      NEUTRAL
C  8      BCL TOTAL L4
C  9      BCL TOTAL L3
C  10     BCL -Z L4
C  11     BCL -Z L3
C  12     BCL CENT L4
C  13     BCL CENT L3
C  14     BCL +Z L4
C  15     BCL +Z L3
C  16     NOT USED
C  WORD8
C  1      ECL -Z TOTAL L4
C  2      ECL -Z TOTAL L3
C  3      ECL -Z OUTER L4
C  4      ECL -Z OUTER L3
C  5      ECL -Z INNER L4
C  6      ECL -Z INNER L3
C  7      ECL +Z TOTAL L4
C  8      ECL +Z TOTAL L3
C  9      ECL +Z OUTER L4
C  10     ECL +Z OUTER L3
C  11     ECL +Z INNER L4
C  12     ECL +Z INNER L3
C  13     R-Z HIGH
C  14     R-Z LOW
C  15     BACK TO BACK TOF*IDC(R-PHI)
C  16     NOT USED
C  WORD 910
C  1 --> 32 TOF LATCHED DATA
C  WORD11
C  TDC DATA 1 0.1562 NSEC
C  WORD12
C  NOT USED
C  WORD13
C  NOT USED
C  WORD14
C  TRIGEGR INFORMATION
C  1      TPC CHIMMNY
C  2      TPC ANTI-CHIMMNY
C  3      FORCE TRIGGER
C  WORD15 TPC ANTI-CHIMMNY
C  1-6 WIRE 2
C  7-14 WIRE 42
C  15 TRIGGER MASK
C  WORD16 TPC CHIMMNY
C  1-6 WIRE 2
C  7-14 WIRE 42
C  15 TRIGGER MASK
C
C
C OUTPUT
      INTEGER TYP(2)
      REAL TIME(4)
C  TYP(1) TRIGGER TYPE
C         1 BCL L4
C         2 BCL L3 ANY OF TWO
C         3 ECL L4
C         4 ECL L3 ANY OF TWO
C         5 BCL AND ECL L3 CMB
C         6 TPC SINGLE ENDCAP
C         7 TPC 2 ENDCAP
C         8 TPC TRIGGER
C         9 LUM
C         10 RANDOM
      CHARACTER*4 NAME(16)
      DATA NAME/'BL4','BL3','EL4','EL3','CMB','TP1','TP2'
     1         ,'TPC','LUM','RAN','   ','   ','   ','   '
     1         ,'   ','   '/
      CHARACTER*1 SPACE(16),STAR,BLANK
      DATA STAR,BLANK/'*',' '/
C  TYP(2) PRE-TRIGGER TYPE
C         1 TOF SINGLE
C         2 TOF B TO B
C         3 IDC R-PHI SINGLE
C         4 TOF AND IDC R-PHI BTOB
C         5 R-Z
      CHARACTER*4 NAME2(16)
      DATA NAME2/'TF1','TF2','RP1','BTB','RZ1','   ','   '
     1         ,'   ','   ','   ','   ','   ','   ','   '
     1         ,'   ','   '/
C  TIME
C         1 NEUTRAL TIMING (NSEC)
      INTEGER*2 TRGLST(16)
      INTEGER LST(16)
      INTEGER BIT(2,32)
      LOGICAL LB(3),LE(4)
      INTEGER H3,H2C80,H555
#ifdef  MSP
      DATA H3,H2C80,H555/Z3,Z2C80,Z555/
#endif
#ifdef  UNIX
      DATA H3,H2C80,H555/X'00000003',X'00002C80',X'00000555'/
#endif
      DO 4 I=1,2
        TYP(I)=0
4     CONTINUE
      DO 5 I=1,4
        TIME(I)=0.0
5     CONTINUE
      DO 1 I=1,32
        BIT(1,I)=0
        BIT(2,I)=0
1     CONTINUE
      CALL TBGETH(1,'RAW_DATA:TRIGGER',1,NW,TRGLST,IRET)
      TIME(1)=TRGLST(11)*0.1562
      DO 2 I=1,16
        LST(I)=TRGLST(I)
2     CONTINUE
      IF(IAND(LST(7),ISHFT(1,4)).NE.0) BIT(1,9)=1
      IF(IAND(LST(7),ISHFT(1,5)).NE.0) BIT(1,10)=1
      IF(IAND(LST(14),H3).NE.0) BIT(1,6)=1
      IF(IAND(LST(14),ISHFT(1,2)).EQ.0) BIT(1,8)=1
      IF(IAND(LST(14),H3).EQ.H3) BIT(1,7)=1
      IF(IAND(LST(7),H2C80).NE.0) BIT(1,1)=1
      IF(IAND(LST(8),H555).NE.0) BIT(1,3)=1
      LB(1)=(IAND(LST(7),ISHFT(1,10)).NE.0)
      LB(2)=(IAND(LST(7),ISHFT(1,12)).NE.0)
      LB(3)=(IAND(LST(7),ISHFT(1,14)).NE.0)
      LE(1)=(IAND(LST(8),ISHFT(1,3)).NE.0)
      LE(2)=(IAND(LST(8),ISHFT(1,5)).NE.0)
      LE(3)=(IAND(LST(8),ISHFT(1,9)).NE.0)
      LE(4)=(IAND(LST(8),ISHFT(1,11)).NE.0)
      IF((LB(1).AND.LB(2)).OR.(LB(1).AND.LB(3)).OR.
     1   (LB(2).AND.LB(3))) BIT(1,2)=1
      IF((LE(1).OR.LE(2)).AND.(LE(3).OR.LE(4))) BIT(1,4)=1
      IF((LB(1).OR.LB(2).OR.LB(3)).AND.
     1   (LE(1).OR.LE(2).OR.LE(3).OR.LE(4))) BIT(1,5)=1
      IF(IAND(LST(7),ISHFT(1,1)).NE.0) BIT(2,1)=1
      IF(IAND(LST(7),ISHFT(1,0)).NE.0) BIT(2,2)=1
      IF(IAND(LST(7),ISHFT(1,2)).NE.0) BIT(2,3)=1
      IF(IAND(LST(8),ISHFT(1,14)).NE.0) BIT(2,4)=1
      IF(IAND(LST(8),ISHFT(1,12)).NE.0) BIT(2,5)=1
      DO 3 I=1,32
        TYP(1)=IOR(TYP(1),ISHFT(BIT(1,I),I-1))
        TYP(2)=IOR(TYP(2),ISHFT(BIT(2,I),I-1))
3     CONTINUE
      IF(IDEB.EQ.1) THEN
CCC     PRINT 100,LST
100     FORMAT(1H ,'TRIGGER LST',/,' '
     1            ,Z4,' ',Z4,' ',Z4,' ',Z4,' '
     1            ,Z4,' ',Z4,' ',Z4,' ',Z4,' '
     1            ,Z4,' ',Z4,' ',Z4,' ',Z4,' '
     1            ,Z4,' ',Z4,' ',Z4,' ',Z4)
       DO 101 I=1,16
         IF(BIT(1,I).EQ.1) THEN
           SPACE(I)=STAR
         ELSE
           SPACE(I)=BLANK
         END IF
101    CONTINUE
       PRINT *,'-----TRIGGER TYPE-----'
       PRINT 102,(SPACE(I),NAME(I),I=1,10)
102    FORMAT(1H ,10(A1,A4))
       DO 104 I=1,16
         IF(BIT(2,I).EQ.1) THEN
           SPACE(I)=STAR
         ELSE
           SPACE(I)=BLANK
         END IF
104    CONTINUE
       PRINT *,'-----PRE_TRIGGER TYPE-----'
       PRINT 102,(SPACE(I),NAME2(I),I=1,10)
       PRINT 103,TIME
103    FORMAT(1H ,'TRIGGER TIMING:',4(' ',F10.3,' NS'))
      END IF
      RETURN
      END
