C   20/03/86 610171152  MEMBER NAME  @HELP    (DISPLAY)  M  FORTRAN
      SUBROUTINE HELP
      PARAMETER   (NBUF1=15)
      PARAMETER   (IBUF =10)
      PARAMETER   (NCOMM=105)
      COMMON /COMMND/COMM(NCOMM)
      COMMON /DVADR/LWCC,LSBA,LSF,LIC,LRA,LPH,LUNPH,LUNPL,LNULL,LADR(64)
C
C  DISPLAY COMMAND
C
      CHARACTER*4   COMM
C
C  COMMAND & ORDER CODE
C
      LOGICAL*1   LWCC,LSBA,LSF,LIC,LRA,LPH,LUNPH,LUNPL,LNULL,LADR
      CHARACTER*1  WCC, SBA, SF, IC, RA, PH, UNPH, UNPL, NULL, ADR(64)
C
      EQUIVALENCE(LWCC,WCC),(LSBA,SBA),(LSF,SF),(LIC,IC),(LRA,RA),
     &           (LPH,PH),(LUNPH,UNPH),(LUNPL,UNPL),(LNULL,NULL),
     &           (LADR(1),ADR(1))
C
C
      INTEGER *4    IADR(NCOMM)
      INTEGER *2    IAD1,IAD2
      CHARACTER   BUF*10,BUF0*120,BUF1(7)*15
      LOGICAL*1   LBUF(10)
      EQUIVALENCE(BUF,LBUF)
      CHARACTER*1  PERIOD/'.'/
C
C  DISPLAY DATA
C
      CHARACTER    DAT0*20,DAT1(2)*40,DAT2(2)*40,DAT3*40
C
        DATA DAT0 /'Display Commands :  '/
        DATA DAT1 /'* To get all information, enter "ENTER" ',
     &             'key at the cursor,                      '/
        DATA DAT2 /'* an item information, move the cursor i',
     &             'n place of the preceding period,        '/
        DATA DAT3 /'  and then enter "ENTER" key.           '/
        DATA NDAT0,NDAT1,NDAT2,NDAT3/20,58,72,29/
C
C  INCLUDE #HELPMSG
C
      INCLUDE (#HELPMSG)
C
C   SCREEN CLEAR
C
      PRINT *,' '
C
C   PUT "DISPALY COMMANDS :" AT 2ND ROW.
C
1     NR=1
      NC=3
      NK=(NR-1)*80+NC
      IR=(NK-1)/64+1
      IL=NK-(NK-1)/64*64
      BUF0=WCC//SBA//ADR(IR)//ADR(IL)//SF//PH//DAT0//IC
      NBUF=7+NDAT0
      CALL TPUT(BUF0,NBUF,'F,W',IGR15)
C
C   PUT ITEMS
C
      DO 11 NR=3,17
      N=(NR-3)*7
      DO 10 NC=5,65,10
      NK=(NR-1)*80+NC
      IR=(NK-1)/64+1
      KR=    NK/64+1
      JR=(NK+1)/64+1
      IL=NK-(NK-1)/64*64
      KL=NK+1-    NK/64*64
      JL=NK+2-(NK+1)/64*64
      M=NC/10+1
      BUF1(M)=SBA//ADR(IR)//ADR(IL)//SF//UNPL//PERIOD//
     &        SBA//ADR(JR)//ADR(JL)//SF//PH//COMM(N+M)
      READ(ADR(KR),'(A1)')IAD1
      READ(ADR(KL),'(A1)')IAD2
      IADR(N+M)=IAD1*IAD2
10    CONTINUE
C
      BUF0=WCC//BUF1(1)//BUF1(2)//BUF1(3)//BUF1(4)//
     &     BUF1(5)//BUF1(6)//BUF1(7)
      NBUF=1+NBUF1*7
      CALL TPUT(BUF0,NBUF,'F,W',IGR15)
11    CONTINUE
C
C  CLEAR THE LAST ITEM
C
C     NR=17
C     NC=66
C     NK=(NR-1)*80+NC
C     IR=(NK-1)/64+1
C     IL=NK-(NK-1)/64*64
C     JC=77
C     JK=(NR-1)*80+JC
C     JR=(JK-1)/64+1
C     JL=JK-(JK-1)/64*64
C     BUF0=WCC//SBA//ADR(IR)//ADR(IL)//SF//PH//
C    &     RA//ADR(JR)//ADR(JL)//NULL
C     NBUF=10
C     CALL TPUT(BUF0,NBUF,'F,W',IGR15)
C
C  PUT "MESSAGE OF SELECTION"
C
      NR=20
      NK=(NR-1)*80+4
      IR=(NK-1)/64+1
      IL=NK-(NK-1)/64*64
      BUF0=WCC//SBA//ADR(IR)//ADR(IL)//DAT1(1)//DAT1(2)(1:NDAT1-40)
      NBUF=4+NDAT1
      CALL TPUT(BUF0,NBUF,'F,W',IGR15)
C
C  2ND MESSAGE
C
      NR=21
      NK=(NR-1)*80+4
      IR=(NK-1)/64+1
      IL=NK-(NK-1)/64*64
      BUF0=WCC//SBA//ADR(IR)//ADR(IL)//DAT2(1)//DAT2(2)(1:NDAT2-40)
      NBUF=4+NDAT2
      CALL TPUT(BUF0,NBUF,'F,W',IGR15)
C
C  3RD MESSAGE
C
      NR=22
      NK=(NR-1)*80+4
      IR=(NK-1)/64+1
      IL=NK-(NK-1)/64*64
      MK=(NR-1)*80+4+50
      MR=(MK-1)/64+1
      ML=MK-(MK-1)/64*64
      BUF0=WCC//SBA//ADR(IR)//ADR(IL)//DAT3(1:NDAT3)//
     &     SBA//ADR(MR)//ADR(ML)//SF//PH
      NBUF=9+NDAT3
      CALL TPUT(BUF0,NBUF,'F,W',IGR15)
C
C   GET AN APPROPRIATE ITEM
C
      CALL TGET(BUF,IBUF,'A,W',IGR15,IGR1)
C
C   BUFFER CLEAR
C
      NR=24
      LC=80
      NK=(NR-1)*80+LC
      IR=(NK-1)/64+1
      IL=NK-(NK-1)/64*64
      BUF0=WCC//SBA//ADR(1)//ADR(1)//RA//ADR(IR)//ADR(IL)//NULL
      NBUF=8
      CALL TPUT(BUF0,NBUF,'F,W',IGR15)
C
C   READ BUFFER ADDRESS
C
      READ(BUF,'(1X,2A1)')IAD1,IAD2
      JADR=IAD1*IAD2
      DO 12 I=1,NCOMM
      IF(IADR(I).EQ.JADR)THEN
        K=I
        GO TO 14
      ENDIF
12    CONTINUE
      DO 13 I=1,NCOMM
      WRITE(6,*)' ',MSG(I)
13    CONTINUE
      RETURN
14    WRITE(6,*)' ',MSG(K)
      RETURN
      END
