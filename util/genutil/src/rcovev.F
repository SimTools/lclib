C**********************************************************************
C*
C*  To recover data compressed by the subroutine SQUZEV.
C*
C*  A. Miyamoto   6-Aug-1991  Original version.
C*
C**********************************************************************
C*
C* -----------------
C* Subroutine RCOVEV( IDREC, LEVEL, IDEBUG, NRET )
C* -----------------
C*
C*(Function)
C*   To squeze event.
C*
C*(Input)
C*   IDREC ; Record ID.
C*   LEVEL ; Level flag.
C*   IDEBUG; Debug flag.
C*
C*(Output)
C*   NRET  ; Return code. = 0 to output this record.
C*                        =-1 to skip this events.
C*
C*(Author)
C*
C*  A. Miyamoto.   6-Aug-1991   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE RCOVEV( IDREC, LEVEL, IDEBUG, NRET )
C
      PARAMETER  (MAXELM=800,MAXDAT=100)
      PARAMETER  (MAXTMP=20000)
      COMMON /SSWORK/ NEDAT(MAXELM), NWDAT(MAXELM),
     >       RDATA(MAXDAT,MAXELM),
     >       LOC1(MAXELM),LOC2(MAXELM),
     >       RTMP(MAXTMP),
     >       RDAT20(20,MAXELM), RDAT03(3,MAXELM),RDAT55(55,MAXELM)
C     REAL*4 RDAT20(20,MAXELM), RDAT03(3,MAXELM),RDAT55(55,MAXELM)
C     EQUIVALENCE (RDAT20(1,1),RDATA(1,1),RDAT03(1,1),RDAT55(1,1))
C
      PARAMETER (MAXBUF=600000)
      COMMON /TBSBUF/ RBUF(MAXBUF)
C
      INTEGER*4 IBUF(MAXBUF)
      INTEGER*2 HBUF(2,MAXBUF),KBUF(2*MAXBUF)
      EQUIVALENCE (RBUF( 1 ),IBUF( 1 ),HBUF(1,1),KBUF(1) )
      INTEGER*4 IDATA(MAXDAT,MAXELM)
      INTEGER*2 HDATA(2,MAXDAT,MAXELM), KDATA(2*MAXDAT,MAXELM)
      EQUIVALENCE (RDATA(1,1),IDATA(1,1),HDATA(1,1,1),KDATA(1,1))
      INTEGER*2 KWDAT(MAXELM)
      INTEGER*4 ITMP(MAXTMP)
      INTEGER*2 HTMP(2,MAXTMP), KTMP(2*MAXTMP)
      EQUIVALENCE (ITMP(1), RTMP(1), HTMP(1,1),KTMP(1))
C
      CHARACTER*64 BANK1, BANK2, BANK3
C
C------------< Entry Point >-----------------------------------------
C
      NRET = -1
      IF( IDREC .EQ. 2. OR. IDREC .EQ. 8 ) THEN
        NRET = 0
        RETURN
      ELSEIF( IDREC .NE. 1 ) THEN
        RETURN
      ENDIF
      NRET = 0
C
C -------------------------------------------------------------------
C (2) De-Compress CDC Track Parameter.
C -------------------------------------------------------------------
C
C     ITEST = 2
C     IF( ITEST.EQ. 2 ) GO TO 7700
      BANK1 = 'Production:CDC;Track_Parameter'
      BANK2 = 'Production:CDC_VTX;Track_Parameter'
      BANK3 = 'Z.Production:CDC_VTX;Track_Parameter'
C     BANK3 = 'T1'
      CALL TBLNKA(1,BANK3,'I*2 ',MAXELM,NEDAT,LOC1,NWDAT,IR)
      IF( IR.LT.0 ) THEN
        NRET = 1
      ELSE
        CALL TBCRTE(1,BANK1,0,0,IR1)
        CALL TBCRTE(1,BANK2,0,0,IR2)
C     PRINT *,' RCOVEV called...IR1,IR2=',IR1,IR2
        IF( IR1.LT.0.OR.IR2.LT.0 ) THEN
          PRINT *,'Fatal error to create Bank ',
     >     'for CDC,VTX Track_Parameter ..IR1=',IR1,' IR2=',IR2
          PRINT *,' Bank1=',BANK1
          PRINT *,' Bank2=',BANK2
          STOP
        ENDIF
        NOELM = NWDAT(1)
        IF( NOELM.GT.MAXELM)  THEN
          PRINT *,' Too many elements in the Track bank'
          PRINT *,' NWDAT(1)=',NWDAT(1),' NWDAT(2)=',NWDAT(2)
          STOP
        ENDIF
        CALL UVZERO(NOELM*55, RDAT55)
        IPD1 = LOC1(1) - 1
        NPD  = (LOC1(2)+1)/2 - 1
        DO 200 IE  = 1, NOELM
          NEDAT(IE) = IE
          DO 210 ID = 1, 7
            RDAT55(ID,IE) = RBUF(NPD+ID)
C           PRINT *,' ID=',ID,' RTMP =',RTMP(NPD+ID)
210       CONTINUE
          RDAT55(9, IE) = RBUF(NPD+8)
          DO 215 ID = 11,18
            RDAT55(ID,IE) = RBUF(NPD+ID-2)
215       CONTINUE
          RDAT55(53,IE) = RBUF(NPD+53-36)
          RDAT55(54,IE) = RBUF(NPD+54-36)
          RDAT55(55,IE) = RBUF(NPD+55-36)
          NWDAT(IE) = 55
          NPD = NPD + KBUF(IPD1+IE)
200     CONTINUE
        CALL TBMPT(1,BANK2,NOELM,55,NEDAT,NWDAT,RDAT55,IR)
        NPD = (LOC1(2)+1)/2 - 1
        NPD1= LOC1(1) - 1
        IPD = 0
#ifdef C
     IF( IPD.EQ.0 ) GO TO 239
#endif 
        DO 220 IE = 1, NOELM
          NEDAT(IE) = IE
          DO 235 ID = 1, 52
            RTMP(ID+IPD) = RDAT55(ID,IE)
235       CONTINUE
          RTMP(53+IPD) = 0
          RTMP(54+IPD) = RBUF(NPD+20)
          RTMP(55+IPD) = RBUF(NPD+21)
          ITMP(56+IPD) = IBUF(NPD+22)
          DO 230 I = 1, ITMP(56+IPD)
            ITMP(56+I+IPD)= IBUF(NPD+22+I)
230       CONTINUE
          NWDAT(IE) = 56 + ITMP(IPD+56)
          NPD = NPD + KBUF(NPD1+IE)
          IPD = IPD + NWDAT(IE)
220     CONTINUE
        CALL TBQPT(1,BANK1,NOELM,NEDAT,NWDAT,RTMP,IR)
239     CONTINUE
        CALL TBDELB(1,BANK3,IR)
      ENDIF
C
7700  CONTINUE
C     ITEST = 1
C     IF( ITEST.EQ.1 ) GO TO 7800
C     PRINT *,'Complete CDC/VTX_Track'
C
C -------------------------------------------------------------------
C (3) Compress EMC;Hit_Cell bank.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:EMC;Hit_Cell'
      BANK2 = 'Z.Production:EMC;Hit_Cell'
C     BANK2 = 'E1'
      CALL TBGET(1,BANK2,1,NW,RTMP,IR)
      IF( IR.GT.0 ) THEN
        NOELM = NW/2
        DO 300 IE  = 1, NOELM
          NEDAT(IE) = IE
          IP        = 2*IE
          RDAT03(1,IE) = RTMP(IP-1)
          RDAT03(2,IE) = RTMP(IP)
          RDAT03(3,IE) = 0
          NWDAT(IE) = 3
300     CONTINUE
        CALL TBCRTE(1,BANK1,0,0,IR1)
        IF( IR1.LT.0 ) THEN
          PRINT *,'Fatal error to create Bank ',BANK1
          PRINT *,' IR1 =',IR1
          STOP
        ENDIF
        CALL TBMPT(1,BANK1,NOELM,3,NEDAT,NWDAT,RDAT03,IR)
        CALL TBDELB(1,BANK2,IR)
      ELSE
        NRET = 1
      ENDIF
 
C     PRINT *,'Complete EMC:Hit_Cell'
C
C -------------------------------------------------------------------
C (3.2) Compress EMC;Cluster
C -------------------------------------------------------------------
C
      BANK1 = 'Production:EMC;Cluster'
      BANK2 = 'Z.Production:EMC;Cluster'
C     BANK2 = 'E2'
C     CALL TBGETH(1,BANK2,1,NOELM, KWDAT,IR1)
C     CALL TBGET (1,BANK2,2,NW,RTMP,IR2)
      CALL TBLNKA(1,BANK2,'I*2 ',MAXELM,NEDAT,LOC1,NWDAT,IR)
      NOELM = NWDAT(1)
      IF( IR.GT.0.AND.NOELM.GT.0) THEN
         NPD = (LOC1(2)+1)/2 - 1
         IPD = 0
#ifdef C
      CALL UVZERO(MAXBUF,RTMP)
#endif 
         DO 320 IE = 1, NOELM
           NEDAT(IE) = IE
           DO 330 ID = 1, 7
             RTMP(IPD+ID) = RBUF(NPD+ID)
330        CONTINUE
           RTMP(14+IPD) = RBUF(NPD+8)
           KPD = 2*NPD
           DO 340 ID = 0, KBUF(17+KPD)
             ITMP(15+IPD+ID) = KBUF(17+KPD+ID)
340        CONTINUE
           NWDAT(IE) = 15 + ITMP(15+IPD)
           NPD = NPD + KBUF(LOC1(1)+IE-1)
           IPD = IPD + NWDAT(IE)
320      CONTINUE
         IF( IPD.GT.MAXBUF) THEN
           PRINT *,' Data size..exceeds MAXBUF for Bank=',BANK1
         ENDIF
         CALL TBCRTE(1,BANK1,0,0,IRT)
         CALL TBQPT(1,BANK1,NOELM,NEDAT,NWDAT,RTMP,IR)
         CALL TBDELB(1,BANK2,IR)
      ELSE
        NRET = 1
      ENDIF
C     PRINT *,'Complete EMC:Cluster'
 
C
C -------------------------------------------------------------------
C (3.4) Compress EMC;Cluster_to_Cell.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:EMC;Cluster_to_Cell'
      BANK2 = 'Z.Production:EMC;Cluster_to_Cell'
C     BANK2 = 'E3'
C     CALL TBGETH(1,BANK2,1,NOELM, KWDAT,IR1)
C     CALL TBGET (1,BANK2,2,NW,RTMP,IR2)
      CALL TBLNKA(1,BANK2,'I*2 ',MAXELM,NEDAT,LOC1,NWDAT,IR)
      IF( IR.GT.0 ) THEN
        NPD1 = LOC1(1) - 1
        NPD  = LOC1(2) - 1
        NOELM = NWDAT(1)
        NW    = NWDAT(2)
        DO 350 IE  = 1, NOELM
          NEDAT(IE) = IE
          NWDAT(IE) = KBUF(NPD1+IE)
350     CONTINUE
        DO 360 ID = 1, NW
          ITMP(ID) = KBUF(ID+NPD)
360     CONTINUE
        CALL TBCRTE(1,BANK1,0,0,IR)
        CALL TBQPT(1,BANK1,NOELM,NEDAT,NWDAT,ITMP,IR)
        CALL TBDELB(1,BANK2,IR)
      ELSE
        NRET = 1
      ENDIF
 
C     PRINT *,' De-compress EMC;Cluster_to_Cell'
C     ITEST = 1
C     IF( ITEST.EQ.1 ) RETURN
 
C -------------------------------------------------------------------
C (4) Compress HDC;Hit_Cell bank.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:HDC;Hit_Cell'
      BANK2 = 'Z.Production:HDC;Hit_Cell'
C     BANK2 = 'H1'
      CALL TBGET(1,BANK2,1,NW,RTMP,IR)
      IF( IR.GT.0 ) THEN
        NOELM = NW/2
        IF( NOELM.GT.MAXELM) THEN
          PRINT *,'Fatal error in RCOVEV..'
          PRINT *,'Too many elements (',NOELM,') in bank',BANK1
          PRINT *,'Max. should be less than ',MAXELM
          STOP
        ENDIF
        DO 400 IE  = 1, NOELM
          NEDAT(IE) = IE
          IP        = 2*IE
          RDAT03(1,IE) = RTMP(IP-1)
          RDAT03(2,IE) = RTMP(IP)
          RDAT03(3,IE) = 0
          NWDAT(IE) = 3
400     CONTINUE
        CALL TBCRTE(1,BANK1,0,0,IR1)
        IF( IR1.LT.0 ) THEN
          PRINT *,'Fatal error to create Bank ',BANK1
          PRINT *,' IR1 =',IR1
          STOP
        ENDIF
        CALL TBMPT(1,BANK1,NOELM,3,NEDAT,NWDAT,RDAT03,IR)
        CALL TBDELB(1,BANK2,IR)
      ELSE
        NRET = 1
      ENDIF
 
C     PRINT *,' De-compress HDC;Hit_Cell bank'
 
C -------------------------------------------------------------------
C (3.2) Compress HDC;Cluster
C -------------------------------------------------------------------
C
      BANK1 = 'Production:HDC;Cluster'
      BANK2 = 'Z.Production:HDC;Cluster'
C     BANK2 = 'H2'
      CALL TBLNKA(1,BANK2,'I*2 ',MAXELM,NEDAT,LOC1,NWDAT,IR)
      NOELM = NWDAT(1)
      IF( IR.GT.0.AND.NOELM.GT.0) THEN
         NPD = (LOC1(2)+1)/2 - 1
         IPD = 0
#ifdef C
      CALL UVZERO(MAXBUF,RTMP)
#endif 
         DO 420 IE = 1, NOELM
           NEDAT(IE) = IE
           DO 430 ID = 1, 7
             RTMP(IPD+ID) = RBUF(NPD+ID)
430        CONTINUE
           RTMP(14+IPD) = RBUF(NPD+8)
           KPD = 2*NPD
           DO 440 ID = 0, KBUF(17+KPD)
             ITMP(15+IPD+ID) = KBUF(17+KPD+ID)
440        CONTINUE
           NWDAT(IE) = 15 + ITMP(15+IPD)
           NPD = NPD + KBUF(LOC1(1)+IE-1)
           IPD = IPD + NWDAT(IE)
420      CONTINUE
         IF( IPD.GT.MAXBUF) THEN
           PRINT *,' Data size..exceeds MAXBUF for Bank=',BANK1
         ENDIF
         CALL TBCRTE(1,BANK1,0,0,IRT)
         CALL TBQPT(1,BANK1,NOELM,NEDAT,NWDAT,RTMP,IR)
         CALL TBDELB(1,BANK2,IR)
      ELSE
        NRET = 1
      ENDIF
C     PRINT *,' De-compress HDC;Cluster bank'
C
C -------------------------------------------------------------------
C (4.4) Compress HDC;Cluster_to_Cell.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:HDC;Cluster_to_Cell'
      BANK2 = 'Z.Production:HDC;Cluster_to_Cell'
C     BANK2 = 'H3'
      CALL TBLNKA(1,BANK2,'I*2 ',MAXELM,NEDAT,LOC1,NWDAT,IR)
      IF( IR .GT.0 ) THEN
        NPD1 = LOC1(1) - 1
        NPD  = LOC1(2) - 1
        NOELM = NWDAT(1)
        NW    = NWDAT(2)
        DO 450 IE  = 1, NOELM
          NEDAT(IE) = IE
          NWDAT(IE) = KBUF(NPD1+IE)
450     CONTINUE
        DO 460 ID = 1, NW
          ITMP(ID) = KBUF(ID+NPD)
460     CONTINUE
        CALL TBCRTE(1,BANK1,0,0,IR)
        CALL TBQPT(1,BANK1,NOELM,NEDAT,NWDAT,ITMP,IR)
        CALL TBDELB(1,BANK2,IR)
      ELSE
        NRET = 1
      ENDIF
C
#ifdef C
   ITEST = 1
#endif 
#ifdef C
   IF( ITEST.EQ.1 ) RETURN
#endif 
7800  CONTINUE
C
C -------------------------------------------------------------------
C (5)   Compress Generator:Particle_List bank
C -------------------------------------------------------------------
C   # word = 11 for stable.
C
      BANK1 = 'Generator:Particle_List'
C     BANK2 = 'Z.Generator:Particle_List'
      BANK2 = 'G1'
      CALL TBGETH(1,BANK2,1,NOELM, KWDAT,IR1)
      CALL TBGET (1,BANK2,2,NW,RTMP,IR2)
      IF( IR1.GT.0.AND.IR2.GT.0 ) THEN
        NPD = 0
        CALL UVZERO(NOELM*20, RDAT20)
        DO 500 IE  = 1, NOELM
          NEDAT(IE) = IE
          NWDAT(IE) = 20
          DO 510 ID = 2, 8
            RDAT20(ID,IE) = RTMP(NPD+ID)
510       CONTINUE
          RDAT20(1,IE) = HTMP(1,NPD+1)
          RDAT20(14,IE) = HTMP(2,NPD+1)
          IW = KWDAT(IE)
          IF( IW .EQ. 11 ) THEN
            DO 520 ID = 9, 11
              RDAT20(ID,IE) = RTMP(NPD+ID)
520         CONTINUE
          ELSEIF( IW .EQ. 9 .OR. IW.EQ. 12 ) THEN
            RDAT20(4,IE) = HTMP(1,NPD+4)
            RDAT20(12,IE) = HTMP(2,NPD+4)
            RDAT20(13,IE) = HTMP(1,NPD+IW)
            RDAT20(14,IE) = HTMP(2,NPD+IW)
          ELSEIF( IW.EQ.14 ) THEN
            RDAT20(4,IE) = HTMP(1,NPD+4)
            RDAT20(12,IE) = HTMP(2,NPD+4)
            RDAT20(13,IE) = HTMP(1,NPD+12)
            RDAT20(14,IE) = HTMP(2,NPD+12)
            RDAT20(16,IE) = RTMP(13+NPD)
            RDAT20(17,IE) = RTMP(14+NPD)
          ENDIF
          NPD = NPD + KWDAT(IE)
500     CONTINUE
        CALL TBCRTE(1,BANK1,0,0,IR)
        CALL TBMPT(1,BANK1,NOELM,20,NEDAT,NWDAT,RDAT20,IR)
        CALL TBDELB(1,BANK2, IR)
      ELSE
        NRET = 1
      ENDIF
C
C -------------------------------------------------------------------
C (6)   Compress Spring:Parton_List bank
C -------------------------------------------------------------------
C
C     BANK1 = 'Spring:Parton_List'
C     BANK2 = 'Z.Spring:Parton_List'
C     BANK2 = 'G0'
C     CALL TBGETH(1,BANK2,1,NOELM,KWDAT,IR1)
C     CALL TBGET (1,BANK2,2,NW,RTMP,IR2)
C     IF( IR1.GT.0.AND.IR2.GT.0 ) THEN
C       NPD = 0
C       CALL UVZERO(20*NOELM, RDAT20)
C       DO 600 IE  = 1, NOELM
C         NEDAT(IE) = IE
C         NWDAT(IE) = 20
C         RDAT20(1,IE) = HTMP(1,NPD+1)
C         RDAT20(18,IE) = HTMP(2,NPD+1)
C         RDAT20( 2,IE) = HTMP(1,NPD+2)
C         RDAT20(17,IE) = HTMP(2,NPD+2)
C         RDAT20( 3,IE) = RTMP( NPD+3)
C         RDAT20( 4,IE) = HTMP(1,NPD+4)
C         RDAT20(14,IE) = HTMP(2,NPD+4)
C         RDAT20(13,IE) = HTMP(1,NPD+9)
C         RDAT20(12,IE) = HTMP(2,NPD+9)
C         DO 610 ID = 5,8
C          RDAT20(ID,IE) = RTMP(ID+NPD)
C 10      CONTINUE
C         NPD = NPD + KWDAT(IE)
C 00    CONTINUE
C       CALL TBMPT(1,BANK1,NOELM,20,NEDAT, NWDAT,RDAT20,IR)
C       CALL TBDELB(2,BANK1,IR)
C     ELSE
C       NRET = 1
C     ENDIF
C
      RETURN
      END
C***********************************************************************
C*
C* -----------------
C* Subroutine DSRCOV
C* -----------------
C*
C*(Function)
C*   Select lepton-hadron mode of e+e- --> W+W-
C*
C*(Input & Output)
C*   None.
C*
C*(Relation)
C*   CALLed from FLANLI
C*
C*(Author)
C*   A. Miyamoto   23-Apr-1987  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DSRCOV
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
C FUNCTIONS
C
      EXTERNAL      RCOVEV
      CHARACTER*12  PRGNAM/'RECOVER_EVT'/
C
C ======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Define program name.
C ----------------------------------------------------------------------
C
      NPROG = NPROG + 1
      NAMCOM(NPROG) = PRGNAM
      ENTDIA(NPROG) = 0
      ENTSTA(NPROG) = 0
 
      ENTINI(NPROG) = 0
      ENTTER(NPROG) = 0
      ENTBRU(NPROG) = 0
      ENTERU(NPROG) = 0
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),RCOVEV)
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
C
C-----------------------------------------------------------------------
C 3. Define default calling sequence.
C-----------------------------------------------------------------------
C
      LISTSP(0)     = NENTR
      LISTSP(NENTR) = NENTR
C
C-----------------------------------------------------------------------
C 4. Define Level and Debug operand.
C-----------------------------------------------------------------------
C
      NJBLVL(20,1)  = 1
      EXELVL(NENTR) = NJBLVL(20,1)
      DBGLVL(NENTR) = NJBDBG(20,1)
C
      RETURN
      END
