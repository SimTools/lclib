C**********************************************************************
C*
C*  To squeze data of JLC simulator.
C*
C*  A. Miyamoto   6-Aug-1991  Original version.
C*
C**********************************************************************
C*
C* -----------------
C* Subroutine SQUZEV( IDREC, LEVEL, IDEBUG, NRET )
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
      SUBROUTINE SQUZEV( IDREC, LEVEL, IDEBUG, NRET )
C
      PARAMETER  (MAXELM=500,MAXDAT=100)
      PARAMETER  (MAXTMP=30000)
      COMMON /SSWORK/ NEDAT(MAXELM), NWDAT(MAXELM),
     >       LOC1(MAXELM),LOC2(MAXELM),
     >       RTMP(MAXTMP)
C
      PARAMETER (MAXBUF=600000)
      COMMON /TBSBUF/ RBUF(MAXBUF)
C
      INTEGER*4 IBUF(MAXBUF)
      INTEGER*2 HBUF(2,MAXBUF),KBUF(2*MAXBUF)
      EQUIVALENCE (RBUF( 1 ),IBUF( 1 ),HBUF(1,1),KBUF(1) )
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
C (2) Compress CDC Track Parameter.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:CDC;Track_Parameter'
      BANK2 = 'Production:CDC_VTX;Track_Parameter'
      BANK3 = 'Z.Production:CDC_VTX;Track_Parameter'
C     BANK3 = 'T1'
      CALL TBLNKA(1,BANK2,'I*4 ',MAXELM,NEDAT,LOC2,NWDAT,NOELM)
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOEL1)
      IF( (NOELM.GT.0.AND.NOELM.LE.MAXELM) .AND.
     >    (NOEL1.GT.0.AND.NOEL1.LE.MAXELM) ) THEN
        NPD = 0
        DO 200 IEP = 1, NOELM
          IDP = LOC2(IEP)-1
          IDP1 = LOC1(IEP)-1
          IE = NEDAT(IEP)
          DO 210 ID = 1, 7
            RTMP(NPD+ID) = RBUF(ID+IDP1)
210       CONTINUE
          RTMP(NPD+8 ) = RBUF(9+IDP1)
          DO 215 ID = 11,18
            RTMP(NPD+ID-2) = RBUF(ID+IDP1)
215       CONTINUE
          DO 220 ID = 53, 55
            RTMP(NPD+ID-36) = RBUF(ID+IDP)
220       CONTINUE
          IDP1 = LOC1(IEP)-1
          DO 230 ID = 54, NWDAT(IEP)
            RTMP(NPD+ID-34) = RBUF(ID+IDP1)
230       CONTINUE
          KWDAT(IEP) = 19 + NWDAT(IEP) - 54 + 1
          NPD = NPD + KWDAT(IEP)
C         PRINT *,' Write.. IEP=',IEP,' KW=',KWDAT(IEP)
200     CONTINUE
        CALL TBCRTE(1,BANK3,0,0,IR)
        CALL TBPUTH(1,BANK3,1,NOELM,KWDAT,IR)
        CALL TBPUT (1,BANK3,2,NPD,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
        CALL TBDELB(1,BANK2,IR)
      ELSE
        IF ( NOELM.GT.0 .AND. NOEL1.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
 
C -------------------------------------------------------------------
C (3) Compress EMC;Hit_Cell bank.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:EMC;Hit_Cell'
      BANK2 = 'Z.Production:EMC;Hit_Cell'
C     BANK2 = 'E1'
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
      IF( NOELM.GT.0 .AND. NOELM.LE.MAXELM ) THEN
        DO 300 IEP = 1, NOELM
          IE        = NEDAT(IEP)
          IP        = 2*IEP
          RTMP(IP-1) = RBUF(LOC1(IE))
          RTMP(IP)   = RBUF(LOC1(IE)+1)
300     CONTINUE
        NW = 2*NOELM
        CALL TBCRTE(1,BANK2,0,0,IR)
        CALL TBPUT(1,BANK2,1,NW,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
      ELSE
        IF ( NOELM.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
 
C -------------------------------------------------------------------
C (3.2) Compress EMC;Cluster
C -------------------------------------------------------------------
C
      BANK1 = 'Production:EMC;Cluster'
      BANK2 = 'Z.Production:EMC;Cluster'
C     BANK2 = 'E2'
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
      IF( NOELM.GT.0 .AND. NOELM.LE.MAXELM ) THEN
        NPD = 0
        DO 320 IEP = 1, NOELM
          IE = NEDAT(IEP)
          IDP1 = LOC1(IEP) - 1
          DO 330 ID = 1, 7
            RTMP(NPD+ID ) = RBUF(ID+IDP1)
330       CONTINUE
          RTMP(NPD+8) = RBUF(14+IDP1)
          HPD = 2*NPD
          KTMP(17+HPD) = IBUF(15+IDP1)
          DO 340 ID = 1, IBUF(15+IDP1)
            KTMP(17+ID+HPD)= IBUF(15+ID+IDP1)
340       CONTINUE
          KWDAT(IEP) = 8 + (IBUF(15+IDP1)+2)/2
          NPD = NPD + KWDAT(IEP)
320     CONTINUE
        CALL TBCRTE(1,BANK2,0,0,IR)
        CALL TBPUTH(1,BANK2,1,NOELM,KWDAT,IR)
        CALL TBPUT (1,BANK2,2,NPD,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
      ELSE
        IF ( NOELM.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
C
C -------------------------------------------------------------------
C (3.4) Compress EMC;Cluster_to_Cell.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:EMC;Cluster_to_Cell'
      BANK2 = 'Z.Production:EMC;Cluster_to_Cell'
C     BANK2 = 'E3'
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
      IF( NOELM.GT.0 .AND. NOELM.LE.MAXELM ) THEN
        NPD = 0
        DO 350 IEP = 1, NOELM
          IE = NEDAT(IEP)
          IDP1 = LOC1(IEP) - 1
          KPD = 2*NPD
          DO 360 ID = 1, NWDAT(IEP)
            KTMP(KPD+ID)  = KBUF(ID+IDP1)
360       CONTINUE
          KWDAT(IEP) = (NWDAT(IEP)+1)/2
          NPD = NPD + KWDAT(IEP)
350     CONTINUE
        CALL TBCRTE(1,BANK2,0,0,IR)
        CALL TBPUTH(1,BANK2,1,NOELM,KWDAT,IR)
        CALL TBPUT (1,BANK2,2,NPD,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
      ELSE
        IF ( NOELM.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
 
 
C -------------------------------------------------------------------
C (4) Compress HDC;Hit_Cell bank.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:HDC;Hit_Cell'
      BANK2 = 'Z.Production:HDC;Hit_Cell'
C     BANK2 = 'H1'
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
      IF( NOELM.GT.0 .AND. NOELM.LE.MAXELM ) THEN
        DO 400 IEP = 1, NOELM
          IE = NEDAT(IEP)
          IP        = 2*IEP
          RTMP(IP-1) = RBUF(LOC1(IE))
          RTMP(IP)   = RBUF(LOC1(IE)+1)
400     CONTINUE
        NW = 2*NOELM
        CALL TBCRTE(1,BANK2,0,0,IR)
        CALL TBPUT(1,BANK2,1,NW,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
      ELSE
        IF ( NOELM.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
 
C -------------------------------------------------------------------
C (3.2) Compress HDC;Cluster
C -------------------------------------------------------------------
C
      BANK1 = 'Production:HDC;Cluster'
      BANK2 = 'Z.Production:HDC;Cluster'
C     BANK2 = 'H2'
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
      IF( NOELM.GT.0 .AND. NOELM.LE.MAXELM ) THEN
        NPD = 0
        DO 420 IEP = 1, NOELM
          IE = NEDAT(IEP)
          IDP1 = LOC1(IEP) - 1
          DO 430 ID = 1, 7
            RTMP(NPD+ID ) = RBUF(ID+IDP1)
430       CONTINUE
          RTMP(NPD+8 ) = RBUF(14+IDP1)
          KPD = 2*NPD
          KTMP(KPD+17) = IBUF(15+IDP1)
          DO 440 ID = 1, IBUF(15+IDP1)
            KTMP(KPD+17+ID)= IBUF(15+ID+IDP1)
440       CONTINUE
          KWDAT(IEP) = 8 + (IBUF(15+IDP1)+2)/2
          NPD = NPD + KWDAT(IEP)
420     CONTINUE
        CALL TBCRTE(1,BANK2,0,0,IR)
        CALL TBPUTH(1,BANK2,1,NOELM,KWDAT,IR)
        CALL TBPUT (1,BANK2,2,NPD,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
      ELSE
        IF ( NOELM.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
C
C -------------------------------------------------------------------
C (4.4) Compress HDC;Cluster_to_Cell.
C -------------------------------------------------------------------
C
      BANK1 = 'Production:HDC;Cluster_to_Cell'
      BANK2 = 'Z.Production:HDC;Cluster_to_Cell'
C     BANK2 = 'H3'
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
      IF( NOELM.GT.0 .AND. NOELM.LE.MAXELM ) THEN
        NPD = 0
        DO 450 IEP = 1, NOELM
          IE = NEDAT(IEP)
          IDP1 = LOC1(IEP) - 1
          KPD = 2*NPD
          DO 460 ID = 1, NWDAT(IEP)
            KTMP( KPD+ID) = KBUF(ID+IDP1)
460       CONTINUE
          KWDAT(IEP) = (NWDAT(IEP)+1)/2
          NPD = NPD + KWDAT(IEP)
450     CONTINUE
        CALL TBCRTE(1,BANK2,0,0,IR)
        CALL TBPUTH(1,BANK2,1,NOELM,KWDAT,IR)
        CALL TBPUT (1,BANK2,2,NPD,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
      ELSE
        IF ( NOELM.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
C
C     ITEST = 1
C     IF( ITEST.EQ.1 ) RETURN
C
C -------------------------------------------------------------------
C (5)   Compress Generator:Particle_List bank
C -------------------------------------------------------------------
C   # word = 11 for stable.
C
      BANK1 = 'Generator:Particle_List'
C     BANK2 = 'Z.Generator:Particle_List'
      BANK2 = 'G1'
      NPD   = 0
      CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
      IF( NOELM.GT.0.AND.NOELM.LE.MAXELM) THEN
        DO 500 IEP = 1, NOELM
          IE = NEDAT(IEP)
          IDP1 = LOC1(IEP) - 1
          IDAU = RBUF(12+IDP1)
          DO 510 ID = 1, 11
            RTMP(NPD+ID) = RBUF(ID+IDP1)
510       CONTINUE
          RDCY = ABS(RBUF(9+IDP1))
     >         + ABS(RBUF(10+IDP1)) + ABS(RBUF(11+IDP1))
          IF( IDAU.EQ.0 ) THEN
            HTMP(1,NPD+1) = RBUF(1+IDP1)
            HTMP(2,NPD+1) = RBUF(14+IDP1)
            IF( RDCY.LT.1.E-8 ) THEN
              KWDAT(IEP) = 8
            ELSE
              KWDAT(IEP) = 11
            ENDIF
          ELSE
            HTMP(1,NPD+1) = RBUF(1+IDP1)
            HTMP(2,NPD+1) = RBUF(14+IDP1)
            HTMP(1,NPD+4) = RBUF(4+IDP1)
            HTMP(2,NPD+4) = RBUF(12+IDP1)
            IF( RBUF(16).LT.1.E-8 ) THEN
              IF( RDCY.LT.1.E-8 ) THEN
                HTMP(1,NPD+9) = RBUF(13+IDP1)
                HTMP(2,NPD+9) = RBUF(14+IDP1)
                KWDAT(IEP) = 9
              ELSE
                HTMP(1,NPD+12) = RBUF(13+IDP1)
                HTMP(2,NPD+12) = RBUF(14+IDP1)
                KWDAT(IEP) = 12
              ENDIF
            ELSE
              HTMP(1,NPD+12) = RBUF(13+IDP1)
              HTMP(2,NPD+12) = RBUF(14+IDP1)
              RTMP(13+NPD) = RBUF(16+IDP1)
              RTMP(14+NPD) = RBUF(17+IDP1)
              KWDAT(IEP) = 14
            ENDIF
          ENDIF
          NPD = NPD + KWDAT(IEP)
500     CONTINUE
        CALL TBCRTE(1,BANK2,0,0,IR)
        CALL TBPUTH(1,BANK2,1,NOELM,KWDAT,IR)
        CALL TBPUT(1,BANK2,2,NPD,RTMP,IR)
        CALL TBDELB(1,BANK1,IR)
      ELSE
        IF ( NOELM.GT.0 ) THEN
           NRET = 1
        ENDIF
      ENDIF
C
C -------------------------------------------------------------------
C (6)   Compress Spring:Parton_List bank
C -------------------------------------------------------------------
C
C     BANK1 = 'Spring:Parton_List'
C     BANK2 = 'Z.Spring:Parton_List'
C     BANK2 = 'G0'
C     CALL TBLNKA(1,BANK1,'I*4 ',MAXELM,NEDAT,LOC1,NWDAT,NOELM)
C     IF( NOELM.GT.0.AND.NOELM.LE.MAXELM) THEN
C       NPD = 0
C       DO 600 IEP = 1, NOELM
C         IE = NEDAT(IEP)
C         IDP1 = LOC1(IEP) - 1
C         HTMP(1,NPD+1 ) = RBUF(1+IDP1)
C         HTMP(2,NPD+1 ) = RBUF(18+IDP1)
C         HTMP(1,NPD+2 ) = RBUF(2+IDP1)
C         HTMP(2,NPD+2 ) = RBUF(17+IDP1)
C         RTMP(NPD+3 ) = RBUF(3+IDP1)
C         HTMP(1,NPD+4)  = RBUF(4+IDP1)
C         HTMP(2,NPD+4)  = RBUF(14+IDP1)
C         HTMP(1,NPD+9)  = RBUF(13+IDP1)
C         HTMP(2,NPD+9)  = RBUF(12+IDP1)
C         DO 610 ID = 5,8
C          RTMP(NPD+ID ) = RBUF(ID+IDP1)
C10       CONTINUE
C         KWDAT(IEP) = 9
C         NPD = NPD + KWDAT(IEP)
C00     CONTINUE
C       CALL TBCRTE(1,BANK2,0,0,IR)
C       CALL TBPUTH(1,BANK2,1,NOELM,KWDAT,IR)
C       CALL TBPUT (1,BANK2,2,NPD, RTMP, IR )
C       CALL TBDELB(1,BANK1,IR)
C     ELSE
C       NRET = 1
C     ENDIF
C
      RETURN
      END
