C   21/06/86 807262003  MEMBER NAME  TVOPTN   (FORT)     M  FORTRAN
C.....  RETURN A UGS OPTION TO TVBGN
C
        SUBROUTINE TVOPTN(PARM,ISEL,LOPEN,LFIRST,LSCREN)
C
        CHARACTER PARM*(*)
        INTEGER*4 ISEL
        LOGICAL*4 LOPEN, LFIRST, LSCREN
C
        INTEGER*4 DDIN(10), DDEX(8)
        CHARACTER DDST
CYM     PARAMETER (ND = 4)
        PARAMETER (ND = 8)
        LOGICAL*1 LOAD(ND), LSEL(ND), LOPN(ND), LBAT(ND), IFTSS
        LOGICAL*1 LBATCH, LTSS
        CHARACTER MNEM(ND)*2, RESP*2, QUES(ND)*2, TLOG*64
        CHARACTER CARD(ND)*15, HELP(ND)*40
        CHARACTER * 4  USERID
        LOGICAL        LEXIST
        CHARACTER * 80  CMDBUF
        INTEGER   *  4  ISIZE, CMDLEN
        EXTERNAL  UPPER
        DATA CARD /
     &   'LASER          ',
     &   'F6242          ',
     &   'F6653          ',
     &   'F6657          ',
     &   'NECPC100       ',
     &   'T4019S         ',
     &   'TEKTRO         ',
     &   'TEK4010        '/
        DATA HELP /
     &   'Laser beam printer',
     &   'FACOM 6242 graphic',
     &   'FACOM 6653 graphic',
     &   'FACOM 6657 printer',
     &   'NECPC100   graphic',
     &   'TEKTRONIX 4019 SLV',
     &   'TEKTRONIX 4010 (S/1)',
     &   'TEKTRONIX 4010 (Native)' /
        DATA LBAT /.TRUE.,
     X             .FALSE.,
     X             .FALSE.,
     X             .TRUE.,
     X             .FALSE.,
     X             .FALSE.,
     X             .FALSE.,
     X             .FALSE./
CYM     DATA MNEM /'L',
CYM  X             '2',
CYM  X             '3',
CYM  X             'T'/
        DATA MNEM /'L ',
     X             '2 ',
     X             '3 ',
     X             '7 ',
     X             'N ',
     X             'T ',
     X             'TE',
     X             'TK'/
        DATA RESP /'T'/
C
C.....  get the list of loaded devices
C
        IF (LFIRST) THEN
          DO 11 I = 1,ND
            LOPN(I) = .FALSE.
11        CONTINUE
          DDIN(1) = 8
          CALL UGLA01(DDIN,DDST,DDEX)
          LOAD(1) = DDEX(1).GE.0
          CALL UGF201(DDIN,DDST,DDEX)
          LOAD(2) = DDEX(2).GE.0
          CALL UGF301(DDIN,DDST,DDEX)
          LOAD(3) = DDEX(1).GE.0
          CALL UGF701(DDIN,DDST,DDEX)
          LOAD(4) = DDEX(1).GE.0
          CALL UGN101(DDIN,DDST,DDEX)
          LOAD(5) = DDEX(1).GE.0
          CALL UGTS01(DDIN,DDST,DDEX)
          LOAD(6) = DDEX(1).GE.0
          CALL UGTE01(DDIN,DDST,DDEX)
          LOAD(7) = DDEX(1).GE.0
          CALL UGTK01(DDIN,DDST,DDEX)
          LOAD(8) = DDEX(1).GE.0
C
C.....  Check batch mode
C
          LTSS = IFTSS()
          LBATCH = .NOT.LTSS
          IF ( LBATCH ) THEN
            DO 12 I = 1,ND
              LOAD(I) = LOAD(I).AND.LBAT(I)
12          CONTINUE
          END IF
        END IF
C
C.....  Check logical names (1 level)
C
        DO 19 I = 1,ND
C         CALL LIB$SYS_TRNLOG(CARD(I),,TLOG)
          LSEL(I) = LOAD(I)
19      CONTINUE
C
C.....  Choose a device
C
        NSEL = 0
        ISEL = 0
        DO 110 I = 1,ND
          IF (LSEL(I)) THEN
            NSEL = NSEL+1
            ISEL = I
          END IF
110     CONTINUE
        IF (NSEL.NE.1) THEN
          NQUE = 0
          DO 120 I = 1,ND
            IF (LSEL(I)) THEN
              NQUE = NQUE+1
              QUES(NQUE) = MNEM(I)
            END IF
120       CONTINUE
          NERR = 0
1         CALL TVSLCT(QUES,NSEL,RESP)
          CALL TRCODE( RESP, 2, UPPER )
          NMATCH = 0
          DO 14 ISEL = 1,ND
            IF (LSEL(ISEL)) THEN
              IF(RESP.EQ.MNEM(ISEL)) GOTO 2
              IF(RESP(2:).EQ.' ' .AND.RESP(1:1).EQ.MNEM(ISEL)(1:1)) THEN
                NMATCH = NMATCH+1
                IMATCH = ISEL
              END IF
            END IF
14        CONTINUE
          IF (NMATCH.EQ.1) THEN
            ISEL = IMATCH
            GOTO 2
          END IF
          WRITE(6,101)
101       FORMAT (/' Available devices:'/)
          DO 15 ISEL = 1,ND
            IF (LSEL(ISEL)) THEN
              WRITE(6,102) MNEM(ISEL),HELP(ISEL)
102           FORMAT (6X,A,': ',A)
            END IF
15        CONTINUE
          RESP = ' '
          NERR = NERR+1
          IF (LBATCH .OR. NERR.GE.10) STOP
          GOTO 1
        END IF
2       CONTINUE
C       CALL LIB$SYS_TRNLOG(CARD(ISEL),,PARM)
        PARM = CARD(ISEL)
        LSCREN = .NOT.LBAT(ISEL)
        LOPEN = LOPN(ISEL)
        LOPN(ISEL) = .TRUE.
CYM
CYM  + 10 CARDS, ISEL=1...LASER, ISEL=4...F6657
CYM
       IF(ISEL.EQ.1)THEN
         IF( LTSS ) THEN
#if defined(G77_521_OR_GREATER)
           IDUSER = GETUID()
           CALL UVCOPY(IDUSER,USERID,1)
#else
           CALL GETUID( USERID )
#endif
           INQUIRE( FILE=USERID//'.@LASER',EXIST=LEXIST )
           IF( LEXIST ) THEN
             CALL IPFCMD( IER, IER, 'DELETE @LASER',13)
           END IF
         END IF
         ISIZE = 10
         CALL INTRD( 'Allocation Size',ISIZE )
         IF( LTSS ) THEN
           WRITE( CMDBUF,1000 ) ISIZE
1000       FORMAT(
     &     'ALLOC DD(UGDEVICE) DS(@LASER) SP(',I4,',10) T REU NEW' )
           CMDLEN = LENRD( CMDBUF )
           CALL IPFCMD( IER1, IER2, CMDBUF(1:CMDLEN), CMDLEN )
           IF( IER1 .NE. 0 .OR. IER2 .NE. 0 ) THEN
             WRITE( 6,* ) '%TVOPTN-E, ERROR IN ALLOCATION OF UGDEVICE'
CMSP              CALL EXIT
             stop
           END IF
         END IF
       END IF
       IF( ISEL.EQ.4 ) THEN
         IF( LTSS ) THEN
#if defined(G77_521_OR_GREATER)
           IDUSER = GETUID()
           CALL UVCOPY(IDUSER,USERID,1)
#else
           CALL GETUID( USERID )
#endif
           INQUIRE( FILE=USERID//'.@UG',EXIST=LEXIST )
           IF( LEXIST ) THEN
             CALL IPFCMD( IER, IER, 'DELETE @UG',10)
           END IF
           CALL IPFCMD( IER, IER,
     &     'ALLOC DD(FT10F001) DS(@UG) SP(10,10) T REU NEW',46)
         END IF
       END IF
CYM
CYM  +1 CARD Screen clear
CYM
      CALL TVCLR
      RETURN
      END
