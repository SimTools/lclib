C***********************************************************************
C* 
C*  ---------------------------------=====
C*  Subroutine Blddb( INPUT, OUTDCB, NRET )
C*  ---------------------------------=====
C* 
C*(Function)
C*   Build Data base information.
C*   Called from subroutine BUILDM, when first character of a line is =,
C*  and fills data base information.
C*   Syntax is as follows.
C* 
C*   (Valid command)
C*     =DATA.FORMAT         ; includes DD statement for format data.
C*     =DATA.REDUCTION      ; includes DD statement for reduction data.
C*     =DATA.PRODUCTION     ; includes DD statement for production data.
C*     =CONSTANT.REDUCTION  ; includes default data statement for the
C*                           reduction job constants.
C*     =CONSTANT.PRODUCTION ; includes default data statement for the
C*                           production job constants.
C* 
C*   (Operand)
C*     EXP(n)           ; Specify Experiment #.
C*     RUN(n1:m1,n2:m2) ; Specify run #.
C*     DDNAME(ddname)   ; only for DATA command, defines DD name to
C*                       assign.  Default is GO.FT01F001
C* 
C*(Input)
C*   INPUT  : Input JCL card.
C*   OUTDCB : DCB data for output file.
C* 
C*(Output)
C*   NRET   : Return code.
C*          > 0, When = statement is not recognized as DB command.
C*          = 0, for normal return.
C*          < 0, when failed to obtain data base information.
C* 
C*(Author)
C*   A. Miyamoto  06-Jun-1986  Original version.
C* 
C***********************************************************************
C  
      SUBROUTINE BLDDB(INPUT, OUTDCB, NRET)
C  
=EXPAND 'T#PB.DB.FORT/DBDSNB.inc'
C  
      CHARACTER*80   INPUT, OUTPUT, WRK1, WRK2
      CHARACTER*80   CEXP,  CRUN, DDNAME
      CHARACTER      DSN*56, VOL*6, TUNIT*3
      INTEGER*4      OUTDCB(50)
      CHARACTER*32   DSTYP1(5)
      CHARACTER*32   DBTYP1(5)
      DATA           DSTYP1/'DACU','RAW_DATA','FORMAT','REDUCTION',
     .                      'PRODUCTION'/
      DATA           DBTYP1/'DACU','RAW_DATA',
     .                      'FORMATTED_DATA','REDUCTION_DATA',
     .                      'PRODUCTION_DATA'/
      INTEGER*4      LDSTP1(5)
      DATA           LDSTP1/4,  8,  6,  9,  10/
C  
C  Stack for Experiment # and Run #.
C  
C     NEXPNO  ; Experiment # specified by =DATA statement.
C     NUMRUN  ; # of run# combination.
C     MX_NRN  ; Maximum Run# combination.
C     NRUNNO(1,i)  ; Start Run #.
C           (2,i)  ; Last Run #.
C  
      PARAMETER  (MX_NRN = 20)
      INTEGER*4   NRUNNO(2,MX_NRN)
C  
C  Default value.
C  
C     NDEEXP  ; Default experiment #.
C     DEFDDN  ; Default ddname.
C     LDEFDD  ; Character length of default ddname.
C  
      CHARACTER*11  DEFDDN
      DATA    DEFDDN/'GO.FT10F001'/
      DATA    LDEFDD/11/
      DATA    NDEEXP/1/
C  
C  
      DATA  IFIRST/1/, IFCNST/1/
C  
C======< Entry Point >==================================================
C  
C (1) Check validity of command.
C  
      NRET = 0
      IF(INPUT(1:6).EQ.'=DATA.') THEN
        IDCMD = 1
        IPNT  = 7
      ELSEIF(INPUT(1:9).EQ.'=CONSTANT') THEN
        IDCMD = 2
        IPNT  = 11
      ELSE
        NRET = 1
        RETURN
      ENDIF
C  
C (1.2) At the first call, open Data base file.
C  
      IF(IDCMD.EQ.1.OR.IDCMD.EQ.2.AND.INPUT(10:10).EQ.'.') THEN
        DO 100 ITYP = 1, 5
          IF(INPUT(IPNT:IPNT+LDSTP1(ITYP)).EQ.
     .       DSTYP1(ITYP)(:LDSTP1(ITYP)+1))    GO TO 110
100     CONTINUE
        PRINT *,'%Error BLDDB..Invalid command'
        GO TO 990
110     CONTINUE
        WRK1  = ' '//INPUT(IPNT+LDSTP1(ITYP):72)
C       PRINT *,' WRK1=',WRK1
      ENDIF
C  
C ---------------------------------------------------------------------
C (2) Interprete operand specified in =DATA command.
C ---------------------------------------------------------------------
C  
C (2.1) Get experiment #.
C  
      CALL UCPICK(WRK1,'EXP(',')',CEXP,LCEXP)
      IF(LCEXP.LE.0) THEN
        IF(IDCMD.EQ.1) NEXPNO = NDEEXP
      ELSE
        READ(CEXP(:LCEXP),*) NEXPNO
      ENDIF
C  
C (2.2) Get dd name.
C  
      IF(IDCMD.EQ.1) THEN
        CALL UCPICK(WRK1,'DDNAME(',')',DDNAME,LDDNAM)
        IF(LDDNAM.LE.0) THEN
          DDNAME = DEFDDN
          LDDNAM = LDEFDD
        ENDIF
      ENDIF
C  
C (2.3) Interprete Run #.
C  
      CALL UCPICK(WRK1,'RUN(',')',CRUN,LCRUN)
      IF(IDCMD.EQ.1) THEN
        IF(LCRUN.LE.0) THEN
          PRINT *,'%Error BLDDB .. Run# not specified.'
          GO TO 990
        ENDIF
        CALL DBIRUN(CRUN(:LCRUN),MX_NRN,NUMRUN,NRUNNO)
      ELSEIF(IDCMD.EQ.2) THEN
        IF(LCRUN.GT.0) CALL DBIRUN(CRUN(:LCRUN),MX_NRN,NUMRUN,NRUNNO)
      ENDIF
      IF(NUMRUN.LE.0) THEN
        PRINT *,'%Error BLDDB .. Run# specification invalid.'
        GO TO 990
      ENDIF
C  
C ----------------------------------------------------------------------
C (3) Access to the data base to get data set information.
C ----------------------------------------------------------------------
C  
      IF(IFIRST.EQ.1) THEN
        CALL DBINIT(NEXPNO)
        IFIRST = 0
      ENDIF
      IF(IDCMD.EQ.2.AND.IFCNST.EQ.1) THEN
        CALL DB_RCA(0,IRET)
        IF(IRET.LT.0) THEN
          PRINT *,'%Error BLDDB  Failed to allocate calibration ',
     .            ' constant file.'
          GO TO 990
        ENDIF
        IFCNST = 0
      ENDIF
      IF(IDCMD.EQ.2) GO TO 400
C  
C (3.1) Output DD statement.
C  
      ILOOP = 0
      OUTPUT = '//****** Input '//DBTYP1(ITYP)
      CALL PUT(OUTDCB,OUTPUT,ILL,80)
      INUMDD = 0
      DO 300 IP   = 1, NUMRUN
        DO 310 IRUN = NRUNNO(1,IP), NRUNNO(2,IP)
          CALL DBGDSN(IRUN,DBTYP1(ITYP),DSN,LDSN,VOL,LFN)
          IF(LDSN.LE.0) THEN
            PRINT *,'%Error BLDDB .. Data set not exist'
            PRINT *,'of type ',DBTYP1(ITYP)(:LDSTP1(ITYP)),' Run#=',IRUN
            GO TO 990
          ENDIF
          INUMDD = INUMDD + 1
          IF(DDNAME(LDDNAM-7:LDDNAM-6).EQ.'FT'.AND.
     .       DDNAME(LDDNAM-3:LDDNAM-3).EQ.'F') THEN
             WRITE(DDNAME(LDDNAM-2:LDDNAM),'(I3.3)') INUMDD
          ENDIF
          OUTPUT = '//'//DDNAME(:LDDNAM)//' DD '
          LOUT   = LDDNAM+6
          OUTPUT(LOUT+1:) = 'DSN='//DSN(:LDSN)//','
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(LFN.LE.0) THEN
            OUTPUT = '//      DISP=SHR,LABEL=(,,,IN)'
            CALL PUT(OUTDCB,OUTPUT,ILL,80)
            PRINT *,'*** Assign   ',DDNAME(LDDNAM-7:LDDNAM),
     .              ' to DSN=',DSN(:LDSN),
     .              'for Run#',IRUN,'Exp#',NEXPNO
          ELSE
            TUNIT = 'OMT'
            IF(VOL(1:3).EQ.'TOD') THEN
              TUNIT = 'OMT'
            ELSEIF(VOL(1:2).EQ.'ZT') THEN
              TUNIT = 'UMT'
            ELSEIF(VOL(1:1).EQ.'T') THEN
              TUNIT = 'CMT'
            ENDIF
            IF(INUMDD.LE.1) THEN
              OUTPUT ='//     DISP=(SHR,PASS),UNIT='//TUNIT//
     .                ',VOL=SER='//VOL(1:6)
            ELSE
              OUTPUT ='//     DISP=(SHR,PASS),UNIT=AFF='//
     .                 DDNAME(LDDNAM-7:LDDNAM-3)//'001'//
     .                ',VOL=SER='//VOL(1:6)
            ENDIF
            CALL PUT(OUTDCB,OUTPUT,ILL,80)
            PRINT *,'*** Assign   ',DDNAME(LDDNAM-7:LDDNAM),
     .              ' to DSN=',DSN(:LDSN),
     .              ' VOL=',VOL(1:6),
     .              ' for Run#',IRUN,'Exp#',NEXPNO
          ENDIF
310     CONTINUE
300   CONTINUE
      GO TO 900
C  
C ----------------------------------------------------------------------
C (4) For =CONSTANT statement.
C ----------------------------------------------------------------------
C  
400   CONTINUE
C  
C ----------------------------------------------------------------------
C (4.1) Output Run information.
C ----------------------------------------------------------------------
C  
      DO 410 IP   = 1, NUMRUN
        DO 420 IRUN = NRUNNO(1,IP), NRUNNO(2,IP)
          WRITE(OUTPUT,430) NEXPNO, IRUN
430       FORMAT(' DATA EXP(',I6,') RUN(',I8,')')
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
420     CONTINUE
410   CONTINUE
C  
C ----------------------------------------------------------------------
C (5)  Output Calibration data information.
C ----------------------------------------------------------------------
C  
      IF(ITYP.LE.0) THEN
        PRINT *,'%Error in BLDDB Data set type of =CONSTANT command ',
     .          ' is not specified yet.'
        GO TO 990
      ENDIF
      LTYP = LDSTP1(ITYP)
      DO 510 IT = 1, NUMTYP
        IF(NDSTYP(IT).NE.2) GO TO 510
        IF(DSNTYP(IT)(1:LTYP).NE.DSTYP1(ITYP)(1:LTYP)) GO TO 510
        LCTYP = INDEX(DSNTYP(IT),' ') - 1
        DO 520 IP   = 1, NUMRUN
        DO 530 IRUN = NRUNNO(1,IP), NRUNNO(2,IP)
          CALL DBGDSN(IRUN,DSNTYP(IT),DSN,LDSN,VOL,LFN)
          IF(LDSN.LE.0) THEN
            PRINT *,'%Error BLDDB .. Data set not exist'
            PRINT *,'of type ',DSNTYP(IT),' Run#=',IRUN
            GO TO 990
          ENDIF
          IF(LFN.EQ.0) THEN
            OUTPUT = ' CONSTANT    TYPE('//DSNTYP(IT)(:LCTYP)//') + '
          ELSE
            PRINT *,'%Error BLDDB .. Calibration constant of type ',
     .          DSNTYP(IT)(:LCTYP),' is not available as disk file.'
            PRINT *,'                It is on tape volume ',VOL(1:6),
     .              ' and file# ',LFN
            GO TO 990
          ENDIF
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
CNGY           IF(DSN(1:2).NE.'T#'.AND.DSN(1:2).NE.'T@') THEN
                 WRITE(OUTPUT,550) IRUN,DSN(:LDSN)
550              FORMAT('       RUN(',I6,') DSN(',A,')')
CNGY           ELSE
CNGY             WRITE(OUTPUT,552) IRUN,DSN(:LDSN)
CNGY 552         FORMAT('       RUN(',I6,') DSN(TRISTAN.',A,')')
CNGY           ENDIF
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
530     CONTINUE
520     CONTINUE
510   CONTINUE
C  
C ----------------------------------------------------------------------
C (9) Normal return
C ----------------------------------------------------------------------
C  
900   CONTINUE
      RETURN
C  
C (9.9) Error return
C  
990   CONTINUE
      NRET = -1
      PRINT *,'Error line is'
      PRINT *,INPUT
      RETURN
      END
