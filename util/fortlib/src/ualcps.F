C***********************************************************************
C* 
C* --------------------------------------------------======
C* SUBROUTINE UALCPS(NUNIT, DSN, DISP, RECFM, NSPACE, NRET)
C* --------------------------------------------------======
C* 
C*(Function)
C*   Allocate dataset.
C* 
C*(Input)
C*   NUNIT  : Fortran logical unit #.
C*   DSN    : Data set name, fully qualified.
C*   DISP   : Disposition.
C*           OLD, SHR, MOD, SHRR, SHRW  : to allocate existing file.
C*           NEW, RENEW     : to allocate new file.
C*   RECFM  : Record format
C*            FB  : DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
C*            VB  : DCB=(RECFM=VB,LRECL=255,BLKSIZE=3120)
C*            FBA : DCB=(RECFM=FBA,LRECL=137,BLKSIZE=3120)
C*            VBA : DCB=(RECFM=VBA,LRECL=163,BLKSIZE=23476)
C*            VBS : DCB=(RECFM=VBS,LRECL=23472,BLKSIZE=23476)
C*            VBSS: DCB=(RECFM=VBS,LRECL=23472,BLKSIZE=23476,
C*                       BUFNO=10,OPTCD=C)
C*   NSPACE : Primary and secondary allocation space in TRK unit.
C* 
C*(Output)
C*   NRET   : Return code, = 0 for normal return
C*          : otherwise error.
C* 
C*   To use this routine, DSN=SYS2.LINKLIB and S000.FORTLIB
C*  must be allocated to SYSLIB in LKED step.
C* 
C*(Author)
C*   A. Miyamoto  12-March-1987  Original version.
C* 
C***********************************************************************
C  
      SUBROUTINE UALCPS(NUNIT, DSN, DISP, RECFM, NSPACE, NRET)
C  
      CHARACTER*(*) DSN, DISP, RECFM
C  
      CHARACTER     DDNAME*8, WDSN*56, WDISP*5, WRECFM*3
      CHARACTER     WCMD1*256,WCMD2*256
      CHARACTER     FORM*12, ACTION*12, STATUS*8
      CHARACTER*8   IOSB(2)/'FT06F001','FT05F001'/
C  
C======< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) When DISP=OLD, MOD, SHR case.
C ----------------------------------------------------------------------
C  
      WDSN   = DSN
      WDISP  = DISP
      WRECFM = RECFM
      IF(WDISP(1:3).EQ.'NEW')   GO TO 300
      IF(WDISP(1:5).EQ.'RENEW') GO TO 200
      IF(WRECFM(1:4).EQ.'VBSS') GO TO 400
C  
      IF(WRECFM(1:3).EQ.'VBS') THEN
        FORM = 'UNFORMATTED'
      ELSE
        FORM = 'FORMATTED'
      ENDIF
C  
      IF(WDISP(1:3).EQ.'OLD') THEN
        ACTION = 'BOTH'
        STATUS = 'OLD'
      ELSEIF(WDISP(1:4).EQ.'SHRR') THEN
        ACTION = 'READ'
        STATUS = 'SHR'
      ELSEIF(WDISP(1:4).EQ.'SHRW') THEN
        ACTION = 'WRITE'
        STATUS = 'SHR'
      ELSEIF(WDISP(1:3).EQ.'SHR') THEN
        ACTION = 'BOTH'
        STATUS = 'SHR'
      ELSEIF(WDISP(1:3).EQ.'MOD') THEN
        ACTION = 'BOTH'
        STATUS = 'MOD'
      ENDIF
C  
      OPEN(UNIT=NUNIT,FILE=DSN,ACCESS='SEQUENTIAL',FORM=FORM,
     .     STATUS=STATUS,ACTION=ACTION,IOSTAT=NRET)
   
      RETURN
C  
C ----------------------------------------------------------------------
C (2) DISP = 'NENEW', delete data set, if exists.
C ----------------------------------------------------------------------
C  
200   CONTINUE
C  
      CALL UCSTRP(WDSN,' ',WCMD1, LW)
      WCMD2 = ''''//WCMD1(:LW)//''''
      CALL DSNCHK(WCMD2, IRET)
      IF(IRET.LT.8) THEN
        WCMD2 = 'DELETE '''//WCMD1(:LW)//''''
        CALL IPFCMD(IRT1, IRT2, WCMD2, 255, IOSB)
        IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
          NRET = IRT1*10000 + IRT2
          RETURN
        ENDIF
      ENDIF
      GO TO 310
C  
C ----------------------------------------------------------------------
C (3) Create New file.
C ----------------------------------------------------------------------
C  
300   CONTINUE
      CALL UCSTRP(WDSN,' ',WCMD1, LW)
C  
C (3.1) Define attribute.
C  
310   CONTINUE
      FORM = 'FORMATTED'
      IF(WRECFM(1:3).EQ.'FB ') THEN
         WCMD1 = 'ATTRIB ATR RECFM(F B) LRECL(80) BLKSIZE(3120) '//
     >           ' DSORG(PS)'
      ELSEIF(WRECFM(1:3).EQ.'FBA') THEN
         WCMD1 = 'ATTRIB ATR RECFM(F B A) LRECL(137) BLKSIZE(23427)'//
     >           ' DSORG(PS)'
      ELSEIF(WRECFM(1:3).EQ.'VB ') THEN
         WCMD1 = 'ATTRIB ATR RECFM(V B) LRECL(255) BLKSIZE(3120) '//
     >           ' DSORG(PS)'
      ELSEIF(WRECFM(1:3).EQ.'VBA') THEN
         WCMD1 = 'ATTRIB ATR RECFM(V B A) LRECL(163) BLKSIZE(23476)'//
     >           ' DSORG(PS)'
      ELSEIF(WRECFM(1:3).EQ.'VBS') THEN
         WCMD1 = 'ATTRIB ATR RECFM(V B S) LRECL(23472) '//
     >           ' BLKSIZE(23476) DSORG(PS)'
         FORM  = 'UNFORMATTED'
      ENDIF
C  
      CALL IPFCMD(IRT1, IRT2, WCMD1, 255, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        NRET = IRT1*10000 + IRT2
        RETURN
      ENDIF
C  
C(3.2) Create data set.
C  
      WRITE(DDNAME,'(''FT'',I2.2,''F001'')') NUNIT
      WRITE(WCMD1,320) DDNAME,WDSN(:LW), NSPACE, NSPACE
320   FORMAT('ALLOC DD(',A,') DS(''',A,''') NEW CA SP(',I5.5,',',I5.5,
     >       ') T REUSE USING(ATR) RELEASE ')
C  
CC    PRINT *,' WCMD1=',WCMD1
      CALL IPFCMD(IRT1, IRT2, WCMD1, 255, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        NRET = IRT1*10000 + IRT2
        RETURN
      ENDIF
   
C  
C (3.3) Open data set.
C  
      OPEN(UNIT=NUNIT,ACCESS='SEQUENTIAL',FORM=FORM,
     .     STATUS='UNKNOWN',ACTION='BOTH',IOSTAT=NRET)
   
      RETURN
C  
C ----------------------------------------------------------------------
C (4) Create New file.
C ----------------------------------------------------------------------
C  
400   CONTINUE
      CALL UCSTRP(WDSN,' ',WCMD1, LW)
      FORM = 'UNFORMATTED'
      ACTION = 'READ'
      STATUS = 'SHR'
      WCMD1 = 'ATTRIB ATR INPUT BUFNO(10) OPTCD(C)'
C  
      CALL IPFCMD(IRT1, IRT2, WCMD1, 255, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        NRET = IRT1*10000 + IRT2
        RETURN
      ENDIF
C  
C(4.2) Allocate file.
C  
      WRITE(DDNAME,'(''FT'',I2.2,''F001'')') NUNIT
      WRITE(WCMD1,420) DDNAME,WDSN(:LW)
420   FORMAT('ALLOC DD(',A,') DS(''',A,''') SHR REUSE USING(ATR)')
C  
      CALL IPFCMD(IRT1, IRT2, WCMD1, 255, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        NRET = IRT1*10000 + IRT2
        RETURN
      ENDIF
   
C  
C (3.3) Open data set.
C  
      OPEN(UNIT=NUNIT,ACCESS='SEQUENTIAL',FORM=FORM,
     .     STATUS='UNKNOWN',ACTION=ACTION,IOSTAT=NRET)
   
      RETURN
      END
