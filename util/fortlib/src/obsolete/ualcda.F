C***********************************************************************
C* 
C* ---------------------------------------------------======
C* SUBROUTINE UALCDA(NUNIT, DSN, DISP, LRECL, NSPACE,  NRET)
C* ---------------------------------------------------======
C* 
C*(Function)
C*   Allocate direct access file.
C* 
C*(Input)
C*   NUNIT  : Fortran logical unit #.
C*   DSN    : Data set name, fully qualified.
C*          = ' ' to use static allocation.
C*   DISP   : Disposition.
C*           OLD, SHR, MOD, SHRR, SHRW  : to allocate existing file.
C*           NEW, RENEW     : to allocate new file.
C*   LRECL  : Record length, DCB of file will be,
C*            DCB=(RECFM=F,LRECL=lrecl,BLKSIZE=lrecl)
C*          When LRECL > 0, access format is FORMATTED.
C*                     < 0, access format is UNFORMATTED.
C*   NSPACE : Allocation space in unit of tracks.
C* 
C*(Output)
C*   NRET   : Return code, = 0 for normal return
C*          : otherwise error.
C* 
C*   To use this routine, DSN=SYS2.LINKLIB and S000.FORTLIB
C*  must be allocated to SYSLIB in LKED step.
C* 
C*(Author)
C*   A. Miyamoto   9-May-1987  Original version.
C* 
C***********************************************************************
C  
      SUBROUTINE UALCDA(NUNIT, DSN, DISP, LRECL, NSPACE, NRET)
C  
      CHARACTER*(*) DSN, DISP
C  
      CHARACTER     DDNAME*8, WDSN*56, WDISP*5
      CHARACTER     WCMD1*256,WCMD2*256
      CHARACTER     FORM*12, ACTION*12, STATUS*8
      CHARACTER*8   IOSB(2)/'FT06F001','FT05F001'/
C  
C======< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) Set parameters.
C ----------------------------------------------------------------------
C  
      WDSN   = DSN
      WDISP  = DISP
C     WRECFM = RECFM
C  
      IF(LRECL.GT.0) THEN
        FORM  = 'UNFORMATTED'
        IRECL = LRECL
      ELSE
        FORM = 'FORMATTED'
        IRECL = -LRECL
      ENDIF
C  
      IF(WDISP(1:3).EQ.'NEW')   GO TO 300
      IF(WDISP(1:5).EQ.'RENEW') GO TO 200
C  
C  
C ----------------------------------------------------------------------
C (1.1) When DISP=OLD, MOD, SHR case.
C ----------------------------------------------------------------------
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
      IF( WDSN(1:1) .EQ. ' ') THEN
        OPEN(UNIT=NUNIT,ACCESS='DIRECT',FORM=FORM,RECL=IRECL,
     .       STATUS=STATUS,ACTION=ACTION,IOSTAT=NRET)
      ELSE
        OPEN(UNIT=NUNIT,ACCESS='DIRECT',FORM=FORM,RECL=IRECL,
     .       FILE=WDSN,
     .       STATUS=STATUS,ACTION=ACTION,IOSTAT=NRET)
      ENDIF
   
      RETURN
C  
C ----------------------------------------------------------------------
C (2) DISP = 'RENEW', delete data set, if exists.
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
      WRITE(WCMD1,315) IRECL, IRECL
315   FORMAT('ATTRIB ATRB RECFM(F) LRECL(',I5.5,') BLKSIZE(',I5.5,
     >   ') DSORG(PS)')
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
      WRITE(WCMD1,320) DDNAME,WDSN(:LW), NSPACE
320   FORMAT('ALLOC DD(',A,') DS(''',A,''') NEW CA SP(',I5.5,
     >       ') T REUSE USING(ATRB) RELEASE ')
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
      OPEN(UNIT=NUNIT,ACCESS='DIRECT',FORM=FORM,RECL=IRECL,
     .     STATUS='UNKNOWN',ACTION='BOTH',IOSTAT=NRET)
   
      NRET = 0
C  
      RETURN
      END
