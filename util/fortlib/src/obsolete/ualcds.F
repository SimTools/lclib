C***********************************************************************
C* 
C* ---------------------------------------------------------------=====
C* SUBROUTINE UALCDS(NUNIT,DSN,DISP,SUNIT,NSP1,NSP2,VOLSER,OPTION,NRET)
C* ---------------------------------------------------------------=====
C* 
C*(Function)
C*   Allocate dataset, VBS format data.
C* 
C*(Input)
C*   NUNIT  : Fortran logical unit #.
C*   DSN    : Data set name, fully qualified.
C*   DISP   : Disposition.
C*           NEW, RENEW     : to allocate new file.
C*   SUNIT  : Allocation unit, CYL, or TRACKS
C*   NSP1   : Primary allocation quantity.
C*   NSP2   : Secondary allcoation quantity.
C*   VOLSER : Disk volume, Blank for not to specify.
C*   OPTION : Other option.
C*          = 'Fast'  for  DCB = (BUFNO=20,OPTCD=C'
C* 
C*(Output)
C*   NRET   : Return code, = 0 for normal return
C*          : otherwise error.
C* 
C*   To use this routine, DSN=SYS2.LINKLIB and S000.FORTLIB
C*  must be allocated to SYSLIB in LKED step.
C* 
C*(Author)
C*   A. Miyamoto  24-April-1987  Original version.
C* 
C***********************************************************************
C  
      SUBROUTINE UALCDS(NUNIT,DSN,DISP,SUNIT,NSP1,NSP2,VOLSER,OPTION,
     >                  NRET)
C  
      CHARACTER*(*) DSN, DISP, SUNIT,VOLSER,OPTION
C  
      CHARACTER     DDNAME*8, WDSN*56, WDISP*5
      CHARACTER     WCMD1*256,WCMD2*256
      CHARACTER     FORM*12
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
      IF(WDISP(1:5).NE.'RENEW')   GO TO 300
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
        CALL IPFCMD(IRT1, IRT2, WCMD2, 255,IOSB)
        IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
          NRET = IRT1*10000 + IRT2
          GO TO 900
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
      FORM  = 'UNFORMATTED'
      WCMD2 = 'ATTRIB ATR RECFM(V B S) LRECL(23472) '//
     >        ' BLKSIZE(23476) DSORG(PS)'
CC    PRINT *,' WCMD2=',WCMD2
      CALL IPFCMD(IRT1, IRT2, WCMD2, 255,IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        NRET = IRT1*10000 + IRT2
        GO TO 900
      ENDIF
C  
C(3.2) Create data set.
C  
      WRITE(DDNAME,'(''FT'',I2.2,''F001'')') NUNIT
      WRITE(WCMD1,320) DDNAME,WDSN(:LW), NSP1, NSP2
320   FORMAT('ALLOC DD(',A,') DS(''',A,''') NEW CA SP(',I5.5,',',I5.5,
     >       ') REUSE USING(ATR) RELEASE ')
      CALL UCSTRP(WCMD1,' ',WCMD2, LW)
CC    PRINT *,' WCMD2=',WCMD2
      IF(VOLSER(1:1).NE.' ') THEN
        WCMD2(LW+1:) = ' VOLUME('//VOLSER//')'
        LW           = LW + 15
      ENDIF
   
      IF(SUNIT(1:1).EQ.'C') THEN
        WCMD2(LW+1:) = ' CYL '
        LW           = LW + 5
      ELSE
        WCMD2(LW+1:) = ' T '
        LW           = LW + 3
      ENDIF
   
      WCMD2(LW+1:) = ' '//OPTION
      LOPT = LEN(OPTION)
      LW   = LW + LOPT + 1
   
CCC   PRINT *,' Execute IPFCMD =',WCMD2(:LW)
      CALL IPFCMD(IRT1, IRT2, WCMD2, LW, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        NRET = IRT1*10000 + IRT2
        GO TO 900
      ENDIF
   
C  
C (3.3) Open data set.
C  
      OPEN(UNIT=NUNIT,ACCESS='SEQUENTIAL',FORM=FORM,
     .     STATUS='UNKNOWN',ACTION='BOTH',IOSTAT=NRET)
CCC   PRINT *,' OPEN...NRET=',NRET
900   CONTINUE
      RETURN
      END
