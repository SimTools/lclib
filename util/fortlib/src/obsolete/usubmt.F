C***********************************************************************
C* 
C* ------------------------------------======
C* SUBROUTINE USUBMT(NUNIT, NLINE, JCL, NRET)
C* -------------------------------------======
C* 
C*(Function)
C*   Submit JCL.
C* 
C*(Input)
C*   NUNIT  : Unit# for INTRDR
C*   NLINE  : # of JCL cards.
C*   JCL(i) : JCL cards
C* 
C* 
C*(Output)
C*   NRET   : Return code.
C*         = -1 , In case of error.
C* 
C*   A. Miyamoto  16-Sep-1986  Original version.
C* 
C***********************************************************************
C  
      SUBROUTINE USUBMT(NUNIT, NLINE, JCL, NRET)
C  
      CHARACTER*(*) JCL(NLINE)
      CHARACTER     ATTRIB*80, ALLOC*80, FREE*40
      DATA  ATTRIB/'ATTRIB FBRDR RECFM(F B) BLKSIZE(3120) LRECL(80)'/
      DATA  ALLOC /'LPALLOC FT91F001 SYSOUT(H) PGM(INTRDR) USING(FBRDR)
     .  REUSE '/
      CHARACTER   IOSB(2)*8
      DATA        IFIRST/0/
      DATA        IOSB/'FT06F001','FT05F001'/
C  
C  
C======< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) Define Attribute at the first call to this routine.
C ----------------------------------------------------------------------
C  
      NRET   = 0
      IF(IFIRST.EQ.0) THEN
        CALL IPFCMD(IRT1, IRT2, ATTRIB, 80, IOSB)
        IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
          PRINT *,'%Error USUBMT .. Failed to define attribute FBRDR'
          PRINT *,' IPFCMD return code : IRT1=',IRT1,' IRT2=',IRT2
          CALL ERRTRA
          GO TO 900
        ENDIF
        IFIRST = 1
      ENDIF
C  
C ----------------------------------------------------------------------
C (2) Allocate INTRDR.
C ----------------------------------------------------------------------
C  
      WRITE(ALLOC(9:16),'(''FT'',I2.2,''F001'')') NUNIT
      CALL IPFCMD(IRT1, IRT2, ALLOC, 80, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        PRINT *,'%Error USUBMT .. Failed to allocate INTRDR.'
        PRINT *,' IPFCMD return code : IRT1=',IRT1,' IRT2=',IRT2
        CALL ERRTRA
        GO TO 900
      ENDIF
C  
C ----------------------------------------------------------------------
C (3) Open INTRDR
C ----------------------------------------------------------------------
C  
      OPEN(UNIT=NUNIT,ACCESS='SEQUENTIAL',FORM='FORMATTED',
     .    STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        PRINT *,'%Error USUBMT .. Falied to open INTRDR'
        PRINT *,' OPEN IOSTAT=',IOS
        CALL ERRTRA
        GO TO 800
      ENDIF
C  
C ----------------------------------------------------------------------
C (4) Output JCL.
C ----------------------------------------------------------------------
C  
      DO 400 IL = 1, NLINE
        WRITE(NUNIT,'(A)') JCL(IL)
400   CONTINUE
C  
C ----------------------------------------------------------------------
C (5) Close INTRDR, JCL is submitted.
C ----------------------------------------------------------------------
C  
      CLOSE(NUNIT)
      NRET = 0
      RETURN
C  
C ----------------------------------------------------------------------
C (8) Error to Open INTRDR.
C ----------------------------------------------------------------------
C  
800   CONTINUE
      WRITE(FREE,'(''FREE DD(FT'',I2.2,''F001'')') NUNIT
      CALL IPFCMD(IRT1,IRT2,FREE,LCMD,IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) THEN
        PRINT *,'%Error USUBMT ..Failed to free INTRDR'
        PRINT *,' IPFCMD return code : IRT1=',IRT1,' IRT2=',IRT2
      ENDIF
900   CONTINUE
      NRET = -1
      RETURN
      END
