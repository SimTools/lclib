C**********************************************************************
C* 
C*  --------------------------------===============================
C*  Subroutine UMTCAT( PREF, MXxFIL, UMTDSN, VOLFSQ, CRDATE, NUMFIL )
C*  --------------------------------===============================
C* 
C*(Function)
C*  Get UMT catalog information into buffer.
C* 
C*(Input)
C*  PREF   ; Catalog prefix.  ex. TMT.TAYM
C*  MXxFIL ; Output buffer size.
C* 
C*(Output)
C*  UMTDSN : Char*44, UMT data set name
C*  VOLFSQ : Char*12, Vol=Ser(Char*6) + File seq#(Char*6)
C*  CRDATE : Char*8 , Created date of catalog.
C*  NUMFIL : I*4 , # of UMT file, negative when NUMFIL > MXxFIL
C*          = 0 In case of error.
C* 
C*(Note)
C*  This program uses, the file uid.@.UMT.LISTC to output LISTC output,
C*  and FORTRAN logical unit# 29 to read this file.
C* 
C*(Author)
C*  A. Miyamoto     26-Sep-1988  Original version.
C* 
C**********************************************************************
C* 
      SUBROUTINE UMTCAT( PREF, MXxFIL, UMTDSN, VOLFSQ, CRDATE, NUMFIL )
C  
      CHARACTER*(*) PREF
      CHARACTER     UMTDSN(MXxFIL)*44, VOLFSQ(MXxFIL)*12,
     >              CRDATE(MXxFIL)*8
C  
      CHARACTER     LSTCDS*80, WORK*160
      CHARACTER*8   IOSB(2)/'FT06F001','FT05F001'/
      CHARACTER*1   INPUT(256)
      CHARACTER*256 INSTR
      EQUIVALENCE  (INSTR(1:1), INPUT(1))
      DATA  IRU / 29/
C  
C =====< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) Allocate temporaly file to output LISTC, and issue LISTC
C ----------------------------------------------------------------------
C  
      NUMFIL     = 0
C  
      CALL GETUID(LSTCDS)
      LSTCDS(5:) = '.@.UMT.LISTC'
      LDSN       = INDEX(LSTCDS,' ') - 1
      WORK       = ''''//LSTCDS(:LDSN)//''''
      CALL DSNCHK(WORK, IRET)
      IF(IRET.LT.8) THEN
        WORK  = 'DELETE '''//LSTCDS(:LDSN)//''''
        CALL IPFCMD(IRT1, IRT2, WORK, 160, IOSB )
        IF(IRT1.NE.0.OR.IRT2.NE.0) RETURN
      ENDIF
      WORK = 'ALLOC DD(LISTCDD) DS('''//LSTCDS(:LDSN)//
     >       ''') NEW CA SP(10 10) T REUSE USING(VBA) '
      CALL IPFCMD(IRT1, IRT2, WORK, 160, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) RETURN
C  
      LPREF = LEN(PREF)
      WORK = 'LISTC L('''//PREF(:LPREF)//
     >       ''')  ALL OUTFILE(LISTCDD) NONVSAM'
      CALL IPFCMD(IRT1, IRT2, WORK, 160, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) RETURN
C  
C ----------------------------------------------------------------------
C (2) Read ate temporaly file to output LISTC, and issue LISTC
C ----------------------------------------------------------------------
C  
      CALL UALCPS(IRU, LSTCDS, 'SHRR', 'FB', 10, IRET)
      CALL ERRSET(212,256,-1, 1, 0, 1)
      NUMFIL  = 0
      NUMTRU  = 0
200   CONTINUE
      READ(IRU,'(163A1)',END=300,IOSTAT=IOS) INPUT
      CALL REDLEN(ILEN, IRET)
      IF( INSTR(2:7) .EQ. 'KQCAMS') GO TO 200
      IF( INSTR(2:8) .EQ. 'NONVSAM' ) THEN
          NUMTRU = NUMTRU + 1
          IF( NUMFIL .GE. MXxFIL )  GO TO 200
          NUMFIL = NUMFIL + 1
          IOK    = 1
          UMTDSN(NUMFIL) = INSTR(18:MIN(63,ILEN))
          CRDATE(NUMFIL) = ' '
          VOLFSQ(NUMFIL) = ' '
          GO TO 200
      ENDIF
      IF( IOK .NE. 0 .AND. INSTR(7:18) .EQ. 'ASSOCIATIONS' ) THEN
          IOK    = 0
          GO TO 200
      ENDIF
      IF( IOK .EQ. 0 ) GO TO 200
C  
      IF( IOK .EQ. 1 .AND. INSTR(14:20) .EQ. 'CREATED') THEN
        IOK = 2
      ELSEIF( IOK .EQ. 2 ) THEN
        READ(INSTR(14:18),*) NFDAY
        CALL UDAYFC(NFDAY, CRDATE(NUMFIL))
        IOK = 3
      ELSEIF( IOK .EQ. 3 .AND. INSTR(7:17) .EQ. '--VOLUMES--') THEN
        IOK = 4
      ELSEIF( IOK .EQ. 4 ) THEN
        VOLFSQ(NUMFIL) = INSTR(9:14)//INSTR(31:35)
      ENDIF
      GO TO 200
C  
C  
C ----------------------------------------------------------------------
C (3) end
C ----------------------------------------------------------------------
C  
300   CONTINUE
      CLOSE( IRU )
      IF( NUMTRU .GT. NUMFIL ) NUMFIL = -NUMTRU
      RETURN
      END
