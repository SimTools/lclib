C***********************************************************************
C* 
C* ---------------------------------------=====================
C* Subroutine UTSSEX( NUNIT, NCMD, CMDLIN, NRET, NOUT, CMDOUT )
C* ---------------------------------------=====================
C* 
C*(Function)
C*   Execute TSS command return answer to the CMDOUT buffer.
C* 
C*(Input)
C*   NUNIT   : Fortran Unit #, to be used to obtain TSS command output.
C*             = 0 to used default, i.e, 75
C*             < 0 to close buffer file before return to caller.
C*   NCMD    : # of command line.
C*   CMDLIN  : TSS command string.
C* 
C*(Output)
C*   NRET    : Return code of TSS command.
C*             = 0  ; success.
C*             =-1  ; TSS command error.
C*   NOUT    : TSS command output # of lines.
C*   CMDOUT  : TSS command output.
C* 
C*(Author)
C*   A. Miyamoto  15-Feb-1988  Original version.
C* 
C***********************************************************************
C  
      SUBROUTINE UTSSEX( NUNIT, NCMD, CMDLIN, NRET, NOUT, CMDOUT )
C  
      CHARACTER*(*) CMDLIN(*), CMDOUT(*)
C  
      CHARACTER     CMD*256
      CHARACTER*8   IOSB (2) /'FT75F001','FT05F001'/
      CHARACTER*8   IOSBT(2) /'FT06F001','FT05F001'/
      DATA          IFIRST/1/
C  
C ----------------------------------------------------------------------
C  
C  
C======< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) Authority check at the first call to this routine.
C ----------------------------------------------------------------------
C  
      LIN    = LEN(CMDLIN(1))
      LOUT   = LEN(CMDOUT(1))
      NRET   = -1
      IUNIT  = IABS( NUNIT )
      IF( NUNIT .EQ. 0 ) IUNIT = 75
C  
      IF( IFIRST .EQ. 1 ) THEN
C  
C ... Allocate files to receive DYNALOC return message.
C  
        WRITE( IOSB(1)(3:4),'(I2.2)') IUNIT
        CMD = 'ATTRIB DYNAFB LRECL(136) BLKSIZE(13600) RECFM(F B)'//
     .        ' DSORG(PS) '
        CALL IPFCMD( IR1, IR2, CMD, 256, IOSBT)
        CMD = 'ALLOC DD('//IOSB(1)//') NEW DELETE'//
     .        ' SP(1) T REUSE USING(DYNAFB) '
        CALL IPFCMD( IR1, IR2, CMD, 256, IOSBT)
        IF( IR1 .NE. 0 .OR. IR2 .NE. 0 ) THEN
          PRINT *,'%Error UTSSEX .. Failed to allocate ',
     .            'message file.'
          PRINT *,' CMD=',CMD(:100)
          PRINT *,' IR1=',IR1,' IR2=',IR2
          GO TO 900
        ENDIF
C  
        OPEN( IUNIT,ACCESS='SEQUENTIAL',FORM='FORMATTED',
     .        STATUS='UNKNOWN',ACTION='BOTH',IOSTAT=IOS)
        IF( IOS .NE. 0 ) THEN
          PRINT *,'%Error UTSSEX .. Failed to open message file.'
          PRINT *,' IOSTAT =',IOS
          GO TO 900
        ENDIF
C  
        IFIRST = 0
      ENDIF
C  
C  
C ----------------------------------------------------------------------
C (2) Execute TSS command.
C ----------------------------------------------------------------------
C  
      DO 200 ICMD = 1, NCMD
        CALL IPFCMD(IR1, IR2, CMDLIN(ICMD), LIN, IOSB )
        IF( IR1 .NE. 0 .OR. IR2 .NE. 0 ) THEN
          PRINT *,'%Error UTSSEX..Failed to execute TSS command.'
          PRINT *,CMDLIN(ICMD)(:LIN)
          PRINT *,' IPFCMD..IR1=',IR1,' IR2=',IR2
          GO TO 900
        ENDIF
200   CONTINUE
C  
C ----------------------------------------------------------------------
C (3) Get output buffer.
C ----------------------------------------------------------------------
C  
      NOUT = 0
      REWIND IUNIT
      DO 300 I = 1, 99999
        READ(IUNIT,'(A)',END=310) CMD(:136)
        NOUT = NOUT + 1
        CMDOUT(NOUT) = CMD(:136)
300   CONTINUE
C  
310   CONTINUE
      NRET = 0
C  
900   CONTINUE
      IF( NUNIT .LT. 0 ) THEN
        CLOSE( IUNIT )
        IFIRST = 1
      ENDIF
C  
      RETURN
      END
