C***********************************************************************
C* 
C* ----------------------------------------------------=====
C* Subroutine UCTLAL(NUNIT, DSN, VOLSER, LPOS, OPTION, NRET)
C* ----------------------------------------------------=====
C* 
C*(Function)
C*   Dynammically allocates CTL files.
C* 
C*(Input)
C*   NUNIT   : Fortran Unit #.
C*   DSN     : Data set name.
C*   VOLSER  : Tape VOLSER (6 character)
C*   LPOS    : Data set sequence #.
C*   OPTION  : Options to allocate CTL.
C* 
C*(Output)
C*   NRET    : Return code.
C*             = 0  ; allocation success.
C*             =-1  ; When volume is not available.
C* 
C*(Format of OPTION string.)
C*   Specify any of following options.  Each option should separated
C*   by blank.
C* 
C*      WAIT(n) : Maximum wait time in minites when volume is not
C*                available,  Set n=0, so as not to wait.
C*      BUFNO(n) : buffer number ( n < 99 )
C*      OPTCD(c) : OPTCD
C*      NORETAIN : Does not retain volume.  Volume is dismounted
C*                 when file is closed.
C*      MESAGE( ) : optional message string to the log file.
C* 
C*(Author)
C*   A. Miyamoto  24-Sep-1987  Test version.
C*                 8-Jan-1988  Support two unit, CTL3 and CTL6
C*                24-DEC-1992  TEMP. MOD. BEFORE FACOM SHUT DOWN.
C*                18-Jan-1993  Modification after FACOM startup..
C*                 1-Jul-1993  Allow multi-volume allocation.
C* 
C***********************************************************************
C  
      SUBROUTINE UCTLAL( NUNIT, DSN, VOLSER, LPOS, OPTION, NRET)
C  
      CHARACTER*(*) DSN, OPTION
      CHARACTER*(*) VOLSER
C  
#include "uctl00.inc"
C  
      CHARACTER     CMD*256, WORK*256, MESAGE*24
      CHARACTER*8   IOSB(2) /'FT76F001','FT05F001'/
      CHARACTER*8   IOSBT(2)/'FT06F001','FT05F001'/
      CHARACTER     BUFNO*10, OPTCD*9, RETAIN*6, WATTIM*8
      DATA          WATTIM/'00010000'/
      CHARACTER*4   UNTCTL
C  
      DATA          IFIRST/1/
      DATA          IMUNIT/76/
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
      LDSN   = LEN(DSN)
      LOPT   = LEN(OPTION)
C  
      IF( IFIRST .EQ. 1 ) THEN
        CALL TIMEL( TIMLFT )
        CALL CLOCKM( ITIME )
        XCPU = TIMLFT + FLOAT(ITIME)/1000.
        IF( XCPU . LT. 61.0 ) THEN
          PRINT *,'%Error UCTLAL..Can not use this routine from ',
     .            'job class E.'
          GO TO 900
        ENDIF
C  
C ... Allocate files to receive DYNALOC return message.
C  
        CMD = 'ATTRIB DYNAFB LRECL(136) BLKSIZE(13600) RECFM(F B)'//
     .        ' DSORG(PS) '
        CALL IPFCMD( IR1, IR2, CMD, 256, IOSBT)
        CMD = 'ALLOC DD('//IOSB(1)//') NEW DELETE'//
     .        ' SP(1) T REUSE USING(DYNAFB) '
        CALL IPFCMD( IR1, IR2, CMD, 256, IOSBT)
        IF( IR1 .NE. 0 .OR. IR2 .NE. 0 ) THEN
          PRINT *,'%Error UCTLAL .. Failed to allocate ',
     .            'message file.'
          PRINT *,' CMD=',CMD(:100)
          PRINT *,' IR1=',IR1,' IR2=',IR2
          GO TO 900
        ENDIF
C  
        OPEN( IMUNIT,ACCESS='SEQUENTIAL',FORM='FORMATTED',
     .        STATUS='UNKNOWN',ACTION='BOTH',IOSTAT=IOS)
        IF( IOS .NE. 0 ) THEN
          PRINT *,'%Error UCTLAL .. Failed to open message file.'
          PRINT *,' IOSTAT =',IOS
          GO TO 900
        ENDIF
C  
        IFIRST = 0
        MOUNIT = 0
      ENDIF
C  
C ----------------------------------------------------------------------
C (2) Scan arguments and set parameters.
C ----------------------------------------------------------------------
C  
   
      CMD    = OPTION(:LOPT)
      CALL UCUPER( LOPT, CMD )
   
      BUFNO  = ' '
      CALL UCPICK( CMD(:LOPT), 'BUFNO(', ')', WORK, LWORK)
      IF( LWORK .GT. 0 ) BUFNO = 'BUFNO('//WORK(:MIN(LWORK,2))//')'
   
      OPTCD  = ' '
      CALL UCPICK( CMD(:LOPT), 'OPTCD(', ')', WORK, LWORK)
      IF( LWORK .GT. 0 ) OPTCD = 'OPTCD('//WORK(1:1)//')'
   
      RETAIN = 'RETAIN'
      INRETA = INDEX( CMD(:LOPT), 'NORETAIN')
      IF( INRETA .NE. 0 ) RETAIN = ' '
   
      LPWAIT = 30
      CALL UCPICK( CMD(:LOPT), 'WAIT(', ')', WORK, LWORK)
      IF( LWORK .GT. 0 ) READ( WORK(:LWORK), * ) LPWAIT
   
      MESAGE = ' '
      CALL UCPICK( CMD(:LOPT), 'MESAGE(', ')', WORK, LWORK)
      IF( LWORK .GT. 0 ) MESAGE = WORK(:MIN(24,LWORK))
   
      IPRINT = INDEX( CMD(:LOPT), 'NOPRINT')
   
C  
C ----------------------------------------------------------------------
C (3) If user allocated tape previously, dismounted it first.
C ----------------------------------------------------------------------
C  
      IF( MOUNIT .GT. 0 ) THEN
        CLOSE( MOUNIT )
CC      CALL UCTLA0( 'C', OLDVOL, MOPOS, ' ' )
      ENDIF
C  
C ----------------------------------------------------------------------
C (4) Create Command to allocate tape.
C ----------------------------------------------------------------------
C  
      CMD   = 'ATTRIB IN INPUT '//BUFNO//' '//OPTCD
C     PRINT *,CMD(1:128)
C  
      CALL IPFCMD(IR1, IR2, CMD , 128, IOSB)
      IF( IR1 .NE. 0 .OR. IR2 .NE. 0 ) THEN
        PRINT *,'%Error UCTLAL..Attribute command error..'
        PRINT *,' IPFCMD..IR1=',IR1,' IR2=',IR2
        GO TO 900
      ENDIF
C  
      UNTCTL = 'CTL6'
      UNTCTL = 'CTL3'
C     IF( VOLSER(1:2) .EQ. 'TB' .OR.
C    >    VOLSER(1:2) .EQ. 'TC' .OR.
C    >    VOLSER(1:2) .EQ. 'TU' )  UNTCTL = 'CTL3'
      IF( VOLSER(1:2) .EQ. 'TQ' .OR.
     >    VOLSER(1:2) .EQ. 'TA' ) UNTCTL = 'CTL5'
      IF( VOLSER(1:2) .EQ. 'TQ' ) THEN
          READ(VOLSER(3:6),*) ITRVOL
          IF( (ITRVOL.GE.446.AND.ITRVOL.LE.450) .OR.
     >        (ITRVOL.GE.461.AND.ITRVOL.LE.470) .OR.
     >        (ITRVOL.GE.482.AND.ITRVOL.LE.490) .OR.
     >        (ITRVOL.GE.651.AND.ITRVOL.LE.670) ) THEN
               UNTCTL = 'CTL4'
          ELSEIF( (ITRVOL.GE.5.AND.ITRVOL.LE.30) .OR.
     >        (ITRVOL.GE.91.AND.ITRVOL.LE.92) .OR.
     >        (ITRVOL.GE.94.AND.ITRVOL.LE.125) .OR.
     >        (ITRVOL.GE.167.AND.ITRVOL.LE.175) .OR.
     >        (ITRVOL.GE.191.AND.ITRVOL.LE.203) .OR.
     >        (ITRVOL.GE.213.AND.ITRVOL.LE.219) .OR.
     >        (ITRVOL.GE.230.AND.ITRVOL.LE.239) .OR.
     >        (ITRVOL.GE.261.AND.ITRVOL.LE.269) .OR.
     >        (ITRVOL.GE.301.AND.ITRVOL.LE.310) .OR.
     >        (ITRVOL.GE.321.AND.ITRVOL.LE.321) .OR.
     >        (ITRVOL.GE.671)                  ) THEN
               UNTCTL = 'CTL3'
          ELSE
               UNTCTL = 'CTL5'
          ENDIF
      ENDIF
      IF( VOLSER(1:2) .EQ. 'TR' ) THEN
          READ(VOLSER(3:6),*) ITRVOL
          IF( (ITRVOL.GE.2011.AND.ITRVOL.LE.2100) .OR.
     >        (ITRVOL.GE.931.AND.ITRVOL.LE.1100) .OR.
     >        (ITRVOL.GE.821.AND.ITRVOL.LE.848) ) THEN
               UNTCTL = 'CTL4'
          ELSE
               UNTCTL = 'CTL3'
          ENDIF
      ENDIF
      IF( VOLSER(1:2) .EQ. 'TT' ) THEN
          READ(VOLSER(3:6),*) ITRVOL
          IF( (ITRVOL.LE.500) ) THEN
               UNTCTL = 'MTL3'
          ELSE
               UNTCTL = 'MTL1'
          ENDIF
      ENDIF
      IF( VOLSER(1:2) .EQ. 'TD' .OR.
     >    VOLSER(1:2) .EQ. 'TL' .OR.
     >    VOLSER(1:2) .EQ. 'TM' ) UNTCTL = 'MTL3'
      IF( VOLSER(1:2) .EQ. 'TM' ) THEN
          READ(VOLSER(3:6),*) ITRVOL
          IF( (ITRVOL.GE.1271.AND.ITRVOL.LE.1390) ) THEN
               UNTCTL = 'MTL1'
          ELSE
               UNTCTL = 'MTL3'
          ENDIF
      ENDIF
C  
      WRITE(CMD , 410) NUNIT, DSN(:LDSN), VOLSER, LPOS, UNTCTL, RETAIN
410   FORMAT('ALLOC DD(FT',I2.2,'F001) DS(',A,') VOLUME(',A,
     >   ') LABEL(SL) POSITION(',I3.3,
     >   ') USING(IN) UNIT(',A,') SHR ',A )
C  
CCC   CALL UCTLA0( 'A', VOLSER, LPOS, MESAGE )
C  
C ----------------------------------------------------------------------
C (5) Loop till succeed to allocate file.
C ----------------------------------------------------------------------
C  
C     PRINT *,'Alloc CMD=',CMD(:128)
      IALOC = -2
      DO 500 IL = 0, LPWAIT
        CALL IPFCMD(IR1, IR2, CMD , 256, IOSB)
        IF( IR1 .EQ. 0 .AND. IR2 .EQ. 0 ) THEN
          IALOC = 0
          GO TO 600
        ELSEIF( IR1 .NE. 16 .OR. IR2 .NE. 12 ) THEN
          PRINT *,'%Error UCTLAL..Allocate  command error..'
          PRINT *,' IPFCMD..IR1=',IR1,' IR2=',IR2
          GO TO 900
        ENDIF
C  
C ... Read ALLOCATE command message and analize.
C  
        IALOC  = -1
        REWIND IMUNIT
        READ(IMUNIT,'(A)',END=520) WORK(:80)
CC      PRINT *,'Read 1=',WORK(:80)
        IF( INDEX(WORK(:80),'VOLUME NOT AVAILABLE+') .NE. 0 ) THEN
          WORK(81:) = WORK(:80)
          READ(IMUNIT,'(A)',END=520) WORK(:80)
CC        PRINT *,'Read 2=',WORK(:80)
          IF( INDEX(WORK(:80),
     .       'ALLOCATED TO ANOTHER JOB OR USER, TRY LATER').NE.0) THEN
            IF( IPRINT .NE. 0 ) THEN
               CALL DATE(WORK(201:208))
               CALL UCTIME(WORK(210:217))
               WORK(209:209) = ' '
               WORK(218:218) = ' '
               PRINT *,WORK(201:218),WORK(:80)
            ENDIF
            IF( IL .LT. LPWAIT ) CALL WAIT(WATTIM)
            REWIND IMUNIT
            GO TO 500
          ENDIF
          PRINT *,WORK(81:160)
          PRINT *,WORK(:80)
        ELSEIF( INDEX(WORK(:80),'NOT ALLOCATED+') .NE. 0 ) THEN
          WORK(81:) = WORK(80:)
          READ(IMUNIT,'(A)',END=520) WORK(:80)
          IF( INDEX(WORK(:80),'NO UNIT AVAILABLE') .NE. 0 ) THEN
            IF( IPRINT .NE. 0 ) THEN
               CALL DATE(WORK(201:208))
               CALL UCTIME(WORK(210:217))
               WORK(209:209) = ' '
               WORK(218:218) = ' '
               PRINT *,WORK(201:218),WORK(:80)
            ENDIF
            IF( IL .LT. LPWAIT ) CALL WAIT(WATTIM)
            REWIND IMUNIT
            GO TO 500
          ENDIF
          PRINT *,WORK(81:160)
          PRINT *,WORK(:80)
        ELSE
          PRINT *,WORK(:80)
        ENDIF
510     CONTINUE
        READ(IMUNIT,'(A)',END=520) WORK(:80)
C       PRINT *,'Read??'
        PRINT *,WORK(:80)
        GO TO 510
520     CONTINUE
        PRINT *,'%Error UCTLAL .. Invalid return message from ',
     .          ' allocation command.'
        GO TO 900
   
500   CONTINUE
      GO TO 800
C  
C ----------------------------------------------------------------------
C (6) Open file.
C ----------------------------------------------------------------------
C  
600   CONTINUE
      OPEN(UNIT=NUNIT,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     .       STATUS='UNKNOWN',ACTION='READ',IOSTAT=IOS)
      IF( IOS .NE. 0 ) THEN
        PRINT *,'%Error UCTLAL..Failed to open file'
        GO TO 900
      ENDIF
C  
C ----------------------------------------------------------------------
C (7) Svae status information.
C ----------------------------------------------------------------------
C  
      MOUNIT = NUNIT
      MOPOS  = LPOS
      MOVOL  = VOLSER
C  
800   CONTINUE
      CALL IPFCMD(IR1, IR2, 'FREE ATTRLIST(IN)   ', 20, IOSBT)
      NRET   = IALOC
      RETURN
C  
C ----------------------------------------------------------------------
C (9) Error stop.
C ----------------------------------------------------------------------
C  
900   CONTINUE
      PRINT *,' DSN    =',DSN(:LDSN)
      PRINT *,' VOLSER =',VOLSER
      PRINT *,' File#  =',LPOS
      CALL ERRTRA
      STOP
      END
