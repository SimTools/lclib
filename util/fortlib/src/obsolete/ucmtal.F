C***********************************************************************
C* 
C* ----------------------------------------------------------=====
C* Subroutine UCMTAL(NUNIT, TYPE, NEXP, NRUN, NFILE, OPTION, NRET )
C* ----------------------------------------------------------=====
C* 
C*(Function)
C*   Dynammically allocates Raw data or DST tapes.
C* 
C*(Input)
C*   NUNIT   : Fortran Unit #.
C*   TYPE    : 'FORMAT' to allocate format tape.
C*             'DST001' to allocate DST pass 1 tape.
C*             'DST002' to allocate DST pass 2 tape.
C*   NEXP    : Experiment #.
C*   NRUN    : Run #.
C*   NFILE   : File # of the run. ( = 1 to allocate first file.)
C* 
C*(Output)
C*   NRET    : Return code.
C*            >= 0  ; allocation success.
C*                    Value indicates the # of remaining files of the
C*                    run. = 0 indicates the last file.
C*             =-1  ; When volume is not available.
C*             =-2  ; When requested DST does not exists.
C*             =-3  ; When requested run  does not exists.
C*             =-4  ; When requested volume have been scratched.
C*             =-999; Faital error to allocate file.
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
C*      NOPRINT  : Does not print message.
C* 
C*(Author)
C*   A. Miyamoto  26-Sep-1987  Test version.
C*                30-Nov-1987  Support DST pass2 tape.
C*                24-AUG-1988  Support non-FORMAT/DST tapes
C*                 1-Jul-1993  Allow multi-volume allocation of FORMAT
C* 
C***********************************************************************
C  
      SUBROUTINE UCMTAL( NUNIT, TYPE, NEXP, NRUN, NFILE, OPTION, NRET )
C  
#include "prunls.inc"
C  
      CHARACTER*(*) TYPE, OPTION
      CHARACTER     DSN*44, WTYPE*20
      CHARACTER     VOLSER*60
      INTEGER*4     LVOLSR
C  
      INTEGER*4     LSTEXP, LSTRUN, LSTFIL
      SAVE          LSTEXP, LSTRUN, LSTFIL
      DATA          LSTEXP/-1/, LSTRUN/-1/, LSTFIL/-1/
C  
      PARAMETER    (MXxBUF=6000)
      INTEGER*4     NRNLST(MXxBUF)
      SAVE          NRNLST
C  
      INTEGER*4     LSTTYP
      SAVE          LSTTYP
      DATA          LSTTYP/0/
C  
C     LSTTYP  ; = 0 while allocating FORMAT/DST tapes.
C               > 0 type id of latest file.
C  
      CHARACTER     OLDTYP*20
      SAVE          OLDTYP
      DATA          OLDTYP/' '/
      PARAMETER    (MXxDAT=20000)
      COMMON /SSWORK/ IBUF(MXxDAT)
C  
C======< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) decode. OPTION parameter.
C ----------------------------------------------------------------------
C  
      LOPT = LEN( OPTION )
      IOPT = INDEX( OPTION, 'NOPRINT')
      LVOLSR = 6
C  
      WTYPE  = TYPE
      LTYPE  = MIN(NUCRMV(WTYPE(:LEN(TYPE))),8)
      CALL UCUPER(LTYPE, WTYPE)
CC    CALL MADEF(0, 'TAYM.@.TOPAZ.RUNLIST', ' ')
C  
C  
C ----------------------------------------------------------------------
C (2) Swich according to the type.
C ----------------------------------------------------------------------
C  
      IF( WTYPE(1:6) .EQ. 'FORMAT' .OR.
     >    WTYPE(1:6) .EQ. 'DST001' .OR.
     >    WTYPE(1:6) .EQ. 'DST002' )  GO TO 300
   
      GO TO 400
C  
C ----------------------------------------------------------------------
C (3) TYPE = 'FORMAT', 'DST001', or 'DST002' are requested.
C ----------------------------------------------------------------------
C  
300   CONTINUE
      LSTTYP = 0
      IF( NEXP .NE. LSTEXP .OR. NRUN .NE. LSTRUN ) THEN
        CALL MAOPEN( IRET )
        CALL MAGETR( 0, NEXP, NRUN, MXxRLW, NWRNL, LRUNLS )
        CALL MACLOS
        IF( NWRNL .LE. 0 ) THEN
          NRET = -3
          RETURN
        ENDIF
        LSTEXP = NEXP
        LSTRUN = NRUN
        LSTFIL = NFILE
      ENDIF
C  
C (3.1) Format tape.
C  
      IF( WTYPE(1:6) .EQ. 'FORMAT' ) THEN
        WRITE(DSN, 310) NEXP, NRUN
310     FORMAT('FORMAT.E',I5.5,'.R',I6.6)
        WRITE(VOLSER,'(A4,A2)') LRUNLS(18), LRUNLS(19)
        LVOLSR = 6
        LPOS  = LRUNLS(20)
        IDSIZE = LRUNLS(21)
        IF( VOLSER(1:2).EQ.'TD'.AND.IDSIZE.GT.4650 ) THEN
          READ(VOLSER(3:6),*) IVOLNW
          IVOLNW = IVOLNW + 1
          WRITE(VOLSER(7:13),'('',TD'',I4.4)') IVOLNW
          LVOLSR = 13
        ENDIF
        NRET  = 0
C  
C (3.2) DST pass1 tape.
C  
      ELSEIF( WTYPE(1:6) .EQ. 'DST001') THEN
        IF( NWRNL .LT. 90 ) THEN
          IF( IOPT .EQ. 0 )
     .       PRINT *,'%Error..UCMTAL DST001 for Exp#',NEXP,
     .            ' Run#', NRUN,' does not exists.'
          NRET = -2
          RETURN
        ENDIF
        NUMFIL = LRUNLS(71)
        IF( NUMFIL .LT. NFILE ) THEN
          IF( IOPT .EQ. 0 )
     .             PRINT *,'%Error..UCMTAL file #',NFILE,
     .            ' of DST001 for Exp#',
     .            NEXP, ' Run#', NRUN,' does not exists.'
          NRET = -2
          RETURN
        ENDIF
        WRITE(DSN, 320) NEXP, NRUN
320     FORMAT('DST001.E',I5.5,'.R',I6.6)
        IP         = 72 + 3*(NFILE-1)
        WRITE(VOLSER,'(A4,A2)') LRUNLS(IP), LRUNLS(IP+1)
        LVOLSR = 6
        LPOS       = LRUNLS(IP+2)
        NRET       = NUMFIL - NFILE
   
C  
C (3.3) DST pass2 tape.
C  
      ELSEIF( WTYPE(1:6) .EQ. 'DST002') THEN
        IF( NWRNL .LT.140 ) THEN
          IF( IOPT .EQ. 0 )
     .       PRINT *,'%Error..UCMTAL DST002 for Exp#',NEXP,
     .            ' Run#', NRUN,' does not exists.'
          NRET = -2
          RETURN
        ENDIF
        NUMFIL = LRUNLS(111)
        IF( NUMFIL .LT. NFILE ) THEN
          IF( IOPT .EQ. 0 )
     .             PRINT *,'%Error..UCMTAL file #',NFILE,
     .            ' of DST002 for Exp#',
     .            NEXP, ' Run#', NRUN,' does not exists.'
          NRET = -2
          RETURN
        ENDIF
        WRITE(DSN, 330) NEXP, NRUN
330     FORMAT('DST002.E',I5.5,'.R',I6.6)
        IP         = 112 + 3*(NFILE-1)
        WRITE(VOLSER,'(A4,A2)') LRUNLS(IP), LRUNLS(IP+1)
        LVOLSR = 6
        LPOS       = LRUNLS(IP+2)
        NRET       = NUMFIL - NFILE
   
      ENDIF
      GO TO 800
C  
C-----------------------------------------------------------------------
C (4) Non-FORMAT/DST tapes are requested.
C     If File information is not in buffer, load them.
C-----------------------------------------------------------------------
C  
400   CONTINUE
   
      NRET = 0
   
      IF( LSTTYP .GE. 0 .AND. OLDTYP(1:8) .EQ. WTYPE(1:8) ) GO TO 500
   
      LSTEXP = -1
      CALL MAOPEN( IRET )
      CALL MAGETE('FILELIST',  NEXP, MXxDAT,  NW, IBUF )
      NOTYP  = IBUF(3)
      DO 410 ITYP = 1, NOTYP
        IP   = 5 + IBUF(4)*(ITYP-1)
        WRITE(OLDTYP,'(2A4)') IBUF(IP), IBUF(IP+1)
        IF( OLDTYP(1:8) .NE. WTYPE(1:8) ) GO TO 410
        LSTTYP = ITYP
        GO TO 420
410     CONTINUE
        PRINT *,'%Error UCMTAL..Invalid argument ',
     .          ' TYPE was ',TYPE(:LTYPE)
        CALL ERRTRA
        STOP
   
420   CONTINUE
C  
      CALL MAGETE( WTYPE(:LTYPE), NEXP, MXxDAT, NW, IBUF )
      CALL MACLOS
C  
      IF( NW .LT. 0 ) THEN
        IF( IOPT .EQ. 0 )
     .  PRINT *,'%Error..UCMTAL ',WTYPE(:LTYPE),' file for Exp#',NEXP,
     .          'does not exist.'
        NRET  = -2
        RETURN
      ENDIF
C  
      NRNLST(1) = NEXP
      NRNLST(2) = IBUF(3)
      DO 440 IF = 1, IBUF(3)
        IG           = 5  + IBUF(4)*(IF-1)
        IP           = 3  + 3*(IF-1)
        IF( IP .GT. MXxBUF-2 .OR. IG. GT. MXxDAT - 5 ) THEN
          PRINT *,'%Error in UCMTAL...Buffer overflow.  ',
     >      ' Ask A.Miyamoto for the modification of the source files.'
          STOP
        ENDIF
        NRNLST(IP  ) = IBUF(IG+3)
        NRNLST(IP+1) = IBUF(IG+4)
        NRNLST(IP+2) = IBUF(IG+5)
440   CONTINUE
C  
C-----------------------------------------------------------------------
C (5) Prepare file names, etc.. for STRIP files.
C-----------------------------------------------------------------------
C  
500   CONTINUE
C  
      IF( NEXP .NE. NRNLST(1) ) THEN
        CALL MAOPEN(IRET)
        GO TO 420
      ENDIF
   
      IF( NRUN .GT. NRNLST(2) .OR. NRUN .LE. 0 ) THEN
        IF( IOPT .EQ. 0 )
     .  PRINT *,'%Error..UCMTAL ',WTYPE(:LTYPE),' file for Exp#',NEXP,
     .          'file #',NRUN,' does not exist.'
        NRET = -3
        RETURN
      ENDIF
   
      IP   = 3 + 3*(NRUN-1)
      WRITE( VOLSER, '(A4,A2)') NRNLST(IP), NRNLST(IP+1)
      LVOLSR = 6
      LPOS = NRNLST(IP+2)
      WRITE(DSN,510) NRUN, WTYPE(:LTYPE), NEXP
510   FORMAT('T@WK.G',I3.3,'.T@WK.',A,'.E',I5.5)
      IF( WTYPE(1:8).EQ.'WORKFILE') THEN
        WRITE(DSN,520) NRUN, NEXP
520     FORMAT('T@WK.G',I3.3,'.T@WK.TOPAZ02.E',I5.5)
      ENDIF
   
C  
C-----------------------------------------------------------------------
C (8) Allocate Files.
C-----------------------------------------------------------------------
C  
800   CONTINUE
   
      LDSN = INDEX(DSN,' ') - 1
C     PRINT *,' '
C     PRINT *,' Calling UCTLAL..NUNIT=',NUNIT
C     PRINT *,' DSN=',DSN(:LDSN),'=='
C     PRINT *,' VOLSER=',VOLSER(1:LVOLSR),'=='
C     PRINT *,' LPOS  =',LPOS
      IF( LPOS .LT. 0 ) THEN
        IF( IOPT .EQ. 0 ) THEN
          PRINT *,'%Error UCMTAL..Tape ',VOLSER(1:LVOLSR),' for ',
     >            DSN(:LDSN),' is not accesible since ',
     >            'it was moved to the CTL stock.'
        ENDIF
        NRET = -1
        RETURN
      ELSEIF( LPOS .EQ. 0 ) THEN
        IF( IOPT .EQ. 0 ) THEN
          PRINT *,'%Error UCMTAL..Tape ',VOLSER(1:LVOLSR),' for ',
     >            DSN(:LDSN),' is not available. It was scratched.'
        ENDIF
        NRET = -4
        RETURN
      ENDIF
C  
      CALL UCTLAL( NUNIT, DSN(:LDSN), VOLSER(1:LVOLSR),
     >             LPOS, OPTION, IRET )
C  
      IF( IRET .LT. 0 ) NRET = -999
C  
      RETURN
      END
