C***********************************************************************
C*
C*  --------------------------======
C*  Subroutine PRGCNS( DETECT, NRET )
C*  --------------------------======
C*
C*(Function)
C*   Readin constants.  All actions are controled by the data in common
C*   /PRCNSV/
C*
C*(Input)
C*   DETECT  : CHaracter string of constants name.
C*
C*         RDIDC = Reduction IDC         PRIDC   =  Production IDC.
C*         RDTPC =           TPC         PRTPC   =             TPC
C*         RDBTOF=           BTOF        PRBTOF  =             BTOF
C*         RDBDC =           BDC         PRBDC   =             BDC
C*         RDBCL =           BCL         PRBCL   =             BCL
C*         RDBMU =           BMU         PRBMU   =             BMU
C*         RDEDC =           EDC         PREDC   =             EDC
C*         RDECL =           ECL         PRECL   =             ECL
C*         RDLUM =           LUM         PRLUM   =             LUM
C*         RDFLT =           Filter      PRSTR   =             Steering
C*         RDVTX =           VTX         PRVTX   =             VTX
C*         RDTCH =           TCH         PRTCH   =             TCH
C*         RDFCL =           FCL         PRFCL   =             FCL
C*         RDRCL =           RCL         PRRCL   =             RCL
C*         SMSTR = Simulator Steering    SMTRG   = Trigger Simulator
C*         SMIDC =           IDC         SMTPC   = Simulator   TPC
C*         SMBTOF=           BTOF        SMBDC   =             BDC
C*         SMBCL =           BCL         SMBMU   =             BMU
C*         SMEDC =           EDC         SMECL   =             ECL
C*         SMLUM =           LUM         SMVTX   =             VTX
C*         SMTCH =           TCH         SMFCL   =             FCL
C*         SMRCL =           RCL
C*
C*
C*
C*(Output)
C*   NRET    : Return code.
C*         > 0 : Constants obtained sucessfully from data base.
C*              value equal to the constant version #.
C*               or read from logical unit # eqaul to NRET.
C*         = 0 : In case of simulator data.
C*   -1 to -10 : In case of simulator data, absolute value indicates
C*              the version # used for simulator data.
C*      = -999 : Use same version with the previous run.
C*
C*       Program will stop in case of following case.
C*          (1) Record is not defined.
C*          (2) Run # is not exist in the data base.
C*          (3) constant of requested version # does not exist.
C*          (4) Version data is undefined.
C*          (5) Record is not defined in the constant data base.
C*
C*
C*(Author)
C*  A. Miyamoto  5-Oct-1987  Original version.
C*               3-Mar-1988  Read Simulators constant version from
C*                           data base.
C*  A. Miyamoto  9-Sep-1989  Add upgraded detector.
C*
C***********************************************************************
C
      SUBROUTINE PRGCNS( DETECT, NRET )
C
      CHARACTER*(*) DETECT
C
#include "prcnsv.inc"
#include "prcvpt.inc"
#include "prhead.inc"
#include "prdtyp.inc"
#ifdef UNIX
#include "tbsfmt.inc"
#endif
C
      PARAMETER  (NOxDTC = 43 )
      CHARACTER*8   DTCNAM(NOxDTC), WRKNAM
 
      CHARACTER*400 DTCLST
      EQUIVALENCE  (DTCLST(1:1), DTCNAM(1)(1:1))
      DATA          DTCNAM
     . /'RDIDC ', 'RDTPC ', 'RDBTOF', 'RDBDC ','RDBCL ',
     .  'RDBMU ', 'RDEDC ', 'RDECL ', 'RDLUM ','RDFLT ',
     .  'PRIDC ', 'PRTPC ', 'PRBTOF', 'PRBDC ','PRBCL ',
     .  'PRBMU ', 'PREDC ', 'PRECL ', 'PRLUM ','PRSTR ',
     .  'RDVTX ', 'RDTCH ', 'RDFCL ', 'RDRCL ','PRVTX ',
     .  'PRTCH ', 'PRFCL ', 'PRRCL ', 'SMSTR', 'SMTRG',
     .  'SMIDC ', 'SMTPC ', 'SMBTOF', 'SMBDC ','SMBCL ',
     .  'SMBMU ', 'SMEDC ', 'SMECL ', 'SMLUM ','SMVTX ',
     .  'SMTCH ', 'SMFCL ', 'SMRCL '/
      INTEGER*4    IDREC(NOxDTC)
      DATA   	   IDREC
     . / 31, 32, 33, 34, 35,   36, 38, 39,  40, 37,
     .   41, 42, 43, 44, 45,   46, 48, 49,  50, 47,
     .   51, 53, 55, 57, 52,   54, 56, 58,  59, 60,
     .   61, 62, 63, 64, 65,   66, 67, 68,  69, 70,
     .   71, 72, 73 /
C
      CHARACTER    BNKNAM*24, CDATE*8, CTIME*8, FILNAM*44
      INTEGER*4    IWORK(NOxDTC)
C
      INTEGER*4    NDFVRS(NOxDTC)
     . / 1, 2, 2, 1, 1,   1, 1, 1, 1, 1,
     .   2, 1, 1, 1, 1,   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,   1, 1, 1, 1, 1,
     .   1, 1, 1/
C
      INTEGER*4    NOLVRS(NOxDTC)/NOxDTC*-1/
C
CTAYM881201       PARAMETER      (MXxBUF=100)
CTAYM881201       INTEGER*4       LRUNLS(MXxBUF)
#include "prunls.inc"
C
      PARAMETER (MXxBUF=1000)
      COMMON /SSWORK/ IRNLBF( MXxBUF )

C      CHARACTER       KEY*20
C
      DATA  IFIRST/1/
C
C============< Entry Point >=========================================
C
C -------------------------------------------------------------------
C (1) Initilization, set defaul run #.
C -------------------------------------------------------------------
C
      IF( IFIRST .EQ. 1 ) THEN
C       LRUNLS(3) = -989
C       LRUNLS(4) = -989
        IFIRST    = 0
        IPRT      = NPRTCN
        IF( IPRT .EQ. 2 .OR. IPRT .EQ. 3 ) IPRT = IPRT + 2
      ENDIF
C
C -------------------------------------------------------------------
C (2) Get pointer for the constants name
C -------------------------------------------------------------------
C
      LNAME   = LEN(DETECT)
      WRKNAM  = DETECT
      CALL UCUPER( 8, WRKNAM )
      IPNT    = INDEX( DTCLST, WRKNAM )
      IF( IPNT .LE. 0 ) THEN
        PRINT *,'%Error PRGCNS .. Invalid constant name ',
     .           DETECT(:LNAME),' was specified.'
        GO TO 990
      ENDIF
      IDTC    = ( IPNT - 1 ) / 8 + 1
C
C -------------------------------------------------------------------
C (3) Swich according to the NCNVRS
C -------------------------------------------------------------------
C
      IVRS = NCNVRS( IDTC )
      IF( IVRS ) 800, 310, 500
C
C -------------------------------------------------------------------
C (3) Automatic constant version determination.
C     If simulator data, skip to 400
C     If Run # or Exp# is different, get RUNLIST data from data base.
C -------------------------------------------------------------------
C
310   CONTINUE
C
C (3.1) Request constants for the simulator.
C
      IF( IDTC .GT. 28 ) THEN
        CALL MAOPEN( IRET )
        IF( IRET .LT. 0 ) THEN
          PRINT *,'%Error PRGCNS..Failed to open RUNLIST data base.'
          PRINT *,' when tried to read ',DETECT(:LNAME),
     .           ' constant for exp#', xEXPNO,' run# ', xRUNNO
          GO TO 990
        ENDIF
        IEXP = xEXPNO
        IF( IEXP .LE. 0 ) IEXP = 4
        CALL MAGETE('SIMCONS', IEXP, MXxBUF, IWRNL, IRNLBF )
        IF( IWRNL .LE. 0 ) THEN
          PRINT *,'%Error PRGCNS..Simulator constants for ',
     .            ' Exp#',IEXP,' does not exists in RUNLIST ',
     .            ' data base.'
          GO TO 990
        ENDIF
        CALL MACLOS
C
        IVRS = IRNLBF(IDTC-22)
        IF( IVRS .LE. 0 ) THEN
          IF( IPRT .GE. 5 ) PRINT *,
     .            'PRGCNS..This is MC data. Use default ',
     .            DETECT(:LNAME),' constants in block data.'
          NRET = 0
          IVRS = 0
          GO TO 900
        ELSE
          GO TO 500
        ENDIF
C
C (3.2) Simulator data, just return, except PRIDC constants.
C
      ELSEIF( NDTYPE(1) .NE. 1 ) THEN
        CALL MAOPEN( IRET )
        IF( IRET .LT. 0 ) THEN
          PRINT *,'%Error PRGCNS..Failed to open RUNLIST data base.'
          PRINT *,' when tried to read ',DETECT(:LNAME),
     .           ' constant for exp#', xEXPNO,' run# ', xRUNNO
          GO TO 990
        ENDIF
        IEXP = xEXPNO
        IF( IEXP .LE. 0 ) IEXP = 4
        CALL MAGETE('PRODSIM', IEXP, MXxBUF, IWRNL, IRNLBF )
        IF( IWRNL .LE. 0 ) THEN
          PRINT *,'%Error PRGCNS..Production constants for simulator ',
     .            ' data of Exp#',IEXP,' does not exists in RUNLIST ',
     .            ' data base.'
          GO TO 990
        ENDIF
        CALL MACLOS
C
        IVRS = IRNLBF(IDTC-4)
        IF( IDTC .GT. 20 ) IVRS = IRNLBF(IDTC-8)
        IF( IVRS .LE. 0 ) THEN
          IF( IPRT .GE. 5 ) PRINT *,
     .            'PRGCNS..This is MC data. Use default ',
     .            DETECT(:LNAME),' constants in block data.'
          NRET = 0
          IVRS = 0
          GO TO 900
        ELSE
          GO TO 500
        ENDIF
C
C (3.3) Real data, get version data from RUNLIST data base.
C
      ELSEIF( xEXPNO .NE. LRUNLS(3) .OR. xRUNNO .NE. LRUNLS(4)) THEN
        CALL PRLRNL( xEXPNO, xRUNNO, IRET )
        IF( NWRNL .LT. 0 .OR. IRET .LT. 0 ) THEN
          PRINT *,'%Error PRGCNS..Failed to open RUNLIST data base.'
          PRINT *,' when tried to read ',DETECT(:LNAME),
     .             ' constant for exp#', xEXPNO,' run# ', xRUNNO
          GO TO 990
        ENDIF
      ENDIF
C
C ----------------------------------------------------------------------
C (4) Determine version # of constant. ( For real data.)
C     If NW < 60, set default version #.
C     Else obtained it from RUNLIST data base.
C ----------------------------------------------------------------------
C
      IF( NWRNL .LT. 60 ) THEN
         IVRS = NDFVRS( IDTC )
C
C ... For IDC Mag off data.
        IF( xEXPNO .EQ. 2 ) THEN
          IF( xRUNNO .GE. 737 .AND. xRUNNO .LE. 1040 ) THEN
            IF( IDTC .EQ.  1 ) IVRS = 2
            IF( IDTC .EQ. 11 ) IVRS = 3
          ENDIF
        ENDIF
        IF( IPRT .GE. 1 ) PRINT *,
     .      '%Warning..PRGCNS  version# data for ',DETECT(:LNAME),
     .      ' constants of Exp# ',xEXPNO,' Run# ',xRUNNO,
     .      ' not available.  Use default version #',IVRS
      ELSE
        IVRS  = LRUNLS( 40 + IDTC )
      ENDIF
C
C ----------------------------------------------------------------------
C (5) If version # changes, get new constants from data base,
C ----------------------------------------------------------------------
C
500   CONTINUE
C
      IF( IVRS .EQ. NOLVRS( IDTC ) ) THEN
        IF( IPRT .GE. 5 ) PRINT *,
     .    'PRGCNS .. Use same ',DETECT(:LNAME),' constants with ',
     .    ' the previous run.'
        NRET  = -999
        GO TO 900
      ENDIF
C
C .. Get constant from data base.
C
      IDR   = IDREC( IDTC )
      CALL TBCRED(IDR  ,IVRS,IRT)
      IF( IRT .GE. 0 ) GO TO 520
C
C .. In case of fatal error.
C
      IF( IRT .EQ. -1 .OR. IRT. LT. -3 ) THEN
        PRINT *,'%Error PRGCNS .. record for ',DETECT(:LNAME),
     .          ' is not allocated yet.'
        GO TO 990
      ELSEIF( IRT .EQ. -2 ) THEN
        PRINT *,'%Error PRGCNS .. Version#',IVRS,
     .          ' of ',DETECT(:LNAME),' does not exists.'
        GO TO 990
      ELSEIF( IRT .EQ. -3 ) THEN
        PRINT *,'%Error PRGCNS .. ',DETECT(:LNAME),
     .          ' constant does not installed to the data base yet.'
        GO TO 990
      ENDIF
C
C (5.2) Print information of new constants.
C
520   CONTINUE
      NRET           = IRT
      NOLVRS( IDTC ) = IRT
      IF( IPRT .GE. 2 ) THEN
        IF( NDTYPE(1) .NE. 1 ) THEN
 
          PRINT *,
     .        'PRGCNS..This is MC data. Use ',
     .        DETECT(:LNAME),' constants of version #',IRT,
     .        ' for Exp#',IEXP
        ELSE
          WRITE(6,524) DETECT(:LNAME), IRT, xEXPNO, xRUNNO
524       FORMAT(' TBCREDs ',A6,' constant, Vers#',I4,' for Exp#', I4,
     .           ' Run#', I5 )
        ENDIF
      ENDIF
      IVRS = IRT
      GO TO 900
C
C ----------------------------------------------------------------------
C (8) Get constants from the external file.
C ----------------------------------------------------------------------
C
800   CONTINUE
#ifdef MSP
      IUNIT = IABS( IVRS )
      IF( IUNIT .LT. 100 ) THEN
        CALL TBREAD(IUNIT, IDR   )
        IF( IDR   .NE. IDREC( IDTC ) ) THEN
          PRINT *,'%Error PRGCNS .. TBREADed ',DETECT(:LNAME),
     .            ' constant from unit#', IUNIT,
     .            ' was invalid record ID ',IDR,'.'
          GO TO 990
        ENDIF
C
        IF( IPRT .GE. 2 )
     .    WRITE(6,814) DETECT(:LNAME), IUNIT, xEXPNO, xRUNNO
814       FORMAT(' TBREADs ',A6,' constant from unit#',I2,
     .           ' for Exp#',I4,' Run#', I5 )
        REWIND IUNIT
        NRET = IUNIT
        NCNVRS( IDTC ) = NCNVRS( IDTC ) - 100
        IVRS = NCNVRS(IDTC)
      ELSE
        NRET = -999
      ENDIF
#endif
#ifdef UNIX
      IUNIT = IABS( IVRS )
      IF( IUNIT .LT. 100 ) THEN
        open(iunit,file=cnfiles(idtc),form='Unformatted',
     .     iostat=ios)
        if(ios.ne.0) then
	  print *,'%Error PRGCNS .. Unable to open file',
     .          cnfiles(idtc)(:lnblnk(cnfiles(idtc))),
     .          ' for ',DETECT(:LNAME),' constant.'
          go to 990
        endif
        nsavfmt=nrwfmt(1)
        nrwfmt(1) = 0
        call tbread(iunit, idr)
        nrwfmt(1) = nsavfmt
        IF( IDR   .NE. IDREC( IDTC ) ) THEN
          PRINT *,'%Error PRGCNS .. TBREADed ',DETECT(:LNAME),
     .   ' constant from ',cnfiles(idtc)(:lnblnk(cnfiles(idtc))),
     .   ' was invalid record ID ',IDR,'.'
          GO TO 990
        ENDIF
        IF( IPRT .GE. 2 )
     .    WRITE(6,814) DETECT(:LNAME), 
     .             cnfiles(idtc)(:lnblnk(cnfiles(idtc))),
     .             xEXPNO, xRUNNO
814       FORMAT(' TBREADs ',A6,' constant from a file',a,
     .           ' for Exp#',I4,' Run#', I5 )
        close(iunit)
        NRET = IUNIT
        NCNVRS( IDTC ) = NCNVRS( IDTC ) - 100
        IVRS = NCNVRS(IDTC)
      ELSE
        NRET = -999
      ENDIF
#endif
C
C ----------------------------------------------------------------------
C (9) Return to caller.
C ----------------------------------------------------------------------
C
900   CONTINUE
      IF( ( IPRT .EQ. 3 .AND. NRET .NE. -999 ) .OR. IPRT .GE. 5 ) THEN
         BNKNAM = 'CALIBRATION:'//DETECT(3:LNAME)//';HEADER'
         CALL TBGET(IDR ,BNKNAM, 1, NW, IWORK, IRET)
         IF( IRET .LT. 0 ) THEN
           PRINT *,'               Header bank for ',DETECT(:LNAME),
     .             ' not available.'
         ELSE
           CALL UCDATE( IWORK(1), CDATE )
           CALL UCTIME( IWORK(2), CTIME )
           LFL  = MAX(INDEX( FILNAM, ' ' ) - 1, 1)
           WRITE(FILNAM,'(11A4)') (IWORK(K),K=5,15)
           PRINT *,'               Created on ',CDATE,' ',CTIME,
     .             ' for run from Exp# ', IWORK(3),
     .             ' Run# ', IWORK(4),
     .             ' prepared in ',FILNAM(:LFL)
          ENDIF
      ENDIF
      IPRT      = NPRTCN
      CALL PRWCNS(IDTC, IVRS)
      RETURN
C
C ----------------------------------------------------------------------
C (9.9) Fatal error, call ERRTRA, then stop.
C ----------------------------------------------------------------------
C
990   CONTINUE
      CALL MACLOS
      CALL ERRTRA
      STOP
      END
