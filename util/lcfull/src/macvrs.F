C**********************************************************************
C*
C* --------------------------------------=====
C* Subroutine MACVRS(NEXP, NRUN, RECNAM, NVERS)
C* --------------------------------------=====
C*
C*(Function)
C*   Get version number of the calibration constants.
C*
C*(Input)
C*   NEXP   ; Experiment #.
C*   NRUN   ; Run #.
C*   RECNAM ; Record name of the calibration constants.
C*
C*(Output)
C*   NVERS  ; Version number of the constants.
C*        > 0 normal return
C*        = 0 When version data is not available.
C*        =-1 When contstants is not available.
C*
C*(Author)
C*
C*  A. Miyamoto.  30-Apr-1987   Original version.
C*                22-May-1987   Get version information from data base.
C*
C**********************************************************************
C*
      SUBROUTINE MACVRS(NEXP, NRUN, RECNAM, NVERS )
C
C
      CHARACTER*(*)   RECNAM
      CHARACTER*64    WRKNAM
C
      PARAMETER      (MXxBUF=100)
      INTEGER*4       LRUNLS(MXxBUF)
C
      PARAMETER      (MXxCNS=20)
      CHARACTER*44    CNSNAM(MXxCNS)
      INTEGER*4       NVDATA(MXxCNS), NVPNT(MXxCNS)
      DATA  (NVDATA(I), NVPNT(I), CNSNAM(I),I=1,10)/
     1  1, 41, 'REDUCTION:IDC_CONSTANTS',
     2  2, 42, 'REDUCTION:TPC_CONSTANTS',
     3  2, 43, 'REDUCTION:BTOF_CONSTANTS',
     4  1, 44, 'REDUCTION:BDC_CONSTANTS',
     5  1, 45, 'REDUCTION:BCL_CONSTANTS',
     6  1, 46, 'REDUCTION:BMU_CONSTANTS',
     7  1, 47, 'REDUCTION:EDC_CONSTANTS',
     8  1, 48, 'REDUCTION:ECL_CONSTANTS',
     9  1, 49, 'REDUCTION:LUM_CONSTANTS',
     X  1, 50, 'REDUCTION:FILTER_CONSTANTS'  /
 
      DATA  (NVDATA(I), NVPNT(I), CNSNAM(I),I=11, MXxCNS)/
     1  2, 51, 'PRODUCTION:IDC_CONSTANTS',
     2  1, 52, 'PRODUCTION:TPC_CONSTANTS',
     3  1, 53, 'PRODUCTION:BTOF_CONSTANTS',
     4  1, 54, 'PRODUCTION:BDC_CONSTANTS',
     5  1, 55, 'PRODUCTION:BCL_CONSTANTS',
     6  1, 56, 'PRODUCTION:BMU_CONSTANTS',
     7  1, 57, 'PRODUCTION:EDC_CONSTANTS',
     8  1, 58, 'PRODUCTION:ECL_CONSTANTS',
     9  1, 59, 'PRODUCTION:LUM_CONSTANTS',
     X  1, 60, 'PRODUCTION:STEERING_CONSTANTS'/
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
        LRUNLS(3) = -1
        LRUNLS(4) = -1
        IFIRST = 0
      ENDIF
C
C -------------------------------------------------------------------
C (2) If Run # or Exp# is different, get RUNLIST data from data base.
C -------------------------------------------------------------------
C
      IF( NEXP .NE. LRUNLS(3) .OR. NRUN .NE. LRUNLS(4)) THEN
         CALL MAOPEN( IRET )
         IF( IRET .LT. 0 ) THEN
           LREC  = LEN(RECNAM)
           PRINT *,'%Error MACVRS .. Failed to open RUNLIST data base'
           PRINT *,' Requested to get Exp#', NEXP, ' Run#', NRUN
           PRINT *,' of data ', RECNAM(:LREC)
           STOP
         ENDIF
 
         CALL MAGETR(0, NEXP, NRUN, MXxBUF, NW, LRUNLS )
 
         CALL MACLOS
 
       ENDIF
C
C -------------------------------------------------------------------
C (3) Search registered record name.
C -------------------------------------------------------------------
C
      WRKNAM = RECNAM
      CALL UCUPER(64, WRKNAM)
C
      NVERS = -1
      DO 300 I = 1, MXxCNS
        IF( WRKNAM .EQ. CNSNAM(I)(:44) ) GO TO 400
300   CONTINUE
C
      RETURN
C
C -------------------------------------------------------------------
C (4) return pre-determined version #, if NW < 40
C -------------------------------------------------------------------
C
400   CONTINUE
      IF( NW .LT. 60 ) THEN
        NVERS = NVDATA(I)
C
C ... For IDC Mag off data.
        IF( I .EQ. 1 .AND. (NRUN.GE.737 .AND. NRUN.LE.1040) ) NVERS = 2
        IF( I .EQ.10 .AND. (NRUN.GE.737 .AND. NRUN.LE.1040) ) NVERS = 3
C
C -------------------------------------------------------------------
C (5) Return version # in RUNLIST data base, if NW > 60
C -------------------------------------------------------------------
C
      ELSE
 
        NVERS = LRUNLS(NVPNT(I))
 
      ENDIF
C
      RETURN
      END
