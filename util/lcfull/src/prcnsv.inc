C*********************************************************************
C*
C*  How to get version data flag.
C*
C*  Used by PRGCNS
C*
C*********************************************************************
C
#ifdef MSP
      COMMON /PRCNSV/ NPRTCN, NCNVRS(43)
#endif
#ifdef UNIX
      COMMON /PRCNSV/ NPRTCN, NCNVRS(43)
      CHARACTER*128   cnfiles(43)
      common /prcnsvc/ cnfiles
C=    data cnfiles/43*' '/
#endif
C  cnfiles(i):file names for non-default constants file.
C
C=    DATA NPRTCN/2/
C
C     Message level of subroutine PRGCNS
C
C       = 0  : Print nothing except fatal error.
C       = 1  : Print warning message.
C       = 2  : Print version #, and exp#, run #, only when new constants
C             are read in.
C       = 3  : Print header information, too.
C       = 4  : Print version #, exp#, run # of all run data.
C       = 5  : Print header info of all run.
C
C=    DATA NCNVRS/43*0/
C
C     NCNVRS(i) = 0  ; get version # from runlist data base.
C               > 0  ; returns # saved in NCNVRS
C               < 0  ; Indicates that to get them from external file.
C                      file is rewind after read.
C
C     NCNVRS( 1) = Reduction IDC      NCNVRS(11) =  Production IDC.
C           ( 2) =           TPC            (12) =             TPC
C           ( 3) =           BTOF           (13) =             BTOF
C           ( 4) =           BDC            (14) =             BDC
C           ( 5) =           BCL            (15) =             BCL
C           ( 6) =           BMU            (16) =             BMU
C           ( 7) =           EDC            (17) =             EDC
C           ( 8) =           ECL            (18) =             ECL
C           ( 9) =           LUM            (19) =             LUM
C           (10) =           Filter         (20) =             Steering
C           (21) =           VTX            (25) =             VTX
C           (22) =           TCH            (26) =             TCH
C           (23) =           FCL            (27) =             FCL
C           (24) =           RCL            (28) =             RCL
C
C           (29) = Simulator Steering.      (30) = Trigger simulator
C           (31) =           IDC
C           (32) =           TPC
C           (33) =           BTOF
C           (34) =           BDC
C           (35) =           BCL
C           (36) =           BMU
C           (37) =           EDC
C           (38) =           ECL
C           (39) =           LUM
C           (40) =           VTX
C           (41) =           TCH
C           (42) =           FCL
C           (43) =           RCL
C
C     Address of these data will be obtained by
C          Pointer - 40
C     in the member @PRCVPT
C
