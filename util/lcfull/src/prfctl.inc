C*********************************************************************
C*======================                                             *
C*  COMMON /PRFCTL/                                                  *
C*======================                                             *
C*                                                                   *
C*(1) Purpose                                                        *
C*                                                                   *
C*  Control area of the production/reduction job file.               *
C*                                                                   *
C*********************************************************************
C
C --------------------------------------------------------------------
C
      COMMON /PRFCTL/ NINUNI, NINFIL, IINFIL,
     >                NOTUNI, NOTFIL, IOTFIL, NOTDEV, XOSIZE, ZOSIZE
C
C*    NINUNI  ; Input logical unit #.
C*    NINFIL  ; # of input file.
C*    IINFIL  ; Current input file #.
C=    DATA    NINUNI/10/
C=    DATA    NINFIL/ 1/
C=    DATA    IINFIL/ 1/
C
C*    NOTUNI  ; Output logical unit #.
C*    NOTFIL  ; # of output file.
C*    IOTFIL  ; Current output file #.
C*    NOTDEV  ; Output device type.(0=Disk, 1=CTL)
C*    XOSIZE  ; Output data size max.(Trk for Disk, Feet for Tape.)
C*    ZOSIZE  ; Current output size.
C*
C=    DATA    NOTUNI/21/
C=    DATA    NOTFIL/ 1/
C=    DATA    IOTFIL/ 1/
C=    DATA    NOTDEV/ 1/
C=    DATA    XOSIZE/ -1./
C=    DATA    ZOSIZE/  0./
C
C
C ----------------------------------------------------------------------
C (1) How these data are used to control job flow.
C ----------------------------------------------------------------------
C   1. Input.
C
C      IINUNI is incremented, when End-of-File is detected.
C                If IINFIL GT NINFIL, job end as end-of-file.
C      To analize multi-run,
C        (1) Static Allocation case.
C            Set NINFIL > 1, and prepare dd statement.
C        (2) Dynamic allocation.
C            CALL PRNEXT(IEXP, IRUN) in the Run termination.  This
C            routine will allocate new file, and increment NINFIL.
C
C   2. Output.
C      When ZOSIZE GE XOSIZE, consider  it as a end-of-file of output
C      file.  IOTFIL is incremented, and if IOTFIL GT NOTFIL, job
C      will end.
C
