C**********************************************************************
C*
C* ---------------
C* Common /PRDYNA/
C* ---------------
C*
C*   Common to control dynamic allocation of input file.
C*   Extended to allow dynamic allocation of UMT and disk files.
C*                  22-Sep-1989
C*   Extended for use on unix system  13-July-1994 A. Miyamoto
C*
C**********************************************************************
C*
      PARAMETER (MXxDYN=50)
      COMMON /PRDYNA/ NDYNAT, NOWEXP, NOWRUN, NXTFIL, NOWPNT,
     .                NUMDYN, NRNDYN(3, MXxDYN),
     .                NFINFO(4, MXxDYN), NOFALL, NOFINF(5,MXxDYN)
      CHARACTER       OPTDYN*80, TYPDYN(MXxDYN)*8
      CHARACTER       FILDYN(MXxDYN)*200
      CHARACTER       FILDEV(MXxDYN)*32,FILVOL(MXxDYN)*64
      CHARACTER       OUTFIL(MXxDYN)*200
     .               , OUTDEV(MXxDYN)*32, OUTVOL(MXxDYN)*64
 
      COMMON /PRDYNC/ OPTDYN, TYPDYN
      COMMON /PRDYNN/ FILDYN, FILDEV, FILVOL,
     .                OUTFIL, OUTDEV, OUTVOL
C*
C*
C*    NDYNAT =-1 to use static allocation. ( reset in PRCRED )
C*           = 0 to use FORMAT tape.
C*           = 1 to use DST001 tape.
C*           = 2 to use DST002 tape.
C*    NOWEXP : Current experiment # being in allocation.
C*            If NOWEXP=0, allocate file specified by FILDYN instead
C*            of Standard TOPAZ tape.
C*    NOWRUN : Current run # being in allocation.
C*    NXTFIL : Next file # to be allocated.
C*    NOWPNT : Pointer to current run list in NRNDYN
C*    NUMDYN : # of dynamic allocation file group.
C*    NRNDYN(1, i) : Experiment # of dynamic allocation group.
C*          (2, i) : Start run # of dyn. alloc group.
C*          (3, i) : Last run # of dyn, alloc group
C*         Exp# = 0 to allocate UMT or disk file.
C*    OPTDYN : Option string for UCMTAL.
C*    TYPDYN(i) : Type of dynamic allocation files.
C*    FILDYN(i) : Data set name for the dynamic allocation of UMT or
C*                disk files.
C*
C*    NFINFO(1,i) : Input file type
C*                = 0 for standard TBS data.
C*                = 1 for VBS data transfered from FACOM without data
C*                        conversion.
C*                = 2 for file on FACOM to be accessed through
C*                        RMTL library.
C*                = 3 for Data on SONY DIR-1000 tape.
C*                = 4 for crated by fortran READ/WRITE routine.
C*          (2,i) : File position on the tape, when tape device
C*                  is selected.
C*          (3,i) : = 1 to Mount tape prior to open file, otherwise no
C*                   action.
C*          (4,i) : = 1 to Dismount tape when end-of-file is detected.
C*    FILDEV(i)   : Device name for the tape file.
C*    FILVOL(i)   : Tape volume name.
C*
C*    OUTFIL(i)      : Output file name.
C*    OUTDEV(i)      : Output device name for the tape file.
C*    OUTVOL(i)      : Output tape volume name.
C*    NOFINF(1,i)   : Output file type ( not in use.)
C*          (2,i)   : File position of the tape, when the tape is
C*                  selected.
C*          (3,i)   : =1 to mount tape prior to open file.
C*          (4,i)   : =1 to dismount tape when the end-of-file is
C*          (5,i)   : Tape size in a unit of kbytes.
C*    NOFALL        : Total number of output tape file.
C*
