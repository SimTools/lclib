CC*********************************************************************C
C*                                                                     *
C*-------------------======                                            *
C* Subroutine PRGEVT(NRET)                                             *
C*-------------------======                                            *
C*                                                                     *
C*(Purpose)                                                            *
C*    TBREAD data                                                      *
C*(Input)                                                              *
C*   NRET = -9998 
C*    Close and dis-mount tape, if any is open.
C*                                                                     *
C*(Output)                                                             *
C*   IRET   : Return flag.                                             *
C*        > 0   TBREAD suceed.  IRET = Record ID.                      *
C*        = -1  TBREAD error.                                          *
C*        = -2  TBINIT error.                                          *
C*        =-1000 Read End_of_File of input file.                       *
C*        =-1001 Complete event Generation.                            *
C*                                                                     *
C*(Relation)                                                           *
C*    Called by PRCEVT.                                                *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto   27-Apr-1987   Original version.                    *
C*                  29-Sep-1987   Add dynamic allocation option.       *
C*                  24-AUG-1988   Support many types of files for DYNA *
C*                   5-Nov-1988   According to NRET change in UCMTAL   *
C*                   8-Sep-1989   To FULLnize simulator.               *
C*                  19-Apr-1995   UNIX versionCC
C*                  16-Jan-2003   Removed obsolute functions           *
C*                                                                     *
**********************************************************************
 
      SUBROUTINE PRGEVT( NRET )
 
#include "prfctl.inc"
#include "prgenf.inc"
#include "prdyna.inc"
#include "prjctl.inc"
#include "tbsfmt.inc"
C
      EXTERNAL TBVOPN
      INTEGER*4  NOPNFL
      DATA       NOPNFL/0/
      SAVE       NOPNFL
      DATA  NODFIL/ 0 /
      CHARACTER*1 NULL/X'00'/
 
C===========================<< Entry point >>==========================
#ifdef MSP
#error "Obsolute MSP switch has been disabled since Jan-2003"
#error "Get older version package if you really need"
#endif
C
C ---------------------------------------------------------------------
C (1) Allocate input file, if NOWPNT .LE. 0, though NDYNAT >= 0
C ---------------------------------------------------------------------
C
      IF( NDYNAT .LE. -1 ) GO TO 200
C
      IF( NOWPNT .EQ. 0 ) THEN
        NOWPNT = 1
        IF( NOWPNT .GT. NUMDYN ) GO TO 900
        NOWEXP = NRNDYN(1, NOWPNT)
        NOWRUN = NRNDYN(2, NOWPNT)
        NXTFIL = 1
        GO TO 500
      ENDIF
C
C ---------------------------------------------------------------------
C (2) TBREAD data, or TBINIT
C ---------------------------------------------------------------------
C
200   CONTINUE
      IF( NUMGEN .LE. 0 ) THEN
        CALL TBREAD( NINUNI, NRET )
        IF( NRET .NE. -1000 ) RETURN
C
C ... For Event generation mode. ( 8-Sep-1989 )
C
      ELSE
        IUMGEN = IUMGEN + 1
        IF( IUMGEN .GT. NUMGEN + 2 ) THEN
            NRET = -1001
            RETURN
        ENDIF
        NRET   = 1
        IF( IUMGEN .EQ. 1 )          NRET = 2
        IF( IUMGEN .EQ. NUMGEN + 2 ) NRET = 8
        CALL TBINIT( NRET, IRET )
        IF( IRET .LT. 0 )            NRET = -2
        RETURN
      ENDIF
C
C ---------------------------------------------------------------------
C (3) Just read next file, in case of static allocation.
C ---------------------------------------------------------------------
C
      IF( NDYNAT .LE. -1 ) THEN
        IINFIL = IINFIL + 1
        IF( IINFIL .LE. NINFIL ) GO TO 200
        GO TO 900
      ENDIF

C
C ---------------------------------------------------------------------
C (4) Get next run # from the stack.
C ---------------------------------------------------------------------
C
400   CONTINUE
      IF( NXTFIL .GT. 0 ) GO TO 500
      NXTFIL = 1
      NOWRUN = NOWRUN + 1
      IF( NOWRUN .GT. NRNDYN(3, NOWPNT)) THEN
        NOWPNT = NOWPNT + 1
        IF( NOWPNT .GT. NUMDYN ) THEN
          GO TO 900
        ENDIF
        NOWEXP = NRNDYN(1, NOWPNT )
        NOWRUN = NRNDYN(2, NOWPNT )
      ENDIF
C
C ---------------------------------------------------------------------
C (5) Dynamically allocate new file.
C ---------------------------------------------------------------------
C
500   CONTINUE
      IF( NOWEXP .EQ. 0 ) THEN
        IF(NFINFO(1,NOWPNT).EQ.0 ) THEN
           OPEN(NINUNI,FILE=FILDYN(NOWPNT),
     >     FORM='UNFORMATTED',IOSTAT=IRET)
          NRWFMT(1) = 0

        ELSEIF(NFINFO(1,NOWPNT).EQ.4 ) THEN
           OPEN(NINUNI,FILE=FILDYN(NOWPNT),
     >     FORM='UNFORMATTED',IOSTAT=IRET)
          NRWFMT(1) = 4
C
C ==================================================
C Open VBS format tape.
C ==================================================
C
        ELSEIF( NFINFO(1,NOWPNT).EQ.1 ) THEN
          NOPNFL= NOPNFL + 1
          IF( NOPNFL .EQ. 1 ) CALL TBUXIN(3,0)
          NRWFMT(1) = 1
      CALL TBVOPN(FILDYN(NOWPNT)(1:LNBLNK(FILDYN(NOWPNT)))//NULL)

        ELSE
            PRINT *,'In PRGEVT .. File type ',NFINFO(1,NOWPNT),
     >        ' is not supported yet.'
        ENDIF

 
        IF( IRET .LT. 0 ) THEN
          WRITE(6,520) FILDYN(NOWPNT)
        ELSE
          NXTFIL = 0
          GO TO 200
        ENDIF
C
C
      ELSE
520   FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to the error to allocate file.  *',/,
     >' *   File name is =',A,/,
     >' ************************************************************',/,
     >   //)

      ENDIF
C
C ---------------------------------------------------------------------
C (9) Return to caller as end of input file.
C ---------------------------------------------------------------------
C
900   CONTINUE
C
      NRET = -1000
	print *,'Reach end-of-file of input file.'

      RETURN

800     continue
        NRET = -1000
        RETURN
C
C .. Emergency stop.
C
2000    CONTINUE
        return

      END


