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
C*                  19-Apr-1995   UNIX version
C*                                                                     *
CC**********************************************************************
 
      SUBROUTINE PRGEVT( NRET )
 
#include "prfctl.inc"
#include "prgenf.inc"
#include "prdyna.inc"
#include "prjctl.inc"
#ifdef  UNIX
#include "tbsfmt.inc"
CCC=EXPAND 'T#SM.TBS.FORT/TBSFMT.inc'
#endif
C
#ifdef  UNIX
      EXTERNAL TBVOPN
      INTEGER*4  NOPNFL
      DATA       NOPNFL/0/
      SAVE       NOPNFL
#endif
      DATA  NODFIL/ 0 /
      CHARACTER*1 NULL/X'00'/
 
C===========================<< Entry point >>==========================
#ifdef UNIX
      IF( NRET .EQ. -9998 ) GO TO 2000
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
CXX       PRINT *,' PRGEVT CALLED WITH NUMGEN =', NUMGEN
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
CXX         PRINT *,' TBINIT CALLED WITH NRET=',NRET,'  IRET=',IRET
CXX         PRINT *,' IUMGEN =', IUMGEN
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
#ifdef  MSP
          IF( NODFIL .GT. 0 ) CALL UCTLFR( NINUNI )
#endif
C#ifdef  UNIX
C         IF( NODFIL .GT. 0 ) THEN
C           STOP
C         ENDIF
C#endif
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
C      PRINT *,' PRGEVT.. NDYNAT=',NDYNAT,' TYPE=',TYPE(NDYNAT),
C     >        ' NOWEXP=', NOWEXP, 'NOWRUN=',NOWRUN,' NXTFIL=', NXTFIL
C	print *,' Pass 500...NOWEXP =',NOWEXP
      IF( NOWEXP .EQ. 0 ) THEN
#ifdef  MSP
     CALL UALCPS(NINUNI, FILDYN(NOWPNT), 'SHRR', 'VBSS', 0, IRET )
#endif
#ifdef  UNIX
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

C
#ifndef AIX
C******************************************************************
C******************************************************************
C .. Open SONY DIR1000L Format tape
C******************************************************************
C******************************************************************
        ELSEIF( NFINFO(1,NOWPNT).EQ.3 ) THEN
C     Mount a tape.
          IF( NFINFO(3,NOWPNT).EQ.1 ) THEN
      CALL TBTMNT(FILVOL(NOWPNT)(1:LNBLNK(FILVOL(NOWPNT)))//NULL,
     >           FILDEV(NOWPNT)(1:LNBLNK(FILDEV(NOWPNT)))//NULL,
     >            'READ'//NULL, KRET)
            IF(KRET.NE.0 ) THEN
       PRINT *,'Error in PRGEVT .. TBTMNT NRET=',KRET
       PRINT *,'Device=',FILDEV(NOWPNT)(1:LNBLNK(FILDEV(NOWPNT)))
       PRINT *,'Volser=',FILVOL(NOWPNT)(1:LNBLNK(FILVOL(NOWPNT)))
            ENDIF
          ENDIF
C     Open a file
CCUNIX                PRINT *,' PRGEVT ... NOWPNT +',NOWPNT
      CALL TBTOPN(FILDYN(NOWPNT)(1:LNBLNK(FILDYN(NOWPNT)))//NULL,
     >                 NFINFO(2,NOWPNT), 'READ'//NULL)
          NOPNFL= NOPNFL + 1
          IF( NOPNFL .EQ. 1 ) CALL TBUXIN(3,0)
          NRWFMT(1) = 3
#endif
	
        ELSE
            PRINT *,'In PRGEVT .. File type ',NFINFO(1,NOWPNT),
     >        ' is not supported yet.'
        ENDIF


#endif
 
 
        IF( IRET .LT. 0 ) THEN
          WRITE(6,520) FILDYN(NOWPNT)
        ELSE
          NXTFIL = 0
          GO TO 200
        ENDIF
C
C******************************************************************
C******************************************************************
C -----------------------------------------------------------
C   File allocation of TOPAS Standard strip file.
C -----------------------------------------------------------
C******************************************************************
C******************************************************************
C
      ELSE
#ifdef  MSP
      CALL UCMTAL( NINUNI, TYPDYN(NOWPNT), NOWEXP, NOWRUN, NXTFIL,
     .             OPTDYN, IRET )
        NODFIL = NODFIL + 1
C
        IF( IRET .LE. -999 ) THEN
           WRITE(6,510)NOWEXP, NOWRUN, NXTFIL
           STOP
        ELSEIF( IRET .LE. -1 ) THEN
           NXTFIL = 0
           GO TO 400
        ELSEIF( IRET .EQ. 0 ) THEN
           NXTFIL = 0
        ELSE
           NXTFIL = NXTFIL + 1
        ENDIF
        GO TO 200
      ENDIF
510   FORMAT(//,
     >' ************************************************************',/,
     >' *   Job will terminate due to input file allocation time   *',/,
     >' *   out.                                                   *',/,
     >' *       Exp# ',I6,'                                        *',/,
     >' *       Run# ',I6,'                                        *',/,
     >' *       File#',I6,'                                        *',/,
     >' ************************************************************',/,
     >   //)
#endif
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

#ifdef UNIX
800     continue
        NRET = -1000
        RETURN
C
C .. Emergency stop.
C
2000    CONTINUE
        return
#endif
      END


