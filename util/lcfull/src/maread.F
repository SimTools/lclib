C***********************************************************************
C*
C* -------------------------------------------------------
C*  Subroutine MAREAD( NEXP, NRUN1, NRUN2, MXxREC, MXxBUF,
C*                     NW,   NBUF, NREC)
C* --------------------=================
C*
C*(Function)
C*  Read RUNLIST data base record of Exp# NEXP and Run# from NRUN1
C*  to NRUN2
C*
C*(Input)
C*   NEXP  : Experiment #.
C*   NRUN1 : Start run #.
C*   NRUN2 : End   run #.
C*   MXxREC: Buffer size of each record.
C*   MXxBUF: Max. # of record.
C*
C*(Output)
C*   NW    : # of words in each record.
C*   NBUF  : Read runlist record.
C*   NREC  : # of records, = 0 when no run found.
C*                         =-1 when failed to read data base.
C*(Author)
C*  A. Miyamoto  29-May-1987  Original version.
C*               11-Jul-1987  Speed up when reading many runs.
C*
C***********************************************************************
C*
      SUBROUTINE MAREAD( NEXP, NRUN1, NRUN2, MXxREC, MXxBUF,
     >                   NW,   NBUF,  NREC)
C
#include "macntl.inc"
C
      INTEGER*4     NBUF(MXxREC, MXxBUF), NW(MXxBUF)
      CHARACTER*20  KEY
C     PARAMETER    (MCxKYL=16)
C
C======< Entry Point >==================================================
C
C ----------------------------------------------------------------------
C (1) Open Data base file.
C ----------------------------------------------------------------------
C
      NREC = -1
      IF( MCxOPS .EQ. 0 ) THEN
        CALL MAOPEN(IRET)
        IF(IRET.NE.1) THEN
          PRINT *,'%Error MAREAD .. Failed to open data base file.'
          RETURN
        ENDIF
      ENDIF
      CALL MAOPTN( 1, 1  )
C
C ----------------------------------------------------------------------
C (2) Loop to get RUNLIST record and update.
C ----------------------------------------------------------------------
C
      NREC  = 1
      IVERS = 0
      WRITE(KEY,'(''E'',I5.5,''.R'',I6.6)') NEXP, NRUN1
      CALL MAxGET(0, 1, MXxREC, KEY(:MCxKYL),
     >           IVERS, NW(1), NBUF(1,1), IRET)
C     PRINT *,' MAxGET ... Direct access...NW(1) = ', NW(1),' IRET=',
C    >          IRET
      IF(NW(1) .LT. 0 ) THEN
        IOEXP = 0
        IORUN = 0
        NREC = -1
        RETURN
      ELSE
        IOEXP = NBUF(3,1)
        IORUN = NBUF(4,1)
        NREC = 1
      ENDIF
C
2000  CONTINUE
      NREC = NREC + 1
2010  CONTINUE
      IVERS = 0
      CALL MAxGET(3, 1, MXxREC, KEY(:MCxKYL),
     >           IVERS, NW(NREC), NBUF(1,NREC), IRET)
C     PRINT *,' MAxGET..Seq. access..NW(nrec)=',NW(NREC),' IRET =', IRET
      IF(IRET.NE.0) THEN
         NREC = NREC - 1
         GO TO 800
      ENDIF
      IF( NBUF(3,NREC) .EQ. IOEXP .AND. NBUF(4,NREC) .EQ. IORUN) THEN
         GO TO 2010
      ELSEIF( NBUF(3,NREC) .EQ. IOEXP ) THEN
CC      PRINT *,' GET IRET=',IRET,' NEXP, NRUN=', NBUF(3,NREC),
CC     >                    NBUF(4,NREC)
         IORUN = NBUF(4, NREC)
         IF( NBUF(4,NREC) .LT. NRUN2 ) GO TO 2000
      ELSE
         NREC = NREC - 1
      ENDIF
C
800   CONTINUE
      RETURN
      END
