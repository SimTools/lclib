C**********************************************************************
C*
C* --------------------------------
C* Subroutine MASRUN(MODE, NW, NBUF)
C* ---------------------------------
C*
C*(Function)
C*   Save updated run information
C*(Input)
C*   MODE  : = 0 for the initialize, = 1 to stack, = 2 to update DB.
C*   NW    : # of records of this run.
C*   IBUF  : Run list records.
C*(Output)
C*   none.
C*
C*(Author)
C*   A. Miyamoto  10-Mar-1987  Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MASRUN(MODE,  NW, NBUF)
C
#include "macntl.inc"
C
      CHARACTER*20  KEY
      CHARACTER     DSN*44, JDSN*44
      INTEGER*4     NBUF(NW), KBUF(5)
      PARAMETER    (MAXRUN=100, MXxLEN=130)
      COMMON /MASBUF/ NORUN, NWBUF(MAXRUN), NRBUF(MXxLEN, MAXRUN)
#if defined(G77_521_OR_GREATER)
      INTEGER*4     TIME
#endif
C
C
C ===========< Entry Point >============================================
C
C ----------------------------------------------------------------------
C (1) MODE = 0 to clear stack.
C ----------------------------------------------------------------------
C
      IF(MODE.EQ.0) THEN
        NORUN = 0
        RETURN
C
C ----------------------------------------------------------------------
C (2) MODE = 1 to Stack Run information.
C ----------------------------------------------------------------------
C
      ELSEIF(MODE.EQ.1) THEN
        IF(NORUN.GT.MAXRUN) THEN
          PRINT *,' Too many runs in stack, Enter EX to continue.'
          RETURN
        ENDIF
        NORUN = NORUN + 1
        NWBUF(NORUN) = NW
        DO 200 I = 1, NW
          NRBUF(I, NORUN) = NBUF(I)
200     CONTINUE
        PRINT *,' Exp#',NBUF(3),' Run#',NBUF(4),' Stacked.  #Run=',NORUN
        RETURN
C
C ----------------------------------------------------------------------
C (3) Mode = 3 to submit job to update data base.
C ----------------------------------------------------------------------
C
      ELSEIF(MODE.EQ.2) THEN
C
C Allocate Temprary file to store new RUNLIST information.
C
        CALL UIDATE(IDATE)
#if defined(G77_521_OR_GREATER)
        ITIME = TIME()
#else
        CALL TIME(ITIME)
#endif
        ITIME = ITIME/1000
        WRITE(DSN,310) CMxUID(:4),IDATE,ITIME
310     FORMAT(A,'.@RUNLIST.UPDATE.D',I6.6,'.T',I6.6)
        IJUNI = 85
        CALL UALCPS(IJUNI,DSN,'NEW','VBS',30,IRET)
        DO 330 IR = 1, NORUN
          WRITE(KEY,320) NRBUF(3,IR), NRBUF(4,IR)
320       FORMAT('E',I5.5,'.R',I6.6)
          READ(KEY,'(5A4)') KBUF
          KKW = NWBUF(IR) + 5
          WRITE(IJUNI) KKW, KBUF,
     >           (NRBUF(I,IR),I=1,NWBUF(IR))
330     CONTINUE
        CLOSE(IJUNI)
C
C Create JCL to update RUNLIST
C
        JDSN = CMxUID(:4)//'.@RUNLIST.CNTL'
        CALL UALCPS(IJUNI,JDSN,'RENEW','FB',10,IRET)
        WRITE(IJUNI,350) MCxKEY, MCxKYL, MCxVSM, CMxDBS,
     >                   DSN
350     FORMAT('//XXXX JOB CLASS=E',/,
     >         '//VSAMUPDT EXEC PGM=VSAMUTY',/,
     >         '//STEPLIB DD DSN=T#ON.DATABASE.EXEC,DISP=SHR',/,
     >         '//FT06F001 DD SYSOUT=*',/,
     >         '//*** Data base definition data ',/,
     >         '//FT01F001 DD *',/,
     >         I10,',',I10,',',I10,/,
     >         A,/,
     >         '*** End of Data base description.',/,
     >         '//FT02F001 DD *',/,
     >         'ADD 10',/,
     >         '//FT10F001 DD DISP=(OLD,DELETE,KEEP),',/,
     >         '//  DSN=',A)
        CLOSE(IJUNI)
        CALL PFDLNK('EDIT',JDSN(6:))
        NORUN = 0
      ENDIF
C
      RETURN
      END
