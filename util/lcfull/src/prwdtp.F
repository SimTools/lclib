CC*********************************************************************C
C*                                                                     *
C*------------------                                                   *
C* Subroutine PRWDTP
C*------------------                                                   *
C*                                                                     *
C*(Purpose)                                                            *
C*    Write Data type bank in Begin_Run record.
C*                                                                     *
C*(Input&Output)                                                       *
C*    none.                                                            *
C*    Requires common /PRJENV/                                         *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto  06-May-1987  Original version.                      *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRWDTP
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjenv.inc"
#include "prdtyp.inc"
#include "clibuf.inc"
C
      COMMON /SSWORK/ NEARY(200), IBUF(1000)
      DATA       IDBGN/2/
      CHARACTER*130  MESAGE
#ifdef  MSP
      INTEGER*4      CLIMEM
#endif
 
C==================<<  Entry Point  >>================================
 
C
C ---------------------------------------------------------------------
C (1) Update Data_Type bank.
C ---------------------------------------------------------------------
C
C  Format of Data_Type bank.
C  Element #1.
C     IBUF(1) = Real or MC data ( 1,2) = (Real, MC)
C         (2) = Type of Data (0,1,2,3) = (Gen., Mag off, Mag on, Calib.)
C         (3) = Generator (0,1) = (not done, done)
C         (4) = Simulator (0,1) = (not done, done)
C         (5) = Trigger_Simulator (0,1) = (not done, done)
C         (6) = Formatting (0,1) = (not done, done)
C         (7) = Reduction (0,Nr) = (not done, done)
C         (8) = Production(0,Nr) = (not done, done)
C         (9) = Full_Analysis(0,Nr) = (not done, done)
 
      CALL TBNOEL(IDBGN, 'Data_Type', NELM, NEARY)
      IF( NELM .LE. -2 ) THEN
        CALL UERROR(3, DUMMY, 1, 'PRWDTP',
     >  'Begin_Run record not exists.')
 
      ELSEIF( NELM .EQ. -1 ) THEN
        CALL TBCRTE(IDBGN, 'Data_Type', 0, 0, IRET)
        DO 110 I = 1, 9
          NDTYPE(I) = NEXTYP(I)
110     CONTINUE
        IF( IRET .LE. 0 ) THEN
          WRITE(MESAGE,*) 'Failed to TBCRTE Data_Type, IRET=',IRET
          GO TO 9000
        ENDIF
 
       ELSEIF( NELM .EQ. 0 ) THEN
        DO 120 I = 1, 9
          NDTYPE(I) = NEXTYP(I)
120     CONTINUE
 
      ELSE
#ifdef  MSP
        CALL TBGET(IDBGN, 'Data_Type', 1, NW, NDTYPE, IRET, 9)
#endif
#ifdef  UNIX
        CALL TBGETx(IDBGN, 'Data_Type', 1, NW, NDTYPE, IRET, 9)
#endif
        IF( IRET .LE. 0 ) THEN
          WRITE(MESAGE,*) 'Failed to TBGET Data_Type, IRET=',IRET
          GO TO 9000
        ENDIF
        DO 130 I = 3, 9
          IF( NEXTYP(I) .NE. 0 ) NDTYPE(I) = NDTYPE(I) + 1
130     CONTINUE
      ENDIF
      CALL TBPUT(IDBGN, 'Data_Type', 1, 9, NDTYPE, IRET)
      IF( IRET .LE. 0 ) THEN
        WRITE(MESAGE,*) 'Failed to TBPUT Data_Type, IRET=',IRET
        GO TO 9000
      ENDIF
C
C
C     PRINT *,' In PRWDTP.. NDTYPE=',NDTYPE
C
C ---------------------------------------------------------------------
C (2) Update FULL_Analysis bank.
C ---------------------------------------------------------------------
C
      IEPED = 100*(NDTYPE(9)-1)
      IF( IEPED  .EQ. 0 ) THEN
        CALL TBCRTE( IDBGN, 'Full_Analysis', 0, 0, IRET)
        IF( IRET .LE. 0 ) THEN
          WRITE(MESAGE,*) 'Failed to TBCRTE Full_Analysis IRET=',IRET
          GO TO 9000
        ENDIF
      ENDIF
C
C ---------------------------------------------------------------------
C (3) Put Element #1 of Full_Analysis bank.
C ---------------------------------------------------------------------
C
C   Element#(1 + 100*(Nf-1))
C     IBUF(1) = Process Data.
C         (2) = Process time.
C       (3:6) = Program name.
C         (7) = Program version #.
C       (8:11)= Name of FLANLI
C        (12) = FLANLI version #.
C        (13) = FLANLI version date.
C     (14:15) = User name.
C     (16:17) = Machine name.
C
      CALL UIDATE(IBUF(1))
cc      CALL   TIME(IBUF(2))
      CALL   uiTIME(IBUF(2))
      READ(PRGNAM,'(4A4)') (IBUF(K),K=3,6)
      CALL PRGVRS( IVERS, IVLVL, MESAGE)
      IBUF(7) = 100*IVERS + IVLVL
      READ(DSCNAM,'(4A4)') (IBUF(K),K=8,11)
      IBUF(12) = NDSCVR
      IBUF(13) = NDSCDT
#ifdef  MSP
      READ(USERID,'(2A4)') IBUF(14), IBUF(15)
      READ(MACHIN,'(2A4)') IBUF(16), IBUF(17)
#endif
#ifdef  UNIX
#ifdef HIUXF77
      call getenv("USER", 4, mesage, 8)
      READ(MESAGE,'(2A4)') IBUF(14), IBUF(15)
      call getenv("HOST", 4, mesage, 8)
      READ(MESAGE,'(2A4)') IBUF(16), IBUF(17)
#elif MACHTEN_PPC
#else
      CALL GETLOG(MESAGE)
      READ(MESAGE,'(2A4)') IBUF(14), IBUF(15)
      IRET = HOSTNM(MESAGE)
      READ(MESAGE,'(2A4)') IBUF(16), IBUF(17)
#endif
#endif
      IELM = IEPED + 1
      CALL TBPUT(IDBGN, 'Full_Analysis', IELM,17, IBUF, IRET)
      IF( IRET .LE. 0 ) THEN
        WRITE(MESAGE,*)
     >  'Failed to TBPUT Full_Analysis Element#',IELM,' Iret=',IRET
        GO TO 9000
      ENDIF
C
C ---------------------------------------------------------------------
C (4) Put Element #2 of Full_Analysis bank.
C ---------------------------------------------------------------------
C
C   Element#(2 + 100*(Nf-1))
C     Following information is saved according to the execution seq.
C     IBUF(1:3,i) = Module name.
C         (4,i)   = Execution level.
C         (5,i)   = Version Level of the module.
C         (6,i)   = Version date of the module.
C         (7,i)   = Analysis quit level.
C
      DO 400 I = 1, LISTSP(0)
        LI   = LISTSP(I)
        IPED = 7*(I-1)
        READ(ENTNAM(LI),'(3A4)') (IBUF(K),K=IPED+1, IPED+3)
        IBUF(IPED+4) = EXELVL(LI)
        IBUF(IPED+5) = MODVRS(LI)
        IBUF(IPED+6) = MODDAT(LI)
        IBUF(IPED+7) = ENTQUI(LI)
400   CONTINUE
      NW = IPED + 7
      IELM = IEPED + 2
      CALL TBPUT(IDBGN, 'Full_Analysis', IEPED+2, NW, IBUF, IRET)
      IF( IRET .LE. 0 ) THEN
        WRITE(MESAGE,*)
     >  'Failed to TBPUT Full_Analysis Element#',IELM,' Iret=',IRET
        GO TO 9000
      ENDIF
C
C
C ---------------------------------------------------------------------
C (5) Put Element #3 of Full_Analysis bank.
C     Constant version #, saved by sub. PRWCNS called by PRRNIN.
C ---------------------------------------------------------------------
C
C ---------------------------------------------------------------------
C (6) Put Element #4 of Full_Analysis bank.
C     Input parameters, saved by CLIBUF.
C ---------------------------------------------------------------------
C
C
#ifdef  MSP
      LINP = CLIMEM( 0 )
#endif
#ifdef  UNIX
        LINP = -1
#endif
#ifdef  UNIX
        IF( LINP .GT. 0 ) THEN
#endif
CX       PRINT *,' # of input lines =',LINP
CX       DO 610 I = 1, LINP
CX         PRINT *,I,' ',CLINES(I)
CX 610   CONTINUE
CX       PRINT *,' '
      IF( LINP .GE. MXxLIN ) THEN
         PRINT *,'%Warning PRWDTP .. Too many input lines.',
     >           ' rest will not be saved in Full_Analysis bank.'
         CLINES(MXxLIN) = '%Warning.. PRWDTP  too many input lines.'
      ENDIF
      NW   = 20*LINP
      IELM = IEPED + 4
      CALL TBPUT(IDBGN, 'Full_Analysis', IELM, NW, CLINES(1), IRET)
      IF( IRET .LE. 0 ) THEN
        WRITE(MESAGE,*)
     >  'Failed to TBPUT Full_Analysis Element#',IELM,' Iret=',IRET
        GO TO 9000
      ENDIF
#ifdef  UNIX
      ENDIF
#endif
C
C
      RETURN
C
C
C ---------------------------------------------------------------------
C (9) Error Messaged.
C ---------------------------------------------------------------------
C
9000  CALL UERROR(3, DUMMY, 1, 'PRWDTP',MESAGE)
      STOP
      END
