C***********************************************************************
C*
C*  ---------------------------------==============
C*  Subroutine FEVUSR( IDREC, NEVSEQ, OKFLG, EXIFLG )
C*  ---------------------------------==============
C*
C*(Function)
C*   Steer event analysis routine
C*
C*(Input)
C*   IDREC   : Record ID of the event.
C*   NEVSEQ  : Event sequence #
C*
C*(Output)
C*   OKFLG   : Return flag.
C*            = .TRUE. When no error is detected in the event analysis.
C*                     NORMAL return and Warning level should be .TRUE.
C*                     Results is written on file, when OKFLG = .TRUE.
C*            = .FALSE. When Serious or FATAL error is detected.
C*   EXIFLG  : Return flag.
C*            = .TRUE. to continue analysis.
C*            = .FALSE. to terminate analysis.
C*
C*(Relation)
C*   Called from FDOEVT.
C*
C*(Author)
C*   A.Miyamoto  30-Apr-1987  Modified from R.Itoh's original
C*                            for the Production job.
C*   A.Miyamoto  19-May-1987  Use NFSTDB and NLSTDB to set debug flag.
C*               30-Sep-1987  Change meanings of debug flag.
C*                7-Oct-1987  Not increment NSTEVT when module is not
C*                            called.
C*
C***********************************************************************
C*
      SUBROUTINE FEVUSR( IDREC, NEVSEQ, OKFLG, EXIFLG )
C
      LOGICAL * 4   OKFLG, EXIFLG
C
C COMMON
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "presta.inc"
#include "prrsta.inc"
#include "prjflg.inc"
#include "prjctl.inc"
#include "prjpr1.inc"
C
        LOGICAL * 4     FBLCHK, FBLCRE
        LOGICAL * 4     XBLCHK, XBLCRE
        LOGICAL * 4     WRNFLG
C
C MAIN
C
C =======< Entry Point >================================================
C
C
C ----------------------------------------------------------------------
C (0) Execute Event initialization of analysis routine.
C ----------------------------------------------------------------------
C
 
      CALL PREVIN( IDREC, NJBLVL(2,1), NJBDBG(2,1), IRET)
 
C
C ----------------------------------------------------------------------
C (1) Reset flag and accumulate record statistics.
C ----------------------------------------------------------------------
C
 
      OKFLG  = .FALSE.
      WRNFLG = .FALSE.
      IDBG   = NJBDBG(2, 1)/10
 
C
      IDRECX = IDREC
      IF( IDREC .LT. 1 .OR. IDREC .GT. 9  ) IDRECX = 10
      NSTREC( 2, IDRECX) = NSTREC(2, IDRECX) + 1
C
      IF( IDBG .GE. 2) THEN
        WRITE(6,110) IDREC
110     FORMAT(1H ,/,' Start event analysis:Record ID=',I5,/,
     >' Exec                          Blk Blk      CPU    Ok Wrn Ext',/,
     >' Seq. Ord Module name  Lvl Dbg Chk Cre Iret(msec) Flg Flg Flg',/,
     >' ---+---+------------+---+---+----+---+----+-----+---+---+---+')
      ENDIF
C
C
C ----------------------------------------------------------------------
C (2) Loop to call pre-declared event analysis routine.
C ----------------------------------------------------------------------
C
      DO 200 I = 1, LISTSP(0)
 
        LI     = LISTSP(I)
        LEVEL  = EXELVL(LI)
        IF( LEVEL .EQ. 0 .OR. ENTEVT(LI) .EQ. 0) GO TO 200
 
C
C (2.1) Set Debug flag of this event
C
C        IF ( ( DBGRNG(1,LI).EQ.0 .AND. DBGRNG(2,LI).EQ.0 ) .OR.
C    #    ( NEVSEQ.GE.DBGRNG(1,LI) .AND. NEVSEQ.LE.DBGRNG(2,LI) ) ) THEN
C         NDEBUG = DBGLVL(LI)
C       ELSE
C         NDEBUG = 0
C       END IF
        IF( NEVSEQ .GE. NFSTDB .AND. NEVSEQ .LE. NLSTDB ) THEN
          NDEBUG = DBGLVL(LI)
        ELSE
          NDEBUG = 0
        ENDIF
C
C
C ----------------------------------------------------------------------
C (3) Call event analysis routine.
C ----------------------------------------------------------------------
C
        IF( IDREC .EQ. 1 ) THEN
          IF( NDELBK .EQ. 1 ) CALL FBKDEL( LI )
          XBLCHK = FBLCHK(LI)
          XBLCRE = FBLCRE(LI)
 
          IF ( XBLCHK     .AND. XBLCRE     ) THEN
CCC     IF ( FBLCHK(LI) .AND. FBLCRE(LI) ) THEN
 
        ENDIF
 
          CALL CLOCKM ( IBTIME )
 
#ifdef  MSP
          CALL FEXEVT ( ENTEVT(LI), IDREC, LEVEL ,NDEBUG, IRET )
#endif
#ifdef  UNIX
#if !defined(__GFORTRAN__)
        CALL FEXEVT(%VAL(ENTEVT(LI)), IDREC, LEVEL ,NDEBUG, IRET )
#else
        CALL cFEXEVT(ENTEVT(LI), IDREC, LEVEL ,NDEBUG, IRET )
#endif
#endif
 
          CALL CLOCKM ( IETIME )
C
C ----------------------------------------------------------------------
C (4) Accumulate statistics and set return flag.
C ----------------------------------------------------------------------
C
C
          NSTEVT(1, LI) = NSTEVT(1, LI) + 1
          NSTEVT(5, LI) = NSTEVT(5, LI) + ( IETIME - IBTIME )
C
          IF( IRET .GE. ENTQUI(LI) .AND. IRET .LE. 0 ) THEN
            NSTEVT(2, LI) = NSTEVT(2, LI) + 1
            OKFLG         = .TRUE.
          ELSEIF(IRET .GT. 0) THEN
            NSTEVT(3, LI) = NSTEVT(3, LI) + 1
            OKFLG         = .TRUE.
            WRNFLG        = .TRUE.
          ELSEIF(IRET .LT. ENTQUI(LI)  .AND. IRET .GT. -100) THEN
            NSTEVT(4, LI) = NSTEVT(4, LI) + 1
            OKFLG         = .FALSE.
            EXIFLG        = .FALSE.
            GO TO 900
          ELSE
            OKFLG         = .FALSE.
            EXIFLG        = .TRUE.
            GO TO 900
          END IF
CCC     ELSE
CCC       NSTEVT(4, LI) = NSTEVT(4, LI) + 1
CCC       OKFLG  = .FALSE.
CCC       EXIFLG = .FALSE.
        END IF
C
        IF( IDBG .GE. 2 ) THEN
          ICPU = IETIME - IBTIME
          WRITE(6,290) I, LI, ENTNAM(LI), LEVEL,NDEBUG, XBLCHK, XBLCRE,
     >               IRET,  ICPU, OKFLG, WRNFLG, EXIFLG
290       FORMAT(I4,I4,1X,A,1X,2I4,1X,2L4,I5,I6,3L5)
        ENDIF
200   CONTINUE
CCC   CALL FBLREM
C
C ----------------------------------------------------------------------
C (9) Execute Event Termination
C ----------------------------------------------------------------------
C
900   CONTINUE
 
      CALL PREVTM( IDREC, NJBLVL(2,1), NJBDBG(2,1), IRET)
 
C
C ----------------------------------------------------------------------
C (9) Accumulate counter at the end of event.
C ----------------------------------------------------------------------
C
C
      IF( OKFLG ) THEN
        IF( WRNFLG ) THEN
          NSTREC(4, IDRECX) = NSTREC(4, IDRECX) + 1
        ELSE
          NSTREC(3, IDRECX) = NSTREC(3, IDRECX) + 1
        ENDIF
      ELSEIF( .NOT. EXIFLG ) THEN
         NSTREC( 5, IDRECX ) = NSTREC(5, IDRECX) + 1
      ENDIF
C
      IF( IDBG .GE. 2 ) THEN
        ICPU = IETIME - IBTIME
        WRITE(6,910)        ENTNAM(LI), LEVEL,NDEBUG, XBLCHK, XBLCRE,
     >             IRET,  ICPU, OKFLG, WRNFLG, EXIFLG
910     FORMAT(' End at ',1X,A,1X,2I4,1X,2L4,I5,I6,3L5)
       ENDIF
C
      RETURN
      END
