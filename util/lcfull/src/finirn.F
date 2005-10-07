C
C----------
C FINIRN
C----------
C
        SUBROUTINE FINIRN
C
C COMMON
C
C       COMMON / HCOM / HMEMOR ( 1 )
C       INTEGER * 4     HMEMOR
#include "fldscr.inc"
C       INCLUDE         'FLDSCR.INC'
#include "fllevl.inc"
#include "prjflg.inc"
C
C VARIABLES
C
        INTEGER * 4     I
        INTEGER * 4     NWRD
        INTEGER * 4     IRET
        INTEGER * 4     DATTYP ( 9 ), URH ( 16 ), GENHED ( 7 )
        INTEGER * 4     PNTR
C
C FUNCTIONS
C
        INTEGER * 4     HPNTR
C
C MAIN
C
C
C   Call Run initialization of Steering routine.
C
        CALL PRRUNI( NJBLVL(2,1), NJBDBG(2,1))
C
C        DO 10 I = 1, NPROG
	DO 10 I = 1, LISTSP(0)
           LI = LISTSP(I)
          IF ( ENTBRU(LI) .NE. 0 .AND. EXELVL(LI) .GT. 0 ) THEN
#ifdef  MSP
            CALL FEXPRN ( ENTBRU(LI), EXELVL(LI), DBGLVL(LI) )
#endif
#ifdef  UNIX
#if !defined(__GFORTRAN__)
            CALL FEXPRN (%VAL(ENTBRU(LI)), EXELVL(LI), DBGLVL(LI) )
#else
            CALL cFEXPRN (ENTBRU(LI), EXELVL(LI), DBGLVL(LI) )
#endif
#endif
          END IF
10      CONTINUE
C
        CALL PRRNIN( NJBLVL(2,1), NJBDBG(2,1) )
C
C       PRINT *,' FINIRN... HISACT = ',HISACT
C       IF ( HISACT .NE. 0 ) THEN
C         CALL TBGET ( 2, 'Data_Type', 1, NWRD, DATTYP, IRET )
C         IF ( IRET .EQ. 1 ) THEN
C           IF ( DATTYP ( 1 ) .EQ. 2 ) THEN
C             CALL TBGET ( 2, 'Generator', 1, NWRD, GENHED, IRET )
C             IF ( IRET .EQ. 1 ) THEN
C               DATTYP ( 5 ) = GENHED ( 3 )
C               DATTYP ( 6 ) = GENHED ( 4 )
C               DATTYP ( 7 ) = GENHED ( 5 )
C               DATTYP ( 8 ) = GENHED ( 6 )
C             END IF
C           END IF
C            PNTR = HPNTR ( 'TYPE', 'HIST' )
C            DO 20 I = 1, 9
C              HMEMOR ( PNTR + I ) = DATTYP ( I )
C20          CONTINUE
C          END IF
C         CALL TBGET ( 2, 'Begin_Run:Universal_Record_Header', 1, NWRD,
C    #                 URH, IRET )
C         IF ( IRET .EQ. 1 ) THEN
C           PNTR = HPNTR ( 'URHD', 'HIST' )
C           DO 30 I = 1, 16
C             HMEMOR ( PNTR + I ) = URH ( I )
C30          CONTINUE
C          END IF
C        END IF
        RETURN
        END
