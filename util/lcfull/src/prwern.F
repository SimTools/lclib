CC*********************************************************************C
C*                                                                     *
C*------------------                                                   *
C* Subroutine PRWERN
C*------------------                                                   *
C*                                                                     *
C*(Purpose)                                                            *
C*    Update End_Run record at the end of Run.
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
      SUBROUTINE PRWERN
C
#include "prrsta.inc"
#include "presta.inc"
#include "prdtyp.inc"
#include "prjctl.inc"
C
      COMMON /SSWORK/ NEARY(200),
     >        MTMP1, MTMP2, MTMP3(6, 10),
     >        LTMP1, LTMP2, LTMP3(5, 64),  IBUF(100)
      DATA       IDEND/8/
C     CHARACTER*136  MESAGE
 
C==================<<  Entry Point  >>================================
 
C
C ---------------------------------------------------------------------
C (1) Create Production Bank.
C ---------------------------------------------------------------------
C
      IEPED = 100*(NDTYPE(9)-1)
      IF( IEPED .LE. 0 ) THEN
        CALL TBCRTE( IDEND, 'Full_Analysis', 0, 0, IRET)
      ENDIF
C
C ---------------------------------------------------------------------
C (2) Output Error summary..
C ---------------------------------------------------------------------
C
      CALL PRWERR( IDEND, IERFLG )
C
C ---------------------------------------------------------------------
C (3) Output Record Statistics.
C ---------------------------------------------------------------------
C
      MTMP1   = NRDREC - MRDREC
      MTMP2   = NWTREC - MWTREC + 1
      DO 310 I = 1, 10
        DO 320 J = 1, 6
          MTMP3(J, I) = NSTREC(J,I) - MSTREC(J,I)
320     CONTINUE
310   CONTINUE
      MTMP3(6,8)    = MTMP3(6,8) + 1
      CALL TBPUT(IDEND, 'Full_Analysis', IEPED + 2, 62, MTMP1, IRET)
C
C ---------------------------------------------------------------------
C (4) Output Event  Statistics.
C ---------------------------------------------------------------------
C
C     LTMP1   = NCTEND - MCTBGN
      LTMP1   = 0
      LTMP2   = NCTEND - MCTBGN
      DO 410 I = 1, 64
        DO 420 J = 1, 5
          LTMP3(J, I) = NSTEVT(J,I) - MSTEVT(J,I)
420     CONTINUE
410   CONTINUE
      CALL TBPUT(IDEND, 'Full_Analysis', IEPED + 3, 324, LTMP1, IRET)
C
C
C ---------------------------------------------------------------------
C (5) Output Element # 1.
C ---------------------------------------------------------------------
C
      IBUF(1) = IEVSEQ
      CALL UIDATE(IBUF(2))
cc      CALL   TIME(IBUF(3))
      CALL   uiTIME(IBUF(3))
      IBUF(4) = IERFLG
      CALL TBPUT(IDEND, 'Full_Analysis', IEPED+1, 4, IBUF, IRET)
 
C
      RETURN
      END
