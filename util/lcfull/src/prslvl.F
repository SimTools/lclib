C***********************************************************************
C*
C*  -------------------
C*   Subroutine PRSLVL
C*  -------------------
C*(Function)
C*   Store LEVEL and DEBUG flag data set by STEERING command into
C*   EXELVL and DBGLVL arrays of common /FLLEVL/
C*
C*(Relation)
C*   Called by PRRPAR after reading all STEERING command.
C*
C*(Author)
C*   A. Miyamoto  18-Sep-1989  Original version.
C*                21-Mar-1990  Set ENTQUI for RFILTER
C*
C***********************************************************************
C*
      SUBROUTINE PRSLVL
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
      PARAMETER (NUMDFL = 35 )
      CHARACTER*12 NAMDFL(NUMDFL)
      INTEGER*4    INDDFL(3,NUMDFL)
      DATA (NAMDFL(I),(INDDFL(K,I),K=1,3),I=1,NUMDFL) /
     >  'IDC:', 4, 6, 1,    'TPC:', 4, 7,1,  'BTOF:', 5, 8, 1,
     >  'BDC:', 4, 9, 1,    'BCL:', 4,10,1,  'BMU:', 4, 11, 1,
     >  'ETOF:',5,12, 1,    'EDC:', 4,13,1,  'ECL:', 4,14,1,
     >  'LUM:', 4,15, 1,    'EMU:', 4,16,1,  'HY:LNK_TRK', 10, 17, 1,
     >  'HY:VERTEX-1',11,18,1,'HY:CLUSTER', 10, 19, 1,
     >  'VTX:', 4, 21, 1,   'TCH:', 4,22,1,  'FCL:', 4,23,1,
     >  'RCL:', 4, 24, 1,
     >  'RIDC:', 5, 6, 2,    'RTPC:', 5, 7,2,  'RBTOF:',6, 8, 2,
     >  'RBDC:', 5, 9, 2,    'RBCL:', 5,10,2,  'RBMU:', 5, 11, 2,
     >  'RETOF:',6,12, 2,    'REDC:', 5,13,2,  'RECL:', 5,14,2,
     >  'RLUM:', 5,15, 2,    'REMU:', 5,16,2,  'RFILTER:', 8, 17, 2,
     >  'RVTX:', 5, 21, 2,   'RTCH:', 5,22,2,  'RFCL:', 5,23,2,
     >  'RRCL:', 5, 24, 2,   'USER:', 5, 20,1 /
C
C      INDDFL(1,i)  : Character length of NAMDFL
C            (2,i)  : 1-st Index to NJBLVL and NJBDBG
C            (3,i)  : 2-nd index to    "
C
C ======< Entry Point >=================================================
C
C
      DO 100 I = 1, NENTR
        DO 200 ID = 1, NUMDFL
          ILEN = INDDFL(1,ID)
          IF( ENTNAM(I)(1:ILEN) .NE. NAMDFL(ID)(1:ILEN) ) GO TO 200
          EXELVL(I) = NJBLVL(INDDFL(2,ID),INDDFL(3,ID))
          DBGLVL(I) = NJBDBG(INDDFL(2,ID),INDDFL(3,ID))
          IF( ENTNAM(I)(1:ILEN) .EQ. 'RFILTER:' ) THEN
            CALL PRINFO( 10, ENTQUI(I) )
          ENDIF
          GO TO 210
200     CONTINUE
210     CONTINUE
100   CONTINUE
C
      RETURN
      END
