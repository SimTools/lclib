CC********************************************************************CC
C*                                                                    *C
C*===================----===                                          *C
C* Subroutine PRRSUM(IDEBUG, IRET )                                   *C
C*===================----===                                          *C
C*  1) Purpose                                                        *C
C*       Print Run summary.
C*  3) Relation                                                       *C
C*  4) Update Record                                                  *C
C*      05/06/87  A. Miyamoto  Original versionn.                     *C
C*                                                                    *C
CC********************************************************************CC
C
C =====================
      SUBROUTINE PRRSUM( IDEBUG, IRET)
C =====================
C
C
#include "fldscr.inc"
#include "prhead.inc"
#include "prjctl.inc"
#include "prrsta.inc"
#include "presta.inc"
C
      COMMON /SSWORK/ NEARY(200),
     >        MTMP1, MTMP2, MTMP3(6, 10),
     >        LTMP1, LTMP2, LTMP3(5, 64),  IBUF(100)
C
C -------------------------------------------------------------------- C
C     Reset return code.                                               C
C -------------------------------------------------------------------- C
C
      IRET = 0
      IDBG = MOD( IDEBUG, 10 )
      IF( IDBG .LT. 6 ) RETURN
C
C -------------------------------------------------------------------- C
C     Print out job summary.                                           C
C -------------------------------------------------------------------- C
C
C --- Run parameters.
C
      WRITE(6,'(1H1)')
      PRINT *,'   '
      PRINT *,'### Run end summary ################### '
      PRINT *,'   '
      PRINT *,'Last '
      PRINT *,'   Experiment #          = ',xEXPNO
      PRINT *,'   Run #                 = ',xRUNNO
C     PRINT *,'   Run Sequence #        = ',NRNSEQ
      PRINT *,'   Last Event #          = ',xEVTNO
      PRINT *,'   Last Event Sequence # = ',IEVSEQ
      PRINT *,'   # of read  records.   = ',MTMP1
      PRINT *,'   # of write records.   = ',MTMP2
      XCPU  = FLOAT(LTMP2)/1000.
      PRINT *,'   CPU time for this run = ',XCPU,' sec.'
      PRINT *,'   '
      PRINT *,'####################################### '
C
C --- Statistics for record processing.
C
      IF( IDBG .LT. 8 ) RETURN
C
      PRINT *,'   '
      PRINT *,' << Statistics of Records >>'
      PRINT *,'   '
      WRITE(6,210)
 210  FORMAT(' Record Id     #Read  #Process     #Good',
     .       '  #Warning    #Fatal  #Written')
      DO 200 IDSB = 1, 10
        WRITE(6,220) IDSB,(MTMP3(K,IDSB),K=1,6)
 220    FORMAT(7I10)
 200  CONTINUE
C
C --- Statistics for data reduction.
C
      PRINT *,'   '
      PRINT *,' << Event analysis statistics >>'
      PRINT *,'   '
C
      WRITE(6,310)
310   FORMAT(
     .' Exec                                        Total-CPU',/,
     .' Seq. Module name  #Call #Good #Warn #Fatal    (sec)  ')
      DO 300 I = 1, LISTSP(0)
        LI  = LISTSP(I)
        XCPU = FLOAT(LTMP3(5, LI))/1000.
        WRITE(6,340) I, ENTNAM(LI), (LTMP3(K,LI),K=1,4), XCPU
340     FORMAT(I5,1X,A12,4I6,F10.3)
300   CONTINUE
C
C
      RETURN
      END
