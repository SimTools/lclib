CC********************************************************************CC
C*                                                                    *C
C*===================----===                                          *C
C* Subroutine PRJSUM(IRET)                                            *C
C*===================----===                                          *C
C*  1) Purpose                                                        *C
C*       Creates job summary for this run.                            *C
C*  3) Relation                                                       *C
C*       Called by                                                    *C
C*           PRJOBT : termintaes a production job.                    *C
C*       Call the following subroutines                               *C
C*           UERTRM : prints out error statistics.                    *C
C*  4) Update Record                                                  *C
C*      10/21/85  A. Miyamoto  Original versionn.                     *C
C*       6/03/87  A. Miyamoto  Output output size.                    *C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----===
      SUBROUTINE PRJSUM(IRET)
C ======================----===
C
C
#include "fldscr.inc"
#include "prhead.inc"
#include "prjctl.inc"
#include "prjflg.inc"
#include "prrsta.inc"
#include "presta.inc"
#include "prouts.inc"
C
C -------------------------------------------------------------------- C
C     Reset return code.                                               C
C -------------------------------------------------------------------- C
C
      IRET = 0
      IDBG = MOD( NJBDBG(2, 1), 10 )
C
C -------------------------------------------------------------------- C
C     Print out job summary.                                           C
C -------------------------------------------------------------------- C
C
C --- Run parameters.
C
      WRITE(6,'(1H1)')
      PRINT *,'   '
      PRINT *,'*** Job end summary ******************* '
      PRINT *,'   '
      PRINT *,'Last '
      PRINT *,'   Experiment #          = ',xEXPNO
      PRINT *,'   Run #                 = ',xRUNNO
C     PRINT *,'   Run Sequence #        = ',NRNSEQ
      PRINT *,'   Last Event #          = ',xEVTNO
      PRINT *,'   Last Event Sequence # = ',IEVSEQ
      PRINT *,'   # of read  records.   = ',NRDREC
      PRINT *,'   # of write records.   = ',NWTREC
      WRITE(6,120) OUTTAP, OUTDSK
120   FORMAT( '   Output data size      = ',F10.2,
     >        ' feet on CTL tape.( BLKSIZE=32760 )',/,
     >        '                         = ',F10.2,
     >        ' Tracks on Disk   ( BLKSIZE=23476 )')
      PRINT *,'   '
      PRINT *,'*************************************** '
      IF( IDBG .LE. 0 ) RETURN
      IF( IDBG .EQ. 4 ) RETURN
C
C --- Statistics for record processing.
C
      PRINT *,'   '
      PRINT *,' << Statistics of Records >>'
      PRINT *,'   '
      WRITE(6,210)
 210  FORMAT(' Record Id     #Read  #Process     #Good',
     .       '  #Warning    #Fatal  #Written')
      DO 200 IDSB = 1, 10
        WRITE(6,220) IDSB,(NSTREC(K,IDSB),K=1,6)
 220    FORMAT(7I10)
 200  CONTINUE
C
C --- Statistics for data reduction.
C
      PRINT *,'   '
      PRINT *,' << Event analysis statistics >>'
      PRINT *,'   '
C
      XTCPU = 0.0
      WRITE(6,310)
310   FORMAT(
     .' Exec                                        Total-CPU',/,
     .' Seq. Module name  #Call #Good #Warn #Fatal    (sec)  ')
      DO 300 I = 1, LISTSP(0)
        LI     = LISTSP(I)
        XCPU   = FLOAT(NSTEVT(5, LI))/1000.
        XTCPU  = XTCPU + XCPU
        WRITE(6,340) I, ENTNAM(LI), (NSTEVT(K,LI),K=1,4), XCPU
340     FORMAT(I5,1X,A12,4I6,F10.3)
300   CONTINUE
      CALL CLOCKM( IETIME )
      XTTCPU = FLOAT(IETIME)/1000.
      WRITE(6,350) XTCPU, XTTCPU
350   FORMAT(6X,'Sum of CPU time',21X,F10.3,/,
     .       6X,'Total CPU time of JOB',15X,F10.3)
C
      IF( IDBG .LE. 1 ) RETURN
      IF( IDBG .EQ. 4 .OR. IDBG .EQ. 5 .OR. IDBG .EQ. 6 ) RETURN
C
C -------------------------------------------------------------------- C
C     Print out error statistics.                                      C
C -------------------------------------------------------------------- C
C
CTOPAZ       CALL UERTRM
C
C
      RETURN
      END
