CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / PRESTA /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*  This COMMON contains statistical information for each event       *C
C* analysis routine.                                                  *C
C*    NSTEVT(1,j) = Number of event analized by j-th analysis routine.*C
C*          (2,j) = Number of good event in j-th analysis routine.    *C
C*          (3,j) = Number of warning level error in j-th analysis.   *C
C*          (4,j) = Number of Fatal error in j-th analysis.           *C
C*          (5,j) = used CPU time ( msec) by j-th analysis.           *C
C*    NCTBGN      = Cpu time at the Begining of Event. (msec)         *C
C*    NCTEND      = Total CPU time at the end of Event.(msec)         *C
C*                                                                    *C
C*    Mxxxxx is the same information at the begining of RUN.          *C
C*                                                                    *C
C* 2) Update Record                                                   *C
C*    10/15/85  A. Miyamoto  Original Version.                        *C
C*                                                                    *C
CC********************************************************************CC
      COMMON /PRESTA/ NCTBGN, NCTEND, NSTEVT(5, 64),
     >                MCTBGN, MCTEND, MSTEVT(5, 64)
C*
