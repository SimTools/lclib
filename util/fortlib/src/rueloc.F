CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine RUELOC                                                  *C
C*====================                                                *C
C*                                                                    *C
C*  1) Purpose                                                        *C
C*       Tells when error happened in reduction job.                  *C
C*  2) Relation                                                       *C
C*       Called by;                                                   *C
C*           UERLOC : locates error.                                  *C
C*       Calls;                                                       *C
C*           None.                                                    *C
C*  4) Update record                                                  *C
C*       09/14/85  K.Fujii  Original version.                         *C
C*       10/28/85  A. Miyamoto  Dummy routine.                        *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================
      SUBROUTINE RUELOC
C =========================
C  
      WRITE(6,100)
100   FORMAT(/1X,'>>>>>>>>>>>>>> ',8X,'Error in    RUELOC >>>>>>>>>>>',
     .           '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
      PRINT *,' ** Dummy RUELOC in T#PB.FORTLIB.LOAD ha been called. **'
      PRINT *,' ** Prepare correct subroutine RUELOC  *****************'
      STOP
C  
      END
