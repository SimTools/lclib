CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine PUELOC                                                  *C
C*====================                                                *C
C*                                                                    *C
C*  1) Purpose                                                        *C
C*       Tells when error happened in production job.                 *C
C*  2) Relation                                                       *C
C*       Called by;                                                   *C
C*           UERLOC : locates error.                                  *C
C*       Calls;                                                       *C
C*           None.                                                    *C
C*  4) Update record                                                  *C
C*       09/14/85  K.Fujii  Original version.                         *C
C*                                                                    *C
CC********************************************************************CC
C
C =========================
      SUBROUTINE PUELOC
C =========================
C
#include "prhead.inc"
#include "prjctl.inc"
C
C -------------------------------------------------------------------- C
C     Write out event informations to help debugging.                  C
C -------------------------------------------------------------------- C
C
      WRITE(6,610) xEXPNO,xRUNNO,xEVTNO,IEVSEQ, xRECTP
 610  FORMAT( 17X,'  NEXPNO  NRUNNO  NEVTNO  NEVSEQ  REC.TYPE   ',
     .       /17X,                     5I8                       )
C
      RETURN
      END
