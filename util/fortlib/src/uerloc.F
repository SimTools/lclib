CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine UERLOC                                                  *C
C*====================                                                *C
C*                                                                    *C
C*  1) Purpose                                                        *C
C*       Tells when error happened.                                   *C
C*  2) Relation                                                       *C
C*       Called by;                                                   *C
C*           UERROR0: TOPAZ standard error handling routine.          *C
C*       Calls;                                                       *C
C*           SUELOC : locates error in simulation job.                *C
C*           RUELOC : locates error in reduction  job.                *C
C*           PUELOC : locates error in production job.                *C
C*  4) Update record                                                  *C
C*       09/14/85  K.Fujii  Original version.                         *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UERLOC
   
#include "ujbtyp.inc"
   
C--
C  Branch on job type.
C--
      JOBTYP = IABS(JBTYP)
      IF ( JOBTYP .EQ. 1 ) THEN
         CALL SUELOC
      ELSE IF ( JOBTYP .EQ. 2 ) THEN
         CALL RUELOC
      ELSE IF ( JOBTYP .EQ. 3 ) THEN
         CALL PUELOC
      ELSE
      ENDIF
   
      RETURN
      END
