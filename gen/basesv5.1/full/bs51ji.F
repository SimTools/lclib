CC*********************************************************************C
C*                                                                     *
C*==================                                                   *
C* Subroutine BS51JI                                                   *
C*==================                                                   *
C*  (Purpose)                                                          *
C*       Initialization for FULL BASES V5.1                            *
C*                                                                     *
C*  (Input & Output )                                                  *
C*       none.
C*  (Relation)                                                         *
C*       Called from FLANLI
C*                                                                     *
C*  (Author)                                                           *
C*       A. Miyamoto  19-Oct-1994  For FULL system
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE BS51JI
C
      IMPLICIT NONE
      INCLUDE 'bs51pr.inc'
#include "prgenf.inc"

C---------------------------------------------------------------------
C  Write version #.
C---------------------------------------------------------------------
      CALL PRPVRS( 'BASES_V5.1', 101, 941019)

C
C ... Remeber required number of events to generate.
      IF( NGETBS.EQ.1 .AND. NDOSPR.EQ.1 ) THEN
        NOSPEV = NUMGEN
        NUMGEN = -NUMGEN
      ENDIF
      RETURN
      END
