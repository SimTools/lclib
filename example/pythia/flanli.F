CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C* Subroutine FLANLI                                                  *C
C*=================                                                   *C
C*(Purpose)                                                           *C
C*    Define analysis program for standard production job.            *C
C*(Relation)                                                          *C
C*    Called by PRMAIN.
C*(Author)                                                            *C
C*   25-Apr-1987  A. Miyamoto  For Full system                        *C
C*                                                                    *C
CC********************************************************************CC
C
      SUBROUTINE FLANLI
C
#include "fldscr.inc"
C
C
C======< ENTRY POINT >==================================================
C
C ----------------------------------------------------------------------
C (1) Declare version number of Full module.
C ----------------------------------------------------------------------
C
      CALL PRPDSC( 'PYTHIA55 ', 101, 921020)
C
C ----------------------------------------------------------------------
C (2) Reset counters.
C ----------------------------------------------------------------------
C
      NPROG = 0
      NENTR = 0
      NHIST = 0
C
C ----------------------------------------------------------------------
C (3) Define analysis routine and calling sequence.
C ----------------------------------------------------------------------
C
C
C                     Define simulator module.
      CALL DSGPYT
C
      CALL DSGELS

      CALL DSCSWM

      CALL DSCSMR
C                     Define user analysis.
      CALL DSCUSR
C
C
      RETURN
      END
