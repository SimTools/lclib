CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C* Subroutine FLANLI                                                  *C
C*=================                                                   *C
C*    For e+e- ->W+W-                                                 *C
C*                                                                    *C
C*(Purpose)                                                           *C
C*    Define analysis program.                                        *C
C*(Relation)                                                          *C
C*    Called by PRMAIN.
C*(Author)                                                            *C
C*   17-May-1990  A. Miyamoto  For Full system                        *C
C*                                                                    *C
CC********************************************************************CC
C
      SUBROUTINE FLANLI
C
#include "fldscr.inc"
C
C======< ENTRY POINT >==================================================
C
C ----------------------------------------------------------------------
C (1) Declare version number of Full module.
C ----------------------------------------------------------------------
C
      CALL PRPDSC( 'e+e-->W+W-', 101, 940928)
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
C                     Define BASES V5.1 generator.
      CALL DSBS51

C                     Define Event scale down.
CC      CALL DSWWSD
C                     Define Lund Hadronizer.
      CALL DSLHAD
C                     List Generator:Particle_List bank
      CALL DSGELS
C                     Define Swimmer  routine.
      CALL DSCSWM
C                     Define Smearing routine.
      CALL DSCSMR
C                     Define User routine.
      CALL DSCUSR
 
      RETURN
      END
