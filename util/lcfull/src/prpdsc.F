CC*********************************************************************C
C*                                                                     *
C*-----------------------------------------                            *
C* Subroutine PRPDSC( FLNAME, NVERS, NDATE )                           *
C*-----------------------------------------                            *
C*                                                                     *
C*(Purpose)                                                            *
C*    Stack FLANLI's name, version number, and date to the Common.
C*                                                                     *
C*(Input)                                                              *
C*    FLNAME : name of FLANLI       ( Character)
C*    NVERS  : Version number of the module.                           *
C*    VDATE  : Version date.  (yymmdd)
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto  07-May-1987  Original version.                      *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRPDSC( FLNAME, NVERS, NDATE )
C
#include "prjenv.inc"
C
      CHARACTER*(*) FLNAME
 
C==================<<  Entry Point  >>================================
 
C
C ---------------------------------------------------------------------
C (1) Save input to the stack.
C ---------------------------------------------------------------------
C
      DSCNAM = FLNAME
      NDSCVR = NVERS
      NDSCDT = NDATE
C
      RETURN
      END
