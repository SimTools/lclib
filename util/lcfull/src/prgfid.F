CC********************************************************************CC
C*                                                                    *C
C*============================                                        *C
C* Subroutine PRGFID(NDFILT)                                          *C
C*============================                                        *C
C*                                                                    *C
C*(Purpose)                                                           *C
C*   Gives Filter ID to the caller.                                   *C
C*(Input)                                                             *C
C*     None.                                                          *C
C*(Output)                                                            *C
C*     NDFILT  : Filter ID value.                                     *C
C*(Relation)                                                          *C
C*   Called by  user routines.                                        *C
C*(Update Record)                                                     *C
C*   06/13/87  K.Fujii      Original version.                         *C
C*                                                                    *C
CC********************************************************************CC
 
      SUBROUTINE PRGFID(NDFILT)
 
#include "pretyp.inc"
 
C--
C  Filter type.
C--
      NDFILT = IDFILT
 
      RETURN
      END
