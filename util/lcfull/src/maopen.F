CC*********************************************************************C
C*                                                                     *
C*------------------------
C* SUBROUTINE MAOPEN(NRET)
C*------------------------
C*                                                                     *
C*(Purpose)                                                            *
C*   Set default parameters, then open data base file.
C*                                                                     *
C*(Input)                                                              *
C*   none.
C*                                                                     *
C*(Output)                                                             *
C*   NRET = 1 ; normal return
C*        =-1 ; Can not open data base with write mode.                *
C*        =-2 ; When user not authorized to open write mode.           *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  13-Mar-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAOPEN(NRET)
C
#include "macntl.inc"
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Initialize.
C ----------------------------------------------------------------------
C
#ifndef HIUXF77 
#ifdef  MACHTEN_PPC
#else
      CALL GETUID(CMxUID)
#endif
#else
      call getenv("USER", 4, CMxUID, 8)
#endif
      CMxJBN(1:8) = CMxUID
C
C ----------------------------------------------------------------------
C (2) Open data base file.
C ----------------------------------------------------------------------
C
      CALL MAxOPN(NRET)
 
      RETURN
      END
