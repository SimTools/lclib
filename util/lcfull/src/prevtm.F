CC********************************************************************CC
C*                                                                    *C
C*===================----===                                          *C
C* Subroutine PREVTM(IDREC, LEVEL, IDEBUG, NRET)                      *C
C*===================----===                                          *C
C*  1) Purpose                                                        *C
C*       Event termination process for the production job.            *C
C*  2) Input                                                          *C
C*      IDREC  : Record ID                                            *C
C*      LEVEL  : Level.
C*      IDEBUG : Debug flag.                                          *C
C*  3) Relation                                                       *C
C*       Called by FULL system                                        *C
C*  4) Update Record                                                  *C
C*      06/05/87  A. Miyamoto  Original version.                      *C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----===
      SUBROUTINE PREVTM(IDREC, LEVEL, IDEBUG, NRET)
C ======================----===
C
#include "pretyp.inc"
#include "prdtyp.inc"
#include "prjctl.inc"
C
      PARAMETER        (IWKSIZ = 200)
      COMMON /SSWORK/   NEARY(200), IBUFF(IWKSIZ)
C
C =======< Entry Point >===============================================
C
C --------------------------------------------------------------------
C     Reset return flag.
C --------------------------------------------------------------------
C
      NRET = 0
      IF( IDREC .NE. 1 ) RETURN
      LEVEL = LEVEL
      IDEBUG = IDEBUG
C
C --------------------------------------------------------------------
C (1) Set error flag of this event.
C --------------------------------------------------------------------
C
      CALL PRWERR( IDREC, IERFLG )
C
C --------------------------------------------------------------------
C (2) Update Production:Header bank.
C --------------------------------------------------------------------
C
      IELM = NDTYPE(9)
      CALL TBGET(1, 'Production:Header', IELM, NW, IBUFF, IRET)
C
      IBUFF(4) = IERFLG
      IBUFF(5) = IDFILT
      IBUFF(6) = IDEVNT
C
      CALL TBPUT(1, 'Production:Header', IELM, NW, IBUFF, IRET)
C
      RETURN
      END
