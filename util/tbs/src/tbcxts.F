C**********************************************************************
C*
C* -----------------
C* Subroutine TBCxTS
C* -----------------
C*
C*(Function)
C*   Write Time stamp data of the data modification on the data base.
C*
C*(Author)
C*
C*  A. Miyamoto.  18-Mar-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE TBCxTS
C
#include "tbcctl.inc"
#include "tbsmes.inc"
C
#ifdef UNIX
	return
#endif
#ifdef MSP
      INTEGER*4    IBUF(6)
C
C
C============< Entry Point >============================================
C
C-----------------------------------------------------------------------
C (1) Write time stamp.
C-----------------------------------------------------------------------
C
      CALL UIDATE(IBUF(1))
#if defined(G77_521_OR_GREATER)
      IBUF(2) = TIME()
#else
      CALL   TIME(IBUF(2))
#endif
      READ(CMxJBN,'(4A4)') (IBUF(K1),K1=3,6)
      NW      = 6
      REWRITE(MCxDBS,ERR=910) CMxDLM(:MCxKEY),NW, (IBUF(K1),K1=1,NW)
      RETURN
C
910   CONTINUE
      MESAGE(1) = 'Failed to write time stamp data.'
      CALL TBxERR(800, 'TBCxTS', 1)
      STOP
#endif
      END
