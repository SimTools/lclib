C**********************************************************************
C*
C* ---------------------------------------
C* Subroutine TBCINI(NTKEY, NVKEY, MAXVRS )
C* ---------------------------------------
C*
C*(Function)
C*   Initialize data base file, This program ends, when failed to
C*   initialize data base.  This routine mus be called prior to any
C*   access to the data base.
C*
C*(Input)
C*   NTKEY  : Total Key length
C*   NVKEY  : Valid Key length.
C*   MAXVRS : Max version #.
C*
C*(Output)
C*   none.
C*
C*(Format of Header record.)
C*   KEY = '            ' (blank)
C*       (1) = Data base initialized date.
C*       (2) = Data base initialized time
C*     (3:6) = Data base iniitalized job name.
C*       (7) = Total Key length.
C*       (8) = Valid key length.
C*       (9) = Max version #.
C*
C*(Format of EOF record.)
C*   KEY = '999999999999999999999'
C*   IBUF(1) = Last write access date.
C*       (2) = Last write access time
C*     (3:6) = Last write access job name.
C*
C*
C*(Author)
C*
C*  A. Miyamoto.  18-Mar-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE TBCINI(NTKEY, NVKEY, MAXVRS)
C
#include "tbcctl.inc"
#include "tbsmes.inc"
C
#ifdef UNIX
	print *,'Dummy subroutine TBCINI was called.'
	print *,'You don''t need to use TBCINI on UNIX system'
	return
#endif
#ifdef MSP
      CHARACTER*64  KEY
      INTEGER*4     IBUF(15)
C
C============< Entry Point >============================================
C
C ----------------------------------------------------------------------
C (1) Create Header record.
C ----------------------------------------------------------------------
C
      KEY = '   '
      CALL UIDATE(IBUF(1))
#if defined(G77_521_OR_GREATER)
      IBUF(2) = TIME()
#else
      CALL   TIME(IBUF(2))
#endif
      READ(CMxJBN,'(4A4)') (IBUF(K1),K1=3,6)
      IBUF(7) = NTKEY
      IBUF(8) = NVKEY
      IBUF(9) = MAXVRS
      MCxKEY  = NTKEY
      MCxKYL  = NVKEY
      MCxVSM  = MAXVRS
      NW      = 9
      WRITE(MCxDBS,ERR=910) KEY(:MCxKEY),NW,(IBUF(K1),K1=1,NW)
C
C ----------------------------------------------------------------------
C (2) Write delimiter.
C ----------------------------------------------------------------------
C
      NW      = 6
      WRITE(MCxDBS,ERR=910) CMxDLM(:MCxKEY),NW, (IBUF(K1),K1=1,NW)
C
      MESAGE(1) = 'Data base file is initialized.'
      CALL TBxERR(  0,'TBCINI',1)
      RETURN
C
C ----------------------------------------------------------------------
C (9) Error.
C ----------------------------------------------------------------------
C
910   CONTINUE
      CALL TBCCLS(IRET)
      MESAGE(1) = 'Failed to initialize data base file.'
      CALL TBxERR( 800, 'TBCINI', 1)
      STOP

#endif
      END
