C**********************************************************************
C*
C* ---------------------------------------
C* Subroutine MAINIT(NTKEY, NVKEY, MAXVRS )
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
      SUBROUTINE MAINIT(NTKEY, NVKEY, MAXVRS)
C
#include "macntl.inc"
#include "mames.inc"
C
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
#ifdef  UNIX
       IF( KEY(1:1).EQ.' ') THEN
#endif
#ifdef  UNIX
         PRINT *,'MAINIT .. This routine is not supported yet.'
#endif
#ifdef  UNIX
        STOP
#endif
#ifdef  UNIX
       ENDIF
#endif
      CALL UIDATE(IBUF(1))
#ifdef  MSP
      CALL   TIME(IBUF(2))
#endif
#ifdef  UNIX
      CALL  UITIME(IBUF(2))
#endif
      READ(CMxJBN,'(4A4)') (IBUF(K1),K1=3,6)
      IBUF(7) = NTKEY
      IBUF(8) = NVKEY
      IBUF(9) = MAXVRS
      MCxKEY  = NTKEY
      MCxKYL  = NVKEY
      MCxVSM  = MAXVRS
      NW      = 9
      WRITE(MCxDBS,ERR=110) KEY(:MCxKEY),NW,(IBUF(K1),K1=1,NW)
      GO TO 200
110   CONTINUE
#ifdef  MSP
      REWRITE(MCxDBS,ERR=910) KEY(:MCxKEY),NW,(IBUF(K1),K1=1,NW)
#endif
      CALL MAMSG(1, 1,
     > ' MAINIT..Data base header record over-written.')
C
C ----------------------------------------------------------------------
C (2) Write delimiter.
C ----------------------------------------------------------------------
C
200   CONTINUE
      NW      = 6
      WRITE(MCxDBS,ERR=210) CMxDLM(:MCxKEY),NW, (IBUF(K1),K1=1,NW)
      GO TO 300
210   CONTINUE
#ifdef  MSP
      REWRITE(MCxDBS,ERR=910)CMxDLM(:MCxKEY),NW,(IBUF(K1),K1=1,NW)
#endif
      CALL MAMSG(1, 1,
     > ' MAINIT..Data base Trailer record over-written.')
C
300   CONTINUE
      MESAGE(1) = 'Data base file is initialized.'
      CALL MAxERR(  0,'MAINIT',1)
      RETURN
C
C ----------------------------------------------------------------------
C (9) Error.
C ----------------------------------------------------------------------
C
910   CONTINUE
      CALL MACLOS
      MESAGE(1) = 'Failed to initialize data base file.'
      CALL MAxERR( 800, 'MAINIT', 1)
      STOP
      END
