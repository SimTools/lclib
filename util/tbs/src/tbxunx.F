C   10/07/89 401271538  MEMBER NAME  TBxUNX   (FORT)     M  FORTRAN
C+
C File : TBxUNX
C Description : Convert bank contents into IEEE format
C
C Author : Kouichi Nagai, TOPAZ, Kobe Univ.
C Date : 10 - JUN - 1993
C-
C
      SUBROUTINE TBxUNX ( IDREC )
C
C ARGUMENTS
C
      INTEGER * 4      IDREC
C
C CONST
C
      CHARACTER * 64   CHKBNK  / 'PRODUCTION:TPC;TRACK_PARAMETER' /
C
C COMMON
C
#include "sswork.inc"
C      INTEGER * 4      MWORK
C      PARAMETER        ( MWORK = 100000 )
      INTEGER * 4      IBUF ( MWORK )
      REAL * 4         RBUF ( MWORK )
      INTEGER * 2      HBUF ( MWORK*2 )
      CHARACTER * 1    CBUF ( MWORK*4 )
      EQUIVALENCE      ( IWORK, IBUF, RBUF, HBUF, CBUF )
C
C VARIABLES
C
      INTEGER * 4      I, J
      INTEGER * 4      IPSUB
      INTEGER * 4      JP
      CHARACTER * 64   BANK, RECNAM, BNKNAM(100)
      INTEGER * 4      NBANK
      INTEGER * 4      NELM, NUMELM ( 1000 )
C
      INTEGER * 4      DEBUG / 0 /
C
C FUNCTIONS
C
      LOGICAL          TBxVFB
C
C MAIN
C
      CALL TBQBNK ( IDREC, RECNAM, NBANK, BNKNAM )
C     PRINT *, '..................................................'
      DO 10 I = 1, NBANK
        BANK = BNKNAM(I)
C       PRINT *,'BANK=',BANK
        IF ( TBxVFB ( IDREC, BANK ) ) THEN
C         IF ( BANK .EQ. CHKBNK ) THEN
C           DEBUG = 1
CC        ELSE
            DEBUG = 0
C         END IF
C       PRINT *, 'DEBUG=',DEBUG
          CALL TBNOEL ( IDREC, BANK, NELM, NUMELM )
          IF ( DEBUG .EQ. 1 )
     >      PRINT *, 'Processing..... Bank : ', IDREC, BANK
          DO 20 J = 1, NELM
            IF ( DEBUG .EQ. 1 )
     >        PRINT *, '          ..... Elm  : ', NUMELM(J)
            CALL TBGET ( IDREC, BANK, NUMELM(J),
     >                   NWDS, IBUF, IRET )
            CALL TBxVCV ( IDREC, BANK, NUMELM(J),
     >                    NWDS, CBUF, HBUF, IBUF, RBUF, DEBUG )
            CALL TBPUT ( IDREC, BANK, NUMELM(J),
     >                   NWDS, IBUF, IRET )
C           PRINT *, '..... Converted : ', BANK
20        CONTINUE
        ELSE
CC        CALL TBDELB ( IDREC, BANK, IRET )
C         PRINT *, '..... deleted : ', BANK
        END IF
10    CONTINUE
CC    CALL TBSQEZ ( IDREC, IRET )
C
      RETURN
      END
