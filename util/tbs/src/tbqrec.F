C*
C*********************************************************************
C*
C* ---------------------------=======
C* Subroutine TBQREC( DSBNAM, IDDSB )
C* ---------------------------=======
C*
C*(Function)
C*   Query Record ID for given Record name
C*
C*(Input)
C*   DSBNAM ...   Upto 64 character of Record name
C*
C*(Output)
C*   IDDSB  ...   DSB ID, if record name not exists, IDDSB = -1
C*
C*(Author)
C*   A. Miyamoto    13-Feb-1985
C*   S. Kawabata    29-Nov-1985      V4.0
C*
C*********************************************************************
C*
      SUBROUTINE TBQREC( DSBNAM, IDDSB )
C
#include "tbsbnk.inc"
#include "tbsmes.inc"
C
      CHARACTER*(*) DSBNAM
      CHARACTER     TNAME*64
C ==================================================================
C  1. Check bank_name length, and convert to upper case character.
C ==================================================================
      LDSB    = LEN(DSBNAM)
      IF(LDSB.GT.64) THEN
        WRITE(MESAGE(1),8991)
 8991   FORMAT(' The following record name is too long.')
        WRITE(MESAGE(1),8992) DSBNAM(1:64)
 8992   FORMAT(' Trancated name = ',A64)
        CALL TBxERR(320,'TBQREC', 2)
        LDSB = 64
      END IF
 
      TNAME  = DSBNAM
      CALL UCUPER(LDSB, TNAME)
 
C ==================================================================
C  2. Search for the DSB name match with input.
C ==================================================================
 
      CALL TBxCRN( TNAME, IASH, IPSUB )
 
      IF( IPSUB .GT. 0 ) THEN
          IDDSB   = IDSUB(1,IPSUB)
          IF( IASH .GT. 0 ) THEN
              ISSH  = IDSUB(10,IPSUB)
              IDDSB = IDSHR( IASH, ISSH)
          ENDIF
      ELSE
          IDDSB = -1
      ENDIF
 
      RETURN
      END
