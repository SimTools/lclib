C***********************************************************************
C*                                                                     *
C*========================================                             *
C* Subroutine TBxBEX( IPSUB, NOBNK, IRET )                             *
C*========================================                             *
C*(Purpose)                                                            *
C*   To extend the number of banks in the IPSUB-th DSB to NOBNK.       *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB which is to be extended.            *
C*   NOBNK    : Number of banks (New)                                  *
C*(Output)                                                             *
C*   IRET  >0 : Normal return.                                         *
C*   IRET  <0 : Number of banks could not be extended.                 *
C*(Author)                                                             *
C*       S.Kawabata     Jan. 6 '86                                     *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBxBEX( IPSUB, NOBNK, IRET )
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
 
         IRET  = -1
 
C  Check whether IPSUB is legal or not ?
 
         IF( NOSUB .LT. IPSUB) RETURN
 
C  Check whether NOBNK is greater than IDSUB(3,IPSUB) ?
 
         NOOLD  = IDSUB(3,IPSUB)
         INEED  = NOBNK - NOOLD
         IF( INEED .LE. 0 ) THEN
             IRET  = 1
             RETURN
         ENDIF
 
C  Check whether residual number of banks is enough or not ?
 
         ISBTOT = 0
         DO 100 I = 1, NOSUB
            ISBTOT= ISBTOT + IDSUB(3,I)
  100    CONTINUE
         ISBFRE = MAXBNK - ISBTOT
 
         IF( ISBFRE .LT. INEED ) RETURN
         IRET  = 1
 
C        Update the Bank Tables
 
         DO 200 I = 1, NOSUB
            IDSUB(2,I) = -IDSUB(2,I)
  200    CONTINUE
         IXADD    = IDSUB(2,IPSUB)
 
         DO 400 M = 1, NOSUB
            MINADD   = IXADD
            IDADD    = IPSUB
            DO 300 I = 1, NOSUB
               IF( MINADD .GE. IDSUB(2,I)) THEN
                   MINADD = IDSUB(2,I)
                   IDADD  = I
               ENDIF
  300       CONTINUE
            IF( MINADD .EQ. IXADD ) GO TO 500
            IDSUB(2,IDADD) = -MINADD
            DO 350 K = IDSUB(2,IDADD)+IDSUB(4,IDADD),IDSUB(2,IDADD)+1,-1
               IX    = K + INEED
               ISORT(IX) = ISORT(K) + INEED
               BNKNAM(IX) = BNKNAM(K)
               DO 320 L = 1,2
                  NWBNK(L,IX)  = NWBNK(L,K)
                  KEYBNK(L,IX) = KEYBNK(L,K)
  320          CONTINUE
  350       CONTINUE
            IDSUB(2,IDADD) = IDSUB(2,IDADD) + INEED
  400    CONTINUE
 
  500    IDSUB(3,IPSUB) = NOBNK
         DO 600 I = 1,NOSUB
            IF( IDSUB(2,I) .LT. 0 ) THEN
                IDSUB(2,I) = -IDSUB(2,I)
            ENDIF
  600    CONTINUE
 
         NOBEX(IPSUB) = NOBEX(IPSUB) + 1
 
         WRITE(MESAGE(1),9000)
 9000    FORMAT(' Maximum No. of Banks was Expanded for')
         WRITE(MESAGE(2),9005) IDSUB(1,IPSUB)
 9005    FORMAT(' DSB ID =',I7)
         WRITE(MESAGE(3),9010) NOOLD,NOBNK
 9010    FORMAT(' No. of Banks from',I4,' to',I4,' in I*2 unit.')
         CALL TBxERR(100,'TBxBEX', 3)
 
         RETURN
         END
