C***********************************************************************
C*                                                                     *
C*========================================                             *
C* Subroutine TBxEEX( IPSUB, NOEL, IRET )                              *
C*========================================                             *
C*(Purpose)                                                            *
C*   To extend the number of banks in the IPSUB-th DSB to NOBNK.       *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB which is to be extended.            *
C*   NOEL     : Number of bank elements (New)                          *
C*(Output)                                                             *
C*   IRET  >0 : Normal return.                                         *
C*   IRET  <0 : Number of bank elements could not be extended.         *
C*(Author)                                                             *
C*       S.Kawabata     Jan. 6 '86                                     *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBxEEX( IPSUB, NOEL, IRET )
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
 
         IRET  = -1
 
C  Check whether IPSUB is legal or not ?
 
         IF( NOSUB .LT. IPSUB) RETURN
 
C  Check whether NOEL is greater than IDSUB(6,IPSUB) ?
 
         NOEOLD = IDSUB(6,IPSUB)
         INEED  = NOEL - NOEOLD
         IF( INEED .LE. 0 ) THEN
             IRET  = 1
             RETURN
         ENDIF
 
C  Check whether residual number of banks is enough or not ?
 
         ISETOT = 0
         DO 100 I = 1, NOSUB
            ISETOT= ISETOT + IDSUB(6,I)
  100    CONTINUE
         ISEFRE = MAXELM - ISETOT
 
         IF( ISEFRE .LT. INEED ) RETURN
         IRET  = 1
 
C        Update the Bank Tables
 
         DO 200 I = 1, NOSUB
            IDSUB(5,I) = -IDSUB(5,I)
  200    CONTINUE
         IXADD    = IDSUB(5,IPSUB)
         IDSUB(5,IPSUB) = -IXADD
 
         DO 400 M = 1, NOSUB
            MINADD   = IXADD
            IDADD    = IPSUB
            DO 300 I = 1, NOSUB
               IF( MINADD .GE. IDSUB(5,I)) THEN
                   MINADD = IDSUB(5,I)
                   IDADD  = I
               ENDIF
  300       CONTINUE
            IF( MINADD .EQ. IXADD ) GO TO 500
            DO 310 K = IDSUB(2,IDADD)+1,IDSUB(2,IDADD)+IDSUB(4,IDADD)
               KEYBNK(1,K) = KEYBNK(1,K)+INEED
  310       CONTINUE
            IDSUB(5,IDADD) = -MINADD
            DO 350 K = IDSUB(5,IDADD)+IDSUB(6,IDADD),IDSUB(5,IDADD)+1,-1
               IX          = K + INEED
               LINK(1,IX)  = LINK(1,K)
               LINK(2,IX)  = LINK(2,K) + INEED
               LINK(3,IX)  = LINK(3,K)
               NWELM(1,IX) = NWELM(1,K)
               NWELM(2,IX) = NWELM(2,K)
               DO 320 L = 1,3
                  IADTBL(L,IX) = IADTBL(L,K)
  320          CONTINUE
  350       CONTINUE
            IDSUB(5,IDADD) = IDSUB(5,IDADD) + INEED
  400    CONTINUE
 
  500    IDSUB(6,IPSUB) = NOEL
         DO 550 I = IDSUB(5,IPSUB)+IDSUB(7,IPSUB)+1,
     .              IDSUB(5,IPSUB)+IDSUB(6,IPSUB)
            NWELM(1,I)  = -1
            LINK(1,I)   = -1
            LINK(3,I)   = -1
  550    CONTINUE
         DO 600 I = 1,NOSUB
            IF( IDSUB(5,I) .LT. 0 ) THEN
                IDSUB(5,I) = -IDSUB(5,I)
            ENDIF
  600    CONTINUE
 
         NOEEX(IPSUB)  = NOEEX(IPSUB) + 1
 
         WRITE(MESAGE(1),9000)
         WRITE(MESAGE(2),9010) IDSUB(1,IPSUB)
         WRITE(MESAGE(3),9020) NOEOLD, NOEL
 9000    FORMAT(' Maximum No. of Elements was Expanded for')
 9010    FORMAT(' DSB ID =',I7)
 9020    FORMAT(' No. of Elements from',I4,' to',I4,' in I*2 unit.')
         CALL TBxERR( 100,'TBxEEX', 3)
 
         RETURN
         END
