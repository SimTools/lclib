C***********************************************************************
C*                                                                     *
C*========================================                             *
C* Subroutine TBxDEX( IPSUB, NWORD, IRET )                             *
C*========================================                             *
C*(Purpose)                                                            *
C*   To extend the Data Sub Buffer                                     *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB which is to be extended.            *
C*   NWORD    : Number of Words in the DSB (New) in I*4 unit           *
C*(Output)                                                             *
C*   IRET  >0 : Normal return.                                         *
C*   IRET  <0 : DSB could not be extended.                             *
C*(Author)                                                             *
C*       S.Kawabata     Jan. 6 '86                                     *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBxDEX( IPSUB, NWORD, IRET)
 
         IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
 
         IRET  = -1
 
C  Check whether IPSUB is legal or not ?
 
         IF( (IPSUB .LE. 0) .OR. (IPSUB .GT. NOSUB)) THEN
            WRITE(MESAGE(1),9000)
 9000       FORMAT(' Illegal IPSUB ')
            WRITE(MESAGE(2),9005) IPSUB,NOSUB
 9005       FORMAT(' IPSUB =',I3,' should be less than or equal NOSUB ='
     .      ,I3)
            CALL TBxERR( 800,'TBxDEX', 2 )
         ENDIF
 
         HWOLD  = NWSUB(2,IPSUB)
         HWORD  = 2*NWORD
         HNEED  = HWORD - HWOLD
 
C  Check whether residual data space is enough or not ?
 
         IF( NWFREE .LT. HNEED ) RETURN
         NWFREE  = NWFREE - HNEED
 
C  Check whether required space is greater than NWSUB(2,IPSUB) ?
 
         IRET  = 1
         IF( HNEED .LE. 0 ) RETURN
 
C        Update the DSB pointer table NWSUB(1,j)
 
         DO 200 I = 1, NOSUB
            NWSUB(1,I) = -NWSUB(1,I)
  200    CONTINUE
         IXADD    = NWSUB(1,IPSUB)
         NWSUB(1,IPSUB) = -IXADD
 
         DO 400 M = 1, NOSUB
            MINADD   = IXADD
            IDADD    = IPSUB
            DO 300 I = 1, NOSUB
               IF( MINADD .GE. NWSUB(1,I)) THEN
                   MINADD = NWSUB(1,I)
                   IDADD  = I
               ENDIF
  300       CONTINUE
            IF( MINADD .EQ. IXADD ) GO TO 500
            NWSUB(1,IDADD) = -MINADD
 
            NWD      = NWSUB(2,IDADD)
            IAD0     = NWSUB(1,IDADD) + 1
            IAD1     = IAD0 + HNEED
 
            CALL UBCPYH( NWD, IAD0, IAD1, HBUFF )
 
C           NWST     = NWSUB(1,IDADD)+NWSUB(2,IDADD)
C           NWFN     = NWSUB(1,IDADD)+ 1
C           DO 350 K = NWST,NWFN,-1
C              HBUFF(K+HNEED)  = HBUFF(K)
C 350       CONTINUE
            NWSUB(1,IDADD) = NWSUB(1,IDADD) + HNEED
            HINC           = NWSUB(1,IDADD)
            DO 380 I = IDSUB(5,IDADD)+1,IDSUB(5,IDADD)+IDSUB(7,IDADD)
               IF( LINK(3,I) .GE. 0 ) THEN
                   HADD   = IADTBL(1,I) + HINC
                   IF( LINK(3,I) .EQ. 0 ) THEN
                       NPOINT   =  HADD
                   ELSE
                       NPOINT   =  (HADD+1)/2
                   ENDIF
                   CALL UVSET( IADTBL(2,I), NPOINT )
               ENDIF
  380       CONTINUE
  400    CONTINUE
 
  500    NWSUB(2,IPSUB) = HWORD
         DO 600 I = 1,NOSUB
            IF( NWSUB(1,I) .LT. 0 ) THEN
                NWSUB(1,I) = -NWSUB(1,I)
            ENDIF
  600    CONTINUE
 
         NODEX(IPSUB)   = NODEX(IPSUB) + 1
 
         WRITE(MESAGE(1),9010)
 9010    FORMAT(' Size of Data Sub Buffer was Extended for')
         WRITE(MESAGE(2),9015) IDSUB(1,IPSUB)
 9015    FORMAT(' DSB ID =',I7)
         WRITE(MESAGE(3),9020) HWOLD, HWORD
 9020    FORMAT(' Size from',I10,' to',I10,' in I*2 unit.')
         CALL TBxERR( 100,'TBxDEX', 3 )
 
         RETURN
         END
