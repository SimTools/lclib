C
C----------
C TBxVDT
C----------
C
      INTEGER FUNCTION TBxVDT ( PNTR, NWDS )
C
C ARGUMENTS
C
      INTEGER * 4      PNTR ( 2 )
      INTEGER * 4      NWDS
C
C COMMON
C
#include "tbfmap.inc"
C
C MAIN
C
       MAXLEN = RANGE(PNTR(1),PNTR(2),NRANGE(PNTR(1),PNTR(2)),2)
       IF ( NWDS .LE. MAXLEN ) THEN
         DO 10 I = 1, NRANGE(PNTR(1),PNTR(2))
           IF ( NWDS .GE. RANGE(PNTR(1),PNTR(2),I,1) .AND.
     #          NWDS .LE. RANGE(PNTR(1),PNTR(2),I,2) ) THEN
             TBxVDT = MAPTYP ( PNTR(1), PNTR(2), I )
             RETURN
           END IF
10       CONTINUE
         PRINT *, '%TBWVAX-F, Error in Vax Format File - TBxVDT1'
         STOP
       ELSE
         IPT = MOD ( NWDS, MAXLEN )
         IF ( IPT .EQ. 0 ) IPT = MAXLEN
C        PRINT *, 'Range Over : ', NWDS, ' ---> ', IPT
         DO 20 I = 1, NRANGE(PNTR(1),PNTR(2))
C          PRINT *, 'Trying ..... : ', I, RANGE(PNTR(1),PNTR(2),I,1),
C    #               RANGE(PNTR(1),PNTR(2),I,2)
           IF ( IPT .GE. RANGE(PNTR(1),PNTR(2),I,1) .AND.
     #          IPT .LE. RANGE(PNTR(1),PNTR(2),I,2) ) THEN
             TBxVDT = MAPTYP ( PNTR(1), PNTR(2), I )
             RETURN
           END IF
20       CONTINUE
         PRINT *, '%TBWVAX-F, Error in Vax Format File - TBxVDT2'
         STOP
       END IF
       RETURN
       END
