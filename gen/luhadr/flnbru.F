C
C----------
C FLNBRU
C----------
C
      SUBROUTINE FLNBRU ( LEVEL, DEBUG )
C
C ARGUMENTS
C
      INTEGER * 4      LEVEL, DEBUG
C
C NOTE
C
C ---------------------------------------------- C
C     Fill 'Data_Type'.                          C
C       IDATA(1)   = (1,2) <--> (Real,MC)        C
C            (2)   = (0,1,2,3) <--> (G,ON,OFF,C) C
C            (3)   = 1 for generator data        C
C            (4-9) = 0 for generator data        C
C ---------------------------------------------- C
C
C ---------------------------------------------- C
C     Fill 'Generator'.                          C
C       IDATA(1)   = DATE ( YYMMDD )             C
C            (2)   = TIME in msec from 0:00      C
C            (3-6) = program name                C
C            (7)   = version number              C
C ---------------------------------------------- C
C
C COMMON ( BLOCK DATA )
C
#ifndef HIUXF77
      EXTERNAL         LUDATA
#endif
C VARIABLES
C
      CHARACTER*16 PGNAME, GNNAME
      DATA PGNAME / 'Lund V7.3' /, IVERS / 2 /
C
      DIMENSION    IDATA(9),         JDATA(8),ZDATA(8)
      EQUIVALENCE (IDATA(3),GNNAME),(JDATA(1),ZDATA(1))
C
      INTEGER * 4      IENV
C
C MAIN
C
      CALL FULENV ( IENV )
      IF ( IENV .NE. 1 ) RETURN
C
      PRINT *, 'Lund 7.3 Generator - Creating Begin_Run Record.'
C
C     CALL TBINIT(2,IRET)
      CALL TBCRTE(2,'Data_Type',0,0,IRET)
#ifdef TAYM900406
      IDATA(1) = 2
#endif 
#ifdef TYAM900406
      IDATA(2) = 0
#endif 
#ifdef TAYM900406
      IDATA(3) = 1
#endif 
#ifdef TAYM900406
      DO 10 I=4,9
#endif 
#ifdef TAYM900406
        IDATA(I) = 0
#endif 
#ifdef TAYM900406
 10   CONTINUE
#endif 
CTAYM900406
C        Modified so as not to over-write the bank Data_Type
C        if it exists.
C
       IF( IRET .LE. 0 ) THEN
         CALL TBGET(2,'Data_Type',1,NW,IDATA,IRET)
       ELSE
         DO 10 I = 3,9
           IDATA(I) = 0
10       CONTINUE
       ENDIF
       IDATA(1) = 2
       IDATA(2) = 0
       IDATA(3) = IDATA(3) + 1
CTAYM900406
      CALL TBPUT(2,'Data_Type', 1,9,IDATA,IRET)
C
      CALL TBCRTE(2,'Generator',0,0,IRET)
      CALL UIDATE(IDATE)
      CALL UITIME( ITIME )
      IDATA (1) = IDATE
      IDATA (2) = ITIME
      GNNAME    = PGNAME
      IDATA (7) = IVERS
      CALL TBPUT(2,'Generator',1, 7,IDATA,IRET)
C
      RETURN
      END
