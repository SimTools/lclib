CC********************************************************************CC
C*                                                                    *C
C*==================                                                  *C
C* Subroutine BS51RS                                                  *C
C*==================                                                  *C
C*                                                                    *C
C*(Function) 
C*   Store Run information to the Begin_Run record.
C*                                                                    *C
C*(Author)                                                            *C
C*   A. Miyamoto   18-Oct-1994  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE BS51RS
C ==========================
C
      DIMENSION    IDATA(9)
      CHARACTER*16 GNNAME
      EQUIVALENCE (IDATA(3),GNNAME)
C
C=======< Entry Point >================================================
C
C
C -------------------------------------------------------------------- C
C     Fill 'Data_Type'.                                                C
C       IDATA(1)   = (1,2) <--> (Real,MC)                              C
C            (2)   = (0,1,2,3) <--> (G,ON,OFF,C)                       C
C            (3)   = 1 for generator data                              C
C            (4-9) = 0 for generator data                              C
C -------------------------------------------------------------------- C
C
#ifdef TAYM
      CALL TBCRTE(2,'Data_Type',0,0,IRET)
#endif 
#ifdef TAYM
      IF ( IRET .LT. 0 ) THEN
#endif 
#ifdef TAYM
        PRINT *,' FILBGR failed to TBCRTE Data_Type ',IRET
#endif 
#ifdef TAYM
        STOP
#endif 
#ifdef TAYM
      ENDIF
#endif 
C
      CALL TBGET(2,'Data_Type',1, NW, IDATA, IRET )
      IDATA(1) = 2
      IDATA(2) = 0
C
      IDATA(3) = 1
#ifdef TAYM
      DO 10 I=4,9
#endif 
#ifdef TAYM
        IDATA(I) = 0
#endif 
#ifdef TAYM
 10   CONTINUE
#endif 
C
      CALL TBPUT(2,'Data_Type', 1,9,IDATA,IRET)
      IF ( IRET .LT. 0 ) THEN
        PRINT *,' FILBGR failed to TBPUT Data_Type ',IRET
        STOP
      ENDIF
C
C -------------------------------------------------------------------- C
C     Fill 'BASES'.                                                    C
C       IDATA(1)   = DATE ( YYMMDD )                                   C
C            (2)   = TIME in msec from 0:00                            C
C            (3-6) = program name                                      C
C            (7)   = version number                                    C
C -------------------------------------------------------------------- C
C
      CALL TBCRTE(2,'SPRING',0,0,IRET)
C      IF ( IRET .LT. 0 ) THEN
C        PRINT *,' SPRNIN failed to TBCRTE Spring',IRET
C        STOP
C      ENDIF
C
C --- Job ID informations ( Element #1 ).
C
      CALL UIDATE(IDATE)
      CALL TIME  (ITIME)
      IDATA (1) = IDATE
      IDATA (2) = ITIME
      GNNAME    = 'BASES V5.1'
      IDATA (7) = 0101
C
      CALL TBPUT(2,'SPRING',1, 7,IDATA,IRET)
      IF ( IRET .LT. 0 ) THEN
        PRINT *,' BS51RS failed to TBPUT Generator Elm#1 ',IRET
        STOP
      ENDIF
C
C
      RETURN
      END
