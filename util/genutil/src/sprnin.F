CC********************************************************************CC
C*                                                                    *C
C*=====================                                               *C
C* Subroutine SPRNIN(LEVEL, IDEBUG)                                   *C
C*=====================                                               *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Run initialization module for SPRING.                            *C
C*   Store some information.                                          *C
C*                                                                    *C
C*(Author)                                                            *C
C*   A. Miyamoto   9-Feb-1990  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE SPRNIN( LEVEL, IDEBUG )
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
C     Fill 'Spring'.                                                C
C       IDATA(1)   = DATE ( YYMMDD )                                   C
C            (2)   = TIME in msec from 0:00                            C
C            (3-6) = program name                                      C
C            (7)   = version number                                    C
C -------------------------------------------------------------------- C
C
      CALL TBCRTE(2,'Spring',0,0,IRET)
      IF ( IRET .LT. 0 ) THEN
        PRINT *,' SPRNIN failed to TBCRTE Spring',IRET
        STOP
      ENDIF
C
C --- Job ID informations ( Element #1 ).
C
      CALL UIDATE(IDATE)
cc      CALL TIME  (ITIME)
      CALL uiTIME  (ITIME)
      IDATA (1) = IDATE
      IDATA (2) = ITIME
      GNNAME    = 'Spring'
      IDATA (7) = 0101
C
      CALL TBPUT(2,'Spring',1, 7,IDATA,IRET)
      IF ( IRET .LT. 0 ) THEN
        PRINT *,' FILBGR failed to TBPUT Generator Elm#1 ',IRET
        STOP
      ENDIF
C
C ----------------------------------------------------------------
C (3) Does SPRING Initialization.
C ----------------------------------------------------------------
C
      CALL SPBEGN
C
      RETURN
      END
