CC********************************************************************CC
C*                                                                    *C
C*=====================                                               *C
C* Subroutine BS51RI(LEVEL, IDEBUG)                                   *C
C*=====================                                               *C
C*                                                                    *C
C*(Function)                                                          *C
C*   Run initialization module for BASES V5.1.                        *C
C*   Does (1) Store Run information. 
C*        (2) BASES Integration, SAVE results, or READ BASES data.
C*                                                                    *C
C*(Author)                                                            *C
C*   A. Miyamoto   18-Oct-1994  Original version.
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE BS51RI( LEVEL, IDEBUG )
C ==========================
C
      IMPLICIT NONE
      INTEGER*4 LEVEL, IDEBUG
      INTEGER*4 IRET, LNBLNK
      INCLUDE 'bs51pr.inc'
#include "prgenf.inc"
      EXTERNAL FUNC, BS51DT
C
C=======< Entry Point >================================================
C
C
C -------------------------------------------------------------------- 
C (1) Store FULL information into the banks
C -------------------------------------------------------------------- 
C
      CALL BS51RS

C
C -------------------------------------------------------------------- 
C (2) Initialization for BASES
C -------------------------------------------------------------------- 
C

      CALL BSINIT

      CALL USERIN

C
C -------------------------------------------------------------------- 
C (3) Does BASES
C -------------------------------------------------------------------- 
C
      IF( NGETBS .EQ. 0 ) THEN
	IF(NBSINF.EQ.1)
     >  PRINT *,'BS51RI..Bases Integration has started.'
        CALL BASES( FUNC, ESTIM, SIGMA, CTIME, IT1, IT2 )
	IF(NBSINF.EQ.1) THEN
          PRINT *,'BS51RI..End of BASES.'
	  PRINT *,'    ESTIM =',ESTIM,' +- ', SIGMA
	  PRINT *,'    CTIME =',CTIME
	  PRINT *,'    IT1   =',IT1,' IT2=',IT2
	ENDIF
	IF( NPRINF .EQ. 1 ) CALL BSINFO(LUINFO)
	IF( NPRHST .EQ. 1 ) CALL BHPLOT(LUINFO)
	CALL USROUT
C
C .. Get BASES information from TBS bank.
      ELSEIF( NGETBS .EQ. 1 ) THEN 
	IF(NBSINF.EQ.1)
     >  PRINT *,'BS51RI..Bases Integration info. is obtained',
     >          ' from TBS bank.'
	CALL BS51RB(IRET) 
        IF(IRET.NE.0) THEN
          PRINT *,'BS51RI..Error to get BASES information from ',
     >    'a TBS bank.'
          STOP
        ENDIF  
	NUMGEN = NOSPEV
        IUMGEN = 1 
	PRINT *,' NUMGEN =',NUMGEN,' IUMGEN=',IUMGEN
C
C .. Get BASES information from BASES 5.1 Standard file.
      ELSEIF( NGETBS.EQ. 2 ) THEN
	IF(NBSINF.EQ.1)
     >  PRINT *,'BS51RI..Bases Integration info. is obtained',
     >          ' from a file ',BSINF(:LNBLNK(BSINF))
	OPEN(NUBS, FILE=BSINF,STATUS='OLD',FORM='UNFORMATTED')
	CALL BSREAD( NUBS )
	CLOSE( NUBS )

      ELSE
	PRINT *,'Invalid parameter NGETBS=',NGETBS,' in Sub. BS51RI'
	PRINT *,'NGETBS should be 0, 1, or 3.'
	STOP
      ENDIF

C
C -------------------------------------------------------------------- 
C (3) Write Integration results
C -------------------------------------------------------------------- 
C
      IF( NPUTBS .EQ. 1 ) THEN
  	CALL BS51WB(IRET)
        IF(IRET.NE.0) THEN
          PRINT *,'BS51RI..Error to write BASES information to ',
     >    'a TBS bank.'
          STOP
        ENDIF   
	IF(NBSINF.EQ.1)
     >    PRINT *,'BS51RI..Bases Integration info. is written',
     >          ' to the TBS bank.'

      ELSEIF( NPUTBS .EQ. 2 ) THEN
	OPEN(NUBS, FILE=BSOUTF,STATUS='NEW',FORM='UNFORMATTED')
	CALL BSWRIT( NUBS )
        CLOSE( NUBS )
      ELSEIF( NPUTBS .NE. 0 ) THEN
	PRINT *,'Invalid parameter NPUTBS=',NPUTBS,' in Sub. BS51RI'
	PRINT *,'NPUTBS should be 0, 1, or 3.'
	STOP
      ENDIF


C
C -------------------------------------------------------------------- 
C (4) Initialization for SPRING
C -------------------------------------------------------------------- 
C
      IF( NDOSPR.EQ.1 ) THEN
	CALL SPINIT
      ENDIF


      RETURN
      END
