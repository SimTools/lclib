C***********************************************************************
C*
C*  ------------------
C*  Subroutine  SPBEGN
C*  ------------------
C*(Function)
C*   Does Initialization of SPRING job.
C*
C*(Author)
C*   A.miyamoto  9-Feb-1990   Select required part from SPMAIN
C*                                                                     *
C***********************************************************************
C
#ifdef TAYM
      SUBROUTINE SPMAIN(FUNC, MXTRY )
#endif 
C
      SUBROUTINE SPBEGN
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /SPPARM/ NADSPF, MXTRY, NEVENT
C
#ifdef TAYM
      EXTERNAL FUNC
#endif 
      REAL*4   SETIME,TIME1,TIME2,UTIME,XTIME,DTIME,CTIME
      COMMON /SPTIME/ SETIME,TIME1,TIME2,UTIME,XTIME,DTIME,CTIME
      COMMON /BASE0/ NDUM(4),IBASES
      COMMON/LOOP1/ LOOP,MXLOOP
      COMMON/LOOP0/ LOOPC,IRCODE
      COMMON/XHCNTL/ LOCK
 
      IBASES = 0
C  -----------------------------------------------------
C      Initialization for SPRING and User function F
C  -----------------------------------------------------
 
       CALL BSTIME( CTIME, 0 )
 
       UTIME   = 0.0
       CTIME   = 0.0
C
#ifdef TAYM
       READ( 5, * ) MXEVNT
#endif 
C
#ifdef TAYM
       READ( 5, * ) SETIME
#endif 
       SETIME = 24.*60.
C
       SETIME = SETIME*60.0 - 5.0
C
C  -----------------------------------------------------
C     Read the probability distribution from disk file
C  -----------------------------------------------------
C
        CALL BSREAD
C
        LOCK = 1
        LOOPC= LOOP
        CALL USERIN
        LOCK = 0
C
        CALL SPINIT
 
        NBIN    = MXTRY
        IF( MXTRY .GT. 50 ) NBIN = 50
        TRYMX   = MXTRY + 1
 
        CALL XHINIT( -10, 1.0D0, TRYMX, NBIN,
     .  '************* Number of trials to get an event *************')
 
       X1     = 0.37856921D0
       X2     = 0.07680295D0
       CALL DRNSET( X1, X2 )
C
       CALL SHRSET
C
       NEVENT = 0
C
       RETURN
       END
