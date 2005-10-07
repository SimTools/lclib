C***********************************************************************
C*                                                                     *
C*  -----------------------------------------====                      *
C*  Subroutine SPCALL( IDREC, LEVEL, IDEBUG, NRET )                    *
C*  -----------------------------------------====                      *
C*                                                                     *
C*(Function)                                                           *
C*  Event generation by SPRING.  called from FULL steering part.       *
C*                                                                     *
C*(Input)                                                              *
C*  IDREC ; record ID, use only ID=1                                   *
C*  LEVEL ; not used.                                                  *
C*  IDEBUG; not used.                                                  *
C*(Output)                                                             *
C*  NRET  ; Return code.                                               *
C*                                                                     *
C*(Author)                                                             *
C*  A.Miyamoto   9-Feb-1990  Original version.                         *
C*               9-Apr-1992  Add NRET argument to CALL SPEVNT          *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE SPCALL( IDREC, LEVEL, IDEBUG, NRET )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /SPPARM/ NADSPF, MXTRY, NEVENT
C
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
 
C
      NRET = 0
      IF( IDREC .NE. 1 ) RETURN
C
C
C  -----------------------------------------------------
C                Event generation
C  -----------------------------------------------------
C
#ifdef TAYM
        DO 1000 NEVENT = 1, MXEVNT
#endif 
              NEVENT = NEVENT + 1
C
  500        CALL BSTIME( TIME1, 1 )
C
#ifdef TAYM
             CALL SPRING( FUNC, MXTRY, IRET)
#endif 
#if !defined(__GFORTRAN__)
             CALL SPRING( %VAL(NADSPF), MXTRY, IRET)
#else
             CALL SPRINGwrapper(IRET)
#endif
C
             CALL BSTIME( TIME2, 1 )
C
             DTIME   = TIME2 - TIME1
             CTIME   = CTIME + DTIME
             XTIME   = DTIME* 1.1
             IF( XTIME .GT. UTIME ) UTIME = XTIME
#ifdef TAYM
             IF( (SETIME-TIME2) .LE. UTIME )  GO TO 2000
#endif 
 
             RET    = IRET
 
             CALL XHFILL( -10, RET, 1.0D0)
 
             IF( IRET .GT. 0 .AND. IRET .LE. MXTRY ) THEN
               CALL SPEVNT(NRET)
             ENDIF
 
             CALL SHUPDT
C            CALL SHCLER
 
             IF( IRET .GT. MXTRY ) GO TO 500
C
 1000   CONTINUE
CTAYM
C
      RETURN
      END
