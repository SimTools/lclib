C  
C     A = 5.0
C     B = 3.0
C     X0 =10.0
C     Y0 = 0.0
C  
C     CALL UDELPS( A, B, X0, Y0, DIST )
C  
C     PRINT *,' DIST =', DIST
C     STOP
C     END
C***********************************************************************
C* 
C*   --------------------------------======
C*   Subroutine UDELPS( A, B, X0, Y0, DIST )
C*   --------------------------------======
C*(Function)
C*   Calculate the distances from (X0,Y0) to the ellipse boundary.
C*(Input)
C*   A,B    : Parameter for the ellipse
C*   X0,Y0  : Point (X0,Y0)
C*(Output)
C*   DIST   : Distance. ( DIST < 0. when outside ellipse
C*                        DIST > 0, when inside ellipse
C*(Usage note)
C*   Equation of the ellipse is
C* 
C*         X*X       Y*Y
C*        -----  +  ----- = 1   ( left < 1 for inside, left > 1 outside)
C*         A*A       B*B
C* 
C*   To solve 4-th order polynomial equation, this program
C*   uses SSL-II librarY.
C* 
C*(Author)
C*   A. Miyamoto   4-Dec-1989  Original version.
C*(Update)
C*   R.Itoh, 28-Dec-1994 Modified to use CERNLIB instead of SSL2 for UNIX
C* 
C***********************************************************************
C* 
      SUBROUTINE UDELPS( A, B, X0, Y0, DIST )
C  
      REAL*8     DA(0:4),AABB,AABB2,AX, COSTH, TIMAG, SINTH
      COMPLEX*16 ZA(4)
      DATA       IFIRST/1/
C  
C ====< ENtry Point >===================================================
C  
C ----------------------------------------------------------------------
C (1) (X0,Y0) is incide or not, and avoid extream case.
C ----------------------------------------------------------------------
C  
      IF( IFIRST .EQ. 1 ) THEN
        xPI = ACOS(-1.0)
        IFIRST = 0
      ENDIF
C  
      OUTIN = 1.0
      XOUT  = (X0*X0)/(A*A) + (Y0*Y0)/(B*B)
      IF( XOUT .GT. 1.0 ) OUTIN = -1.0
C  
C ----------------------------------------------------------------------
C (2) Set constants for the equation.
C ----------------------------------------------------------------------
C  
      AABB  = DBLE(B-A)*DBLE(B+A)
      AABB2 = AABB*AABB
      AX    = DBLE(A)*DBLE(X0)
   
      DA(4) =  AX*AX
      DA(3) =  AX*AABB*0.5D0
      DA(2) =  AABB2 - DA(4) - (DBLE(B)*DBLE(Y0))**2
      DA(1) = -DA(3)
      DA(0) = -AABB2
   
      NDIM  = 4
#if defined(MSP)
      CALL DLOWP(DA,    NDIM, ZA, ICON )
#elif defined(UNIX)
      Call DMULLZ ( DA, NDIM, 1000, ZA )
      ICON = 0
#endif
C  
      IF( ICON .NE. 0 ) THEN
        PRINT *,'%Fatal error in UDELPS... CALL DLOWP return with ',
     >          ' ICON =',ICON
        CALL ERRTRA
        STOP
      ENDIF
C  
C  
C ----------------------------------------------------------------------
C (3) Search minimum distance among candidates.
C ----------------------------------------------------------------------
C  
300   CONTINUE
      DISMIN = 1.E20
      DO 310 IDIM = 1, NDIM
        TIMAG = DIMAG(ZA(IDIM))
        COSTH = DREAL(ZA(IDIM))
        IF( DABS(TIMAG) .GT. 1.D-10*DABS(COSTH) ) GO TO 310
   
CXXX    PRINT *,' IDIM =', IDIM,' COSTH=',COSTH
   
        IF( ABS(COSTH) .GT. 1.0D0) THEN
           COSTH = 1.0D0
           SINTH = 0.0D0
        ELSE
           SINTH = DSQRT( (1.D0-COSTH)*(1.D0+COSTH) )
        ENDIF
        SGN   = 1.0
        DO 320 ISGN = 1, 2
          IF( ISGN .EQ. 2 ) SGN = -1.
   
          XE  = A*COSTH
          YE  = B*SINTH*SGN
          DCHK = SQRT( (XE-X0)**2 + (YE-Y0)**2 )
          IF( DCHK .LT. DISMIN ) DISMIN = DCHK
320     CONTINUE
310   CONTINUE
   
      DIST = DISMIN*OUTIN
   
C  
      RETURN
      END
