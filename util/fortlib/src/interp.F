C**********************************************************************
C* 
C*  -------------------------------------------======
C*  Subroutine INTERP( X, Y, NPTS, NTERMS, XIN, YOUT )
C*  -------------------------------------------======
C* 
C*(Note)
C*    This program is exactly same as the original Bevington's.
C* 
C*(Function)
C*     Interpolate between data points to evaluate a function.
C*     Make an (n-1)th-degree polynomial fit to pairs of data points
C*   (Xi,Yi) ordered for monotonically increasing bu not necessarily
C*   equally spaced Xi, and to interpolate ( or extrapolate ) to find
C*   the value of Y corresponding to a given value of X.
C* 
C*(Input parameters)
C*    X     - Array of data points for independent variable.
C*    Y     - Array of data points for dependent variable.
C*    NPTS  - Number of pairs of data points.
C*    NTERMS- Number of terms in fitting polynomial( .le. 15 )
C*    XIN   - Input value of X.
C* 
C*(Output parameters)
C*    YOUT  - Interpolated value of Y.
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 266
C* 
C*     Typed    by A. Miyamoto   13-May-1985
C*     Original by T. Tauchi
C* 
C*********************************************************************
C* 
      SUBROUTINE INTERP(X,Y,NPTS,NTERMS,XIN,YOUT)
C  
      PARAMETER  (MXxDIM=10)
      REAL*8      DELTAX,DELTA,A,PROD,SUM
      DIMENSION   X(*),Y(*),DELTA(MXxDIM),A(MXxDIM)
C  
C (1) Search for appropriate value of X(1)
C  
      DO 100 I = 1,NPTS
         IF( XIN .GT. X(I)) THEN
             GO TO 100
         ELSEIF( XIN .LT. X(I)) THEN
             I1  = I -NTERMS/2
             IF( I1 .GT. 0 ) GO TO 200
             I1  = 1
             GO TO 200
         ELSE
             YOUT = Y(I)
             GO TO 600
         ENDIF
100   CONTINUE
   
      I1 = NPTS - NTERMS + 1
200   CONTINUE
      I2 = I1 + NTERMS - 1
      IF( NPTS .LT. I2 ) THEN
          I2 = NPTS
          I1 = I2 - NTERMS + 1
          IF( I1 .LE. 0 ) THEN
              I1 = 1
              NTERMS = I2 - I1 +1
          ENDIF
      ENDIF
C  
C (3) Evaluate deviaions DELTA
C  
      DENOM = X(I1+1) - X(I1)
      DELTAX= (XIN - X(I1))/DENOM
      DO 300 I = 1,NTERMS
        IX = I1 + I -1
        DELTA(I) = (X(IX)-X(I1))/DENOM
300   CONTINUE
C  
C (4)  Accumulate coefficients A
C  
      A(1) = Y(I1)
      DO 400 K= 2, NTERMS
         PROD = 1.
         SUM  = 0.
         IMAX = K - 1
         IXMAX = I1 + IMAX
         DO 410 I =1, IMAX
            J    = K -I
            PROD = PROD*(DELTA(K)-DELTA(J))
            SUM  = SUM -A(J)/PROD
410      CONTINUE
         A(K) = SUM +Y(IXMAX)/PROD
400   CONTINUE
C  
C (5) Accumulate SUM of expansion
C  
      SUM = A(1)
      DO 510 J =2, NTERMS
         PROD = 1.
         IMAX = J-1
         DO 520 I =1, IMAX
            PROD = PROD*(DELTAX-DELTA(I))
520      CONTINUE
         SUM = SUM + A(J)*PROD
510   CONTINUE
      YOUT = SUM
C  
C (6)  Termination
C  
600   CONTINUE
      RETURN
      END
