C*********************************************************************
C* 
C* ----------------------------------------------================
C* Subroutine POLFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,SIGMAA, CHISQ )
C* ----------------------------------------------================
C* 
C*(Caution)
C*     Argument is deferent from the Bevington's original.
C* 
C*(Function)
C*   Make a least-square fit to data with a polynomial curve.
C* 
C*      Y=A(1)+A(2)*X+A(3)*X**2+A(3)*X**3+ . . .
C* 
C*   This program is modified from orginal version.  Use MATRIX
C*   inversion technique to output error of fitted parameter.
C* 
C*(Input Parameters)
C*    X       -  Array of data points for independant variable.
C*    Y       -  Array of data points for dependant variable.
C*    SIGMAY  -  Array of standard deviations for Y data points.
C*    NPTS    -  Number of pairs of data points.
C*    NTERMS  -  Number of coefficients (Degree of polynomial + 1)
C*    MODE    -  Determines method of weighting least square fit.
C*                 +1 (Instrumental) WEIGHT(1) = 1./SIGMAY(1)**2
C*                  0 (No weighting) WEIGHT(1) = 1.
C*                 -1 (Statistical)  WEIGHT(1) = 1./Y(1)
C* 
C*(Output parameters)
C*    A       -  Array of coefficients of polynomial.
C*    SIGMAA  -  Array of the error of coefficient.
C*    CHISQR  -  Reduced chi-square for fit.
C* 
C*(Note)
C*    NTERMS must be less than 10.
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 140
C* 
C*     A. Miyamoto  9-Nov-1985   Original version.
C* 
C*********************************************************************
C* 
      SUBROUTINE POLFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,SIGMAA,CHISQR)
C  
      PARAMETER  (MXxDIM = 10,
     >            MXxDM2 = 2*MXxDIM-1)
      REAL*8      ARRAY(MXxDIM,MXxDIM),BETA(MXxDIM),CHISQ
      REAL*8      XTERM, YTERM, XI, WEIGHT
      REAL*4      X(NPTS),Y(NPTS),SIGMAY(*),A(NTERMS),SIGMAA(NTERMS)
C  
C------< Entry Point >-------------------------------------------------
C  
C (1) Evaluate Beta vector and ARRAY matrix.
C  
      DO 100 J = 1, NTERMS
         BETA(J) = 0.
         DO 100 K = 1, J
            ARRAY(J,K) = 0.
100   CONTINUE
C  
      DO 150 I = 1, NPTS
        IF(MODE.GT.0) THEN
           WEIGHT = 1./SIGMAY(I)**2
        ELSEIF(MODE.EQ.0.OR.Y(I).EQ.0.) THEN
           WEIGHT = 1.
        ELSE
           WEIGHT = ABS(1./Y(I))
        ENDIF
        XI = X(I)
        YTERM = Y(I)
        IF(XI.NE.0.) THEN
          DO 200 J = 1, NTERMS
             XTERM = WEIGHT*XI**(J-1)
             BETA(J) = BETA(J) + YTERM*XTERM
             DO 210 K = 1, J
                XTERM = WEIGHT*XI**(K+J-2)
                ARRAY(J,K) = ARRAY(J,K) + XTERM
210          CONTINUE
200       CONTINUE
        ELSE
          BETA(1) = BETA(1) + WEIGHT*YTERM
          ARRAY(1,1) = ARRAY(1,1) + WEIGHT
        ENDIF
150   CONTINUE
C  
      DO 170 J = 1, NTERMS
         DO 170 K = 1, J
            ARRAY(K,J) = ARRAY(J,K)
170   CONTINUE
C  
C (2) Invert Matrix
C  
      CALL MATINV(ARRAY, NTERMS, DET)
C  
C (3) Returns fitted results and parameters.
C  
      DO 300 J = 1, NTERMS
         A(J) = 0
         DO 300 K = 1, NTERMS
            A(J) = A(J) + ARRAY(J,K)*BETA(K)
300   CONTINUE
C  
C (4) Calculate chi-square.
C  
      CHISQ = 0.
      DO 400 I = 1, NPTS
        IF(MODE.GT.0) THEN
           WEIGHT = 1./SIGMAY(I)**2
        ELSEIF(MODE.EQ.0.OR.Y(I).EQ.0.) THEN
           WEIGHT = 1.
        ELSE
           WEIGHT = ABS(1./Y(I))
        ENDIF
        XI    = X(I)
        YTERM = Y(I)
        IF(XI.NE.0.) THEN
          DO 410 J = 1, NTERMS
             YTERM = YTERM - A(J)*XI**(J-1)
410       CONTINUE
        ELSE
          YTERM = YTERM - A(1)
        ENDIF
        CHISQ = CHISQ + WEIGHT*YTERM*YTERM
400   CONTINUE
C  
      CHISQ = CHISQ / FLOAT(NPTS-NTERMS)
      DO 450 J = 1, NTERMS
         IF(MODE.EQ.0) THEN
           SIGMAA(J) = ARRAY(J,J)*CHISQ
         ELSE
           SIGMAA(J) = ARRAY(J,J)
         ENDIF
450   CONTINUE
      CHISQR = CHISQ
C  
C (5) Normal termination.
C  
      RETURN
      END
