C**********************************************************************
C* 
C* ---------------------------------------------------------------
C* Subroutine CURFIT( X, Y, SIGMAY, NPTS, NTERMS, MODE, A, DELTAA,
C*                    SIGMAA, FLAMDA, YFIT, CHISQR, FUNCTN, FDERIV)
C* -------------------=======-------================--------------
C* 
C*(Caution)
C*     Argument is deferent from the Bevington's original.
C* 
C*(Function)
C*      Make a least-squres fit to a non-linear function
C*    with a linearization of the fitting function
C*    Using the algorithm of Marquardt which combines a gradient
C*    search with an analytical solution developped from linearization
C*    of the fitting function.
C* 
C*(Input Parameters)
C*      X       - Array of data points for independent variable
C*      Y       - Array of data points for dependent variable
C*      SIGMAY  - Array of standard deviations for Y data points
C*      NPTS    - Number of pairs of data points
C*      NTERMS  - Number of parameters
C*      MODE    - Determines method of weighting least-squres fit
C*                +1 (instrumental) WEIGHT = 1./SIGMAY(I)**2
C*                 0 (no weighting) WEIGHT = 1.
C*                -1 (statistical)  WEIGHT = 1./Y(I)
C*      A       - Array of parameters, input and output.
C*      DELTAA  - Array of increments for parameters A
C*      FLAMDA  - Propotion of gradient search included
C*      FUNCTN  - Name of function to calculate the fitting function
C*               for the Ith term.  This routine must be declaired
C*               as EXTERNAL in the caller routine.  Calling sequence
C*               is,
C* 
C*               Function FUNCTN(X,I,A)
C* 
C*               Input to function FUNCTN is,
C* 
C*               X   ... Array of data points for independant variable.
C*               I   ... Index of data point.
C*               A   ... Array of parameters.
C* 
C*               Output is a value of function FUNCTN.
C*               For example of this routine, please reffer to function
C*               GQFNCT.
C*      FDERIV  -Name of subroutine to evaluate derivalives of fitting
C*               function for the Ith term with respect to each
C*               parameter. This routine must be declaired in the
C*               caller routine as EXTERNAL.. Calling sequence of FDERIV
C*               is,
C* 
C*               Subroutine FDERIV(X, I, A, DELTAA, NTERMS, DERIV)
C* 
C*               Input to subroutine FDERIV is,
C* 
C*               X   ... Array of data points for independant variable.
C*               I   ... Index of data point.
C*               A   ... Array of parameters.
C*               DELTAA  Array of parameter increments.
C*               NTERMS  Number of parameters.
C* 
C*               Required output is,
C* 
C*               DERIV . Derivatives of function.  For example, please
C*               see subroutine GQDRIV.
C* 
C*(Output Parameters)
C*      A       - Array of parameters, input and output.
C*      SIGMAA  - Array of standard deviations for parameters A
C*      YFIT    - Array of calculated values of Y
C*      CHISQR  - Reduced CHI squre for FIT
C* 
C*(Subroutines and function subprograms required).
C*      FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
C*         evaluates reduced chi squre for fit to data
C*         evaluates the derivatives of the fitting function
C*         for the Ith term with respect to each parameter
C*      MATINV(ARRAY,NTERMS,DET)
C*         inverts a symmetric two-dimensional matrix of degree nterms
C*         and calculates its determinant
C* 
C*(Notes)
C*      dimension statement valid for nterms up tp 10
C*      set FLAMDA = 0.001 at beginning of search
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 237
C* 
C*     Typed    by A. Miyamoto   13-May-1985
C*     Original by T. Tauchi
C* 
C*********************************************************************
C* 
      SUBROUTINE CURFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,DELTAA,
     > SIGMAA,FLAMDA,YFIT,CHISQR,FUNCTN,FDERIV)
C  
      PARAMETER  (MXxDIM=10)
      DIMENSION   X(NPTS),Y(NPTS),SIGMAY(*),      A(NTERMS),
     >            DELTAA(NTERMS), SIGMAA(NTERMS), YFIT(NPTS)
      REAL*8      ARRAY(MXxDIM, MXxDIM)
      REAL*4      ALPHA(MXxDIM,MXxDIM),
     >            BETA(MXxDIM),   DERIV(MXxDIM),  B(MXxDIM)
C  
C     EXTERNAL    FUNCTN, FDERIV
C  
C--------< Entry Point >-----------------------------------------------
C  
      NFREE = NPTS -NTERMS
      IF( NFREE .LE. 0 ) THEN
          PRINT *,' %Error..CURFIT..# of data less than # of parameter'
          CHISQR = 0.
          GO TO 800
      ENDIF
C  
C (2) evaluate weights
C  
C   Original Bevington was modified to calculate weight in DO 320 loop.
C  
C (3) evaluate alpha and beta matrices
C  
      DO 310 J =1,NTERMS
         BETA(J) = 0.
         DO 310 K =1,J
            ALPHA(J,K)=0.
310   CONTINUE
      DO 320 I = 1,NPTS
         CALL FDERIV(X,I,A,DELTAA,NTERMS,DERIV)
         IF( MODE .GT. 0 ) THEN
             WEIGHT = 1. / SIGMAY(I)**2
         ELSEIF( MODE .EQ. 0. .OR. Y(I) .EQ. 0.) THEN
             WEIGHT = 1.
         ELSE
             IF( Y(I) .GT. 0. ) THEN
                 WEIGHT = 1. / Y(I)
             ELSE
                 WEIGHT = 1./(-Y(I))
             ENDIF
         ENDIF
         DO 330 J  =1,NTERMS
            BETA(J) = BETA(J) +WEIGHT*(Y(I)-FUNCTN(X,I,A))*DERIV(J)
            DO 330 K =1,J
               ALPHA(J,K)=ALPHA(J,K) + WEIGHT*DERIV(J)*DERIV(K)
330        CONTINUE
320   CONTINUE
      DO 340 J=1,NTERMS
      DO 340 K=1,J
        ALPHA(K,J) = ALPHA(J,K)
340   CONTINUE
C  
C (4) evaluate chi squre at starting point
C  
      DO 400 I=1,NPTS
400     YFIT(I)=FUNCTN(X,I,A)
      CHISQ1 =FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
C     PRINT *,' CHISQ1 =',CHISQ1
C  
C (5) invert modified curvature matrix to find new parameters
C  
500   CONTINUE
      DO 510 J=1,NTERMS
         DO 520 K=1,NTERMS
            ARRAY(J,K)=ALPHA(J,K)/SQRT(ALPHA(J,J)*ALPHA(K,K))
520      CONTINUE
         ARRAY(J,J)=1.+FLAMDA
510   CONTINUE
      CALL MATINV(ARRAY,NTERMS,DET)
      DO 530 J=1,NTERMS
        B(J) = A(J)
        DO 530 K=1,NTERMS
           B(J)=B(J)+BETA(K)*ARRAY(J,K)/SQRT(ALPHA(J,J)*ALPHA(K,K))
530   CONTINUE
C  
C (6) if chi squre increase, increase FLAMDA and try again
C  
      DO 600 I=1,NPTS
        YFIT(I) = FUNCTN(X,I,B)
600   CONTINUE
        CHISQR=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
C     PRINT *,' CHISQR =',CHISQR
      IF( CHISQ1 .LT. CHISQR ) THEN
        FLAMDA = 10.*FLAMDA
        GO TO 500
      ENDIF
C  
C (7) evaluate parameters and uncertainties
C  
      DO 700 J =1,NTERMS
        A(J) = B(J)
        IF(ARRAY(J,J).LT.0.0.OR.ALPHA(J,J).LT.0.) THEN
           WRITE(6,710) J,ARRAY(J,J),ALPHA(J,J)
710        FORMAT(' %Error..CURFIT..diagonal element of ARRAY or ALPHA',
     >            ' Less than 0.',/,
     >        10X,'j,array(j,j),alpha(j,j)=',I4,2E13.5)
           SIGMAA(J) = -1.
        ELSE
           SIGMAA(J)=SQRT(ARRAY(J,J)/ALPHA(J,J))
        ENDIF
700   CONTINUE
      FLAMDA=FLAMDA/10.
C  
C (8) Termination
C  
800   RETURN
      END
