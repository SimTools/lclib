C*********************************************************************
C* 
C* ----------------------------------------------------==========
C* Subroutine POLFT2( X, Y, SIGMAY, NPTS, NTERMS, MODE, A, CHISQ )
C* ----------------------------------------------------==========
C* 
C*(Note)
C*   This subroutine is exactly same as the subroutine POLFIT in the
C*   Bevington's book p.140.
C* 
C*(Function)
C*   Make a least-square fit to data with a polynomial curve.
C*      Y=A(1)+A(2)*X+A(3)*X**2+A(3)*X**3+ . . .
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
C*    CHISQR  -  Reduced chi-square for fit.
C* 
C*(Note)
C*    NTERMS must be less than 10.
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 140
C* 
C*     Typed    by A. Miyamoto   13-May-1985
C*     Original by T. Tauchi
C* 
C*********************************************************************
C* 
      SUBROUTINE POLFT2(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,CHISQR)
C  
      PARAMETER  (MXxDIM = 10,
     >            MXxDM2 = 2*MXxDIM-1)
      REAL*8      SUMX,SUMY,XTERM,YTERM,ARRAY,CHISQ
      DIMENSION   X(NPTS),Y(NPTS),SIGMAY(NPTS),A(NTERMS)
      DIMENSION   SUMX(MXxDM2),SUMY(MXxDIM),ARRAY(MXxDIM,MXxDIM)
C  
C------< Entry Point >-------------------------------------------------
C  
C (1) Accumulate weighted sums.
C  
      NMAX    = 2*NTERMS-1
      DO 110 N=1,NMAX
110      SUMX(N) = 0.
      DO 120 J=1,NTERMS
120     SUMY(J) = 0.
      CHISQ   = 0.
      DO 200 I=1,NPTS
        XI      =X(I)
        YI      =Y(I)
        IF(MODE.GT.0) THEN
           WEIGHT = 1./SIGMAY(I)**2
        ELSEIF(MODE.EQ.0.OR.YI.EQ.0.) THEN
           WEIGHT = 1.
        ELSE
           WEIGHT = ABS(1./YI)
        ENDIF
        XTERM  = WEIGHT
        DO 210 N=1,NMAX
           SUMX(N)= SUMX(N) + XTERM
           XTERM  = XTERM*XI
210     CONTINUE
        YTERM  = WEIGHT*YI
        DO 220 N=1,NTERMS
           SUMY(N)= SUMY(N) + YTERM
           YTERM  = YTERM*XI
220     CONTINUE
        CHISQ  = CHISQ + WEIGHT*YI**2
200   CONTINUE
C  
C (2) Construct matrices and calculate coefficients.
C  
      DO 310 J=1,NTERMS
      DO 310 K=1,NTERMS
         N      =J + K - 1
         ARRAY(J,K) = SUMX(N)
310   CONTINUE
      DELTA =DETERM(ARRAY,NTERMS)
      IF(DELTA.EQ.0.) THEN
        CHISQR =0.
        DO 320 J=1, NTERMS
           A(J) =0.
320     CONTINUE
        GO TO 500
      ENDIF
   
      DO 330 L=1,NTERMS
         DO 340 J=1,NTERMS
            DO 350 K=1,NTERMS
               N=J+K-1
               ARRAY(J,K)=SUMX(N)
350         CONTINUE
            ARRAY(J,L)=SUMY(J)
340      CONTINUE
         A(L)=DETERM(ARRAY,NTERMS)/DELTA
330   CONTINUE
C  
C (4) Calculate chi square.
C  
      DO 400 J=1,NTERMS
         CHISQ=CHISQ-2.*A(J)*SUMY(J)
         DO 400 K=1,NTERMS
            N=J+K-1
            CHISQ=CHISQ+A(J)*A(K)*SUMX(N)
400   CONTINUE
   
      FREE=NPTS-NTERMS
      CHISQR=CHISQ/FREE
C  
C (5) Termination.
C  
500   CONTINUE
      RETURN
      END
