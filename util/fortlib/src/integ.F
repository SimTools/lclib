C***********************************************************************
C* 
C*    -------------------------------------------=====
C*    Subroutine INTEG( X, Y, NTERMS, I1, X1, X2, SUM)
C*    -------------------------------------------=====
C* 
C*(Note)
C*    This program is exactly same as the original Bevington's.
C* 
C*(Function)
C*    Integrate the area beneath two data points
C* 
C*(Input Parameters)
C*    X       - Array of data points for independent variable
C*    Y       - Array of data points for dependent variable
C*    NTERMS  - Number of terms in fitting polynomial
C*    I1      - First data point for fitting polynomial
C*    X1      - First value of X for integration
C*    X2      - Final value of X for integration
C* 
C*(Output Parameters)
C*    SUM     - results (Double precision).
C* 
C*(Subroutines and function subprograms required)
C*               MATINV(ARRAY,NTERMS,DET)
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 274
C* 
C*     Typed    by A. Miyamoto   13-May-1985
C*     Original by T. Tauchi
C* 
C**********************************************************************
C* 
      SUBROUTINE INTEG (X,Y,NTERMS,I1,X1,X2,SUM)
C  
      DOUBLE PRECISION XJK,ARRAY,A,DENOM,DELTAX,SUM
      DIMENSION X(*),Y(*)
      PARAMETER (MXxDIM=10)
   
      DIMENSION ARRAY(MXxDIM,MXxDIM)
C  
C------< Entry Point >-------------------------------------------------
C  
C (1) Construct squre matrix and invert
C  
      IF(NTERMS.GT.MXxDIM) THEN
        PRINT *,' %Error..INTEG...',
     >          ' NTERMS greather than the size of ARAY '
        STOP
      ENDIF
      DO 100 J =1, NTERMS
        I = J + I1 -1
        DELTAX = X(I)-X(I1)
        XJK = 1.
        DO 100 K =1,NTERMS
           ARRAY(J,K) = XJK
           XJK = XJK*DELTAX
100   CONTINUE
      CALL MATINV(ARRAY,NTERMS,DET)
      IF(DET.EQ.0.0) THEN
        IMID = I1 + NTERMS/2
        SUM = SUM + Y(IMID)*(X2-X1)
        GO TO 300
      ENDIF
C  
C (2) Evaluate coefficients and integrate
C  
      DX1 = X1 - X(I1)
      DX2 = X2 - X(I1)
      DO 200 J=1,NTERMS
        I = J +I1-1
        A = 0.
        DO 210 K =1,NTERMS
           A = A +Y(I)*ARRAY(J,K)
210     CONTINUE
        DENOM = J
        SUM = SUM + (A/DENOM)*(DX2**J-DX1**J)
200   CONTINUE
C  
C (3)  Termination
C  
300   CONTINUE
      RETURN
      END
