C**********************************************************************
C* 
C*  ------------------------------------
C*  Function  AREA( X, Y, NPTS, NTERMS )
C*  ------------------------------------
C* 
C*(Note)
C*      This program is exactly same as the original Bevington's.
C* 
C*(Function)
C*      Integrate the area beneath a set of data points
C* 
C*(Input Parameters)
C*      X       - Array of data points for independent variable
C*      Y       - Array of data points for dependent variable
C*      NPTS    - Number of pairs of data points
C*      NTERMS  - Number of terms in fitting polynomial
C* 
C*(Subroutines and function subprograms required)
C*      INTEG( X, Y, NTERMS, I1, X1, X2, SUM)
C*      fits a polynomial with NTERMS starting at I1
C*      and integrates area from X1 to X2
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 272
C* 
C*     Typed    by A. Miyamoto   13-May-1985
C*     Original by T. Tauchi
C* 
C**********************************************************************
C* 
      FUNCTION AREA (X,Y,NPTS,NTERMS)
C  
      DOUBLE PRECISION SUM
      DIMENSION X(NPTS),Y(NPTS)
C  
C-------< Entry Points >----------------------------------------------
C  
      SUM = 0.
C  
C (1) Fit all points with one curve, if NPTS .LE.NTERMS
C  
      IF(NPTS.LE.NTERMS) THEN
        X1 = X(1)
        X2 = X(NPTS)
        CALL INTEG(X,Y,NPTS,1,X1,X2,SUM)
        GO TO 400
      ENDIF
   
      NEVEN = 2*(NTERMS/2)
      IDELTA = NTERMS/2 - 1
C  
C (2) Even number of terms
C  
      IF(NTERMS.LE.NEVEN) THEN
        X1 = X(1)
        J  = NTERMS - IDELTA
        X2 = X(J)
        CALL INTEG(X,Y,NTERMS,1,X1,X2,SUM)
        I1 = NPTS - NTERMS + 1
        J =  I1 + IDELTA
        X1 = X(J)
        X2 = X(NPTS)
        CALL INTEG(X,Y,NTERMS,I1,X1,X2,SUM)
        IF(I1.GT.2) THEN
          IMAX = I1 -1
          DO 210 I=2,IMAX
            J = I + IDELTA
            X1 = X(J)
            X2 = X(J+1)
            CALL INTEG(X,Y,NTERMS,I,X1,X2,SUM)
210       CONTINUE
        ENDIF
      ELSE
C  
C (3)  Odd number of terms
C  
        X1 = X(1)
        J = NTERMS - IDELTA
        X2= (X(J)+X(J-1))/2.
        CALL INTEG(X,Y,NTERMS,1,X1,X2,SUM)
        I1 = NPTS - NTERMS +1
        J = I1 + IDELTA
        X1 = (X(J)+ X(J+1))/2.
        X2 = X(NPTS)
        CALL INTEG(X,Y,NTERMS,I1,X1,X2,SUM)
        IF(I1.GT.2) THEN
          IMAX = I1 -1
          DO 300 I=2,IMAX
            J = I + IDELTA
            X1 = (X(J+1) +X(J))/2.
            X2 = (X(J+2) +X(J+1))/2.
            CALL INTEG(X,Y,NTERMS,I,X1,X2,SUM)
300       CONTINUE
        ENDIF
      ENDIF
C  
C (4)  Termination
C  
400   CONTINUE
      AREA = SUM
      RETURN
      END
