C**********************************************************************
C* 
C* -------------------------------------------=======
C* Subroutine GQDRIV( X, I, A, DELTAA, NTERMS, DERIV )
C* -------------------------------------------=======
C* 
C*(Note)
C*    This program is exactly same as the subroutine FDRIVE in
C*    the Bevington's book p.241
C* 
C*(Function)
C*    Evaluate derivative of function for least-square search with
C*    form of a gaussian peak plus quadratic polynomial.
C* 
C*    FUNCTN(X,I,A) = A(1)*EXP(-Z**2/2) + A(4) + A(5)*X + A(6)*X**2
C* 
C*    where
C*       X = X(i)
C*       Z = (X - A(2))/A(3)
C* 
C*(Input)
C*    X      ... Array of data points for independant variable.
C*    I      ... Index of data point.
C*    A      ... Array of parameters with size 6.
C*    DELTAA ... Array of parameters increment.
C*    NTERMS ... Number of parameters.
C* 
C*(Output)
C*    DERIV  ... Derivative of function.
C*                     d f{x(i)}
C*         DERIV(j) = -----------
C*                     d a(j)
C* 
C*(Subroutine and function required.)
C*    none.
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 241
C* 
C*     Typed by A. Miyamoto     8-Nov-1985
C* 
C**********************************************************************
C* 
      SUBROUTINE GQDRIV( X, I, A, DELTAA, NTERMS, DERIV )
C  
      REAL*4     X(I), A(NTERMS), DELTAA(NTERMS), DERIV(NTERMS)
C  
C---------------<< Entry Point >>--------------------------------------
C  
      XI     = X(I)
      Z      = (XI - A(2))/A(3)
      Z2     = Z**2
C  
      IF(Z2 .GT. 50. ) THEN
        DO 100 J = 1, 3
           DERIV(J) = 0.
100     CONTINUE
      ELSE
        DERIV(1) = EXP( -Z2/2.)
        DERIV(2) = A(1) * DERIV(1) * Z/A(3)
        DERIV(3) = DERIV(2) * Z
      ENDIF
C  
      DERIV(4) = 1.
      DERIV(5) = XI
      DERIV(6) = XI**2
      RETURN
      END
