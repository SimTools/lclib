C**********************************************************************
C* 
C* ---------======----------
C* Function GQFUNC(X, I, A )
C* ---------======----------
C* 
C*(Note)
C*    This program is exactly same as the subroutine FUNCTN in
C*    the Bevington's book p.214
C* 
C*(Function)
C*    Evaluate terms of function for non-linear least-square search
C*    with form of a gaussian peak plus quadratic polynomial
C* 
C*    GQFUNC(X,I,A) = A(1)*EXP(-Z**2/2) + A(4) + A(5)*X + A(6)*X**2
C* 
C*    where
C*       X = X(i)
C*       Z = (X - A(2))/A(3)
C* 
C*(Input)
C*    X      ... Array of data points for independant variable.
C*    I      ... Index of data point.
C*    A      ... Array of parameters with size 6.
C* 
C*(Output)
C*    GQFUNC ... value of function.
C* 
C*(Subroutine and function required.)
C*    none.
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 214
C* 
C*     Typed by A. Miyamoto     8-Nov-1985
C* 
C**********************************************************************
C* 
      FUNCTION GQFUNC( X, I, A )
C  
      REAL*4     X(I), A(6)
C  
C---------------<< Entry Point >>--------------------------------------
C  
      XI     = X(I)
      GQFUNC = A(4) + A(5)*XI + A(6)*XI**2
      Z      = (XI - A(2))/A(3)
      GQFUNC = GQFUNC + A(1) * EXP( - Z**2/2.)
      RETURN
      END
