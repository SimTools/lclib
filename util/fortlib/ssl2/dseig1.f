C****************************************************************
C*
C*  ----------------------=====--==---======
C*   Subroutine DSEIG1(A,N,E,EV,K,M,VW,ICON)
C*  ----------------------=====--==---======
C*(Function)
C*   Obtain eigen value and eigen vector of real
C*  symmetric matrix.
C*(Input)
C*  A( )   : Real symmetric matrix.  Compress format for
C*           real symmetric matrix, i.e., Size is
C*           N(N+1)/2.  Contents will be destroyed after
C*           the call to this routine.
C*  N      : dimension of the matrix.
C*  K      : Size of the array EV
C*(Output)
C*  E(N)    : Eigen values.
C*  EV(K,N) : Eigen vectors.
C*  M       : Number of eigen values.
C*  VW(2N)  : Work area of the size 2N
C*  ICON    : Condition code.
C*       = 0      no error.
C*       = 10000  N=1
C*       = 15000  Can not determine all eigen values.
C*       = 20000  Can not obtain any eigen vaues/eigen vectors.
C*       = 30000  N < 1 or K > N
C*(Author)  
C*  A.Miyamoto  15-Apr-1992  Prepared acording to SSL2 manual.
C*(Note)
C*  (1) This routine uses the subroutine HOQRVD of NUMPAC libraries.
C*      For HOQRVD, EPS=1.D-6 is used.  EPS is the variable to determine
C*      conversion.  If you want more accuracy, use smaller EPS value.
C*
C**********************************************************************
C
      SUBROUTINE DSEIG1(A,N,E,EV,K,M,VW,ICON)
      IMPLICIT   REAL*8 (A-H,O-Z)
      DIMENSION  A(*), E(N), EV(K,N), VW(2*N)
C
C =====< Entry Point >=========================================
C
      ICON = 0
      IF( N.LT.1 .OR. K.LT.N ) THEN
         ICON = 30000
         RETURN
      ELSEIF( N.EQ.1 ) THEN
         ICON = 10000
         RETURN
      ENDIF
C
C  Prepare inputs to call HORQVD
C
      CALL DCSGM( A, N, EV, K,  ICON ) 
C
C  Get Eigen value and Eigne vectors.
C
      ILL = 1
      EPS = 1.0D-6
      CALL HOQRVD( EV, K, N, E, VW, EPS, ILL)
C
      IF( ILL.EQ.0 ) THEN
         M = N
         ICON = 0
      ELSE   
         M = 0
         ICON = 20000
      ENDIF
C
      RETURN
      END
