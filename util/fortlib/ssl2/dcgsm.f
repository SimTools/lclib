C******************************************************
C*
C* ---------------------------=========
C*  Subroutine DCGSM(AG, K, N, AS, ICON)
C* ---------------------------=========
C*(Function)
C*  Convert Matrix data (n x n real symmetric matrix) storage mode.
C*  From General format to compress format for symmetric
C*  Matrix.
C*(Input)
C*  AG(K,N)  : Input two dimensional array.
C*  K        : Size of array AG...(K.ge.N)
C*  N        : Size of array AG
C*(Output)
C* AS        : One dimensional array of commpress format.
C*             size n(n+1)/2
C* ICON      : Condition code.
C*             = 0 normal retuyrn
C*             = 30000  Error, N < 1 or K < N
C*(Author)
C*  A.Miyamoto  Apr-15-1992  Prepared with same argument as SSL2
C*
C**************************************************************
C
      SUBROUTINE DCGSM(AG, K, N, AS, ICON)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION AG(K,N)
      DIMENSION AS(*)
C
C ====< Entry Point >==========================================
C
      ICON = 0
      IF( N.LT.0 .OR. K.LT.N ) THEN
          ICON = 30000
          RETURN
      ENDIF
C
C
      DO 100 I = 1, K
         DO 100 J = 1, I
            IP = I*(I-1)/2 + J
            AS(IP) = AG(I,J)
 100  CONTINUE
C
      RETURN
      END

