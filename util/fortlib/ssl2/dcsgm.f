C******************************************************
C*
C* ---------------------------=========
C*  Subroutine DCSGM(AG, K, N, AS, ICON)
C* ---------------------------=========
C*(Function)
C*  Convert Matrix data (n x n real symmetric matrix) storage mode.
C*  From  compress format for symmetric
C*  Matrix to general format.
C*(Input)
C*  AS(*)  : Input one dimensional array.
C*           Size is N(N+1)/2
C*  N      : Dimension of the array AG
C*(Output)
C* AG(K,N) : Output matrix of general storge mode.
C* K       : Size of array
C* ICON      : Condition code.
C*             = 0 normal retuyrn
C*             = 30000  Error, N < 1 or K < N
C*(Author)
C*  A.Miyamoto  Apr-15-1992  Prepared with same argument as SSL2
C*
C**************************************************************
C
      SUBROUTINE DCSGM(AS,N,AG,K,ICON)
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
            AG(I,J) = AS(IP)
            AG(J,I) = AG(I,J)
 100  CONTINUE
C
      RETURN
      END

