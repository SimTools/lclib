C**********************************************************************
C* 
C* -------------------------------=====
C* Subroutine MATINV(ARRAY,NORDER, DET)
C* -------------------------------=====
C* 
C*(Note)
C*    This program is exactly same as the original Bevington's.
C* 
C*(Function)
C*    Invert a symmetric matrix.
C* 
C*(Input)
C*    ARRAY  ... Input symmetric matrix. Must be 10x10 real*8 array.
C*    NORDER ... Degree of matrix.
C* 
C*(Output)
C*    ARRAY  ... Inverse of input matrix.
C*    DET    ... Determinant of input matrix.
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 302
C* 
C*     Typed by A. Miyamoto     6-Apr-1985
C* 
C**********************************************************************
C* 
      SUBROUTINE MATINV(ARRAY,NORDER, DET)
C  
      PARAMETER  (MXxDIM = 10)
C  
      REAL*8      ARRAY(MXxDIM,MXxDIM),AMAX,SAVE
      INTEGER*4   IK(MXxDIM),JK(MXxDIM)
C  
C---------------<< Entry Point >>--------------------------------------
C  
      DET = 1.
      DO 100 K=1,NORDER
C  
C (1) Find largest element array(i,j) in rest of matrix.
C  
        AMAX=0.
110     CONTINUE
        DO 120 I = K, NORDER
        DO 120 J = K, NORDER
          IF(DABS(AMAX).GT.DABS(ARRAY(I,J))) GO TO 120
            AMAX=ARRAY(I,J)
            IK(K)=I
            JK(K)=J
120     CONTINUE
C  
C (2) Interchange rows and columns to put AMAX in ARRAY(K,K)
C  
        IF(AMAX.EQ.0.0) THEN
          PRINT *,'%Error in MATINV... Element of array all zero.'
          PRINT *,'                    Matrix is not inverted.'
          DET = 0.
          RETURN
        ENDIF
C  
        I=IK(K)
        IF(I.LT.K) GO TO 110
        IF(I.GT.K) THEN
           DO 150 J=1,NORDER
              SAVE=ARRAY(K,J)
              ARRAY(K,J)=ARRAY(I,J)
              ARRAY(I,J)=-SAVE
150        CONTINUE
        ENDIF
C  
        J=JK(K)
        IF(J.LT.K) GO TO 110
        IF(J.GT.K) THEN
           DO 160 I=1,NORDER
              SAVE =ARRAY(I,K)
              ARRAY(I,K)=ARRAY(I,J)
              ARRAY(I,J)=-SAVE
160        CONTINUE
        ENDIF
C  
C (3) Accumulate elements of inverse matrix
C  
        DO 170 I=1,NORDER
           IF(I.NE.K) ARRAY(I,K)=-ARRAY(I,K)/AMAX
170     CONTINUE
        DO 180 I=1,NORDER
        DO 180 J=1,NORDER
           IF(I.NE.K .AND. J.NE.K)
     &        ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)
180     CONTINUE
        DO 190 J=1,NORDER
           IF(J.NE.K) ARRAY(K,J)=ARRAY(K,J)/AMAX
190     CONTINUE
        ARRAY(K,K)=1./AMAX
        DET = DET * AMAX
   
100   CONTINUE
C  
C (4) Restore ordering of matrix.
C  
      DO 400 L=1,NORDER
         K=NORDER-L+1
         J=IK(K)
         IF(J.GT.K) THEN
            DO 410 I=1,NORDER
               SAVE=ARRAY(I,K)
               ARRAY(I,K)=-ARRAY(I,J)
               ARRAY(I,J)=SAVE
410         CONTINUE
         ENDIF
C  
         I=JK(K)
         IF(I.GT.K) THEN
            DO 420  J=1,NORDER
               SAVE=ARRAY(K,J)
               ARRAY(K,J)=-ARRAY(I,J)
               ARRAY(I,J)=SAVE
420         CONTINUE
         ENDIF
400   CONTINUE
C  
      RETURN
      END
