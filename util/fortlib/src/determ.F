C**********************************************************************
C* 
C* -------------------------------
C* Function DETERM(ARRAY,NORDER)
C* -------------------------------
C* 
C*(Note)
C*    This program is exactly same as the original Bevington's.
C* 
C*(Function)
C*    Calculate the determinant of a square matrix.
C* 
C*(Input)
C*    ARRAY  ... Input symmetric matrix. Must be 10x10 real*8 array.
C*    NORDER ... Degree of matrix (Order of determinant).
C* 
C*(Output)
C*    DETERM ... Determinant of a matrix (REAL*4)
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 294
C* 
C*     Typed by A. Miyamoto    6-Apr-1985
C* 
C**********************************************************************
C* 
      FUNCTION DETERM(ARRAY,NORDER)
C  
      PARAMETER  (MXxDIM = 10)
      REAL*8   ARRAY(MXxDIM,MXxDIM),SAVE
C  
C ------------<< Entry Point >>----------------------------------------
C  
      DETERM   = 1.
      DO 100 K = 1, NORDER
C  
C (1) Interchange columns if diagonal element is zero.
C  
        IF(ARRAY(K,K).EQ.0.) THEN
           DO 110 J = K, NORDER
              IF(ARRAY(K,J).NE.0.) GO TO 120
110        CONTINUE
           DETERM   = 0.
           PRINT *,'%Warning ... DETERM  ...'
           PRINT *,'             Contains all zero columns, DETERM=0.'
           RETURN
C  
120        CONTINUE
           DO 130 I = K, NORDER
              SAVE  = ARRAY(I,J)
              ARRAY(I,J) = ARRAY(I,K)
              ARRAY(I,K) = SAVE
130        CONTINUE
           DETERM   = -DETERM
        ENDIF
   
C  
C (2) Subtract row K from lower rows to get diagonal matrix.
C  
        DETERM = DETERM * ARRAY(K,K)
        IF( K .LT. NORDER ) THEN
            K1 = K + 1
            DO 200 I = K1, NORDER
            DO 200 J = K1, NORDER
              ARRAY(I,J) = ARRAY(I,J) - ARRAY(I,K)*ARRAY(K,J)/ARRAY(K,K)
200         CONTINUE
        ENDIF
C  
100   CONTINUE
      RETURN
      END
