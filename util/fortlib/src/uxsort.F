CC********************************************************************CC
C*                                                                    *C
C*===================================                                 *C
C*  Subroutine UXSORT(X,NTRY,ISORT)                                   *C
C*===================================                                 *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Sorts out X(*) in ascending order by giving a set of pointers   *C
C*    without touching the array itself.                              *C
C* (Inputs)                                                           *C
C*      X(I)     = array to be sorted out.                            *C
C*      NTRY     = array size.                                        *C
C* (Output)                                                           *C
C*      ISORT(I) = pointer to I-th from the smallest.                 *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =====================================
      SUBROUTINE UXSORT(X,NTRY,ISORT)
C =====================================
C  
      DIMENSION ISORT(NTRY),X(NTRY)
C  
C --- RESET ISORT FIRST
C  
      DO 5 I=1,NTRY
      ISORT(I) = -999
 5    CONTINUE
C  
C --- START SORTING X(NTRY)
C  
      I     = 1
      NDONE = 0
 6    CONTINUE
C  
      IF(ISORT(I).LT.0)   GO TO 7
      I = I+1
      GO TO 6
 7    CONTINUE
      IMIN = I
      DO 10  J=IMIN+1,NTRY
      IF(ISORT(J).GT.0)   GO TO 10
      IF(X(IMIN).LE.X(J)) GO TO 10
      IMIN = J
 10   CONTINUE
      NDONE = NDONE+1
      ISORT(NDONE) = ISIGN(IMIN,ISORT(NDONE))
      ISORT(IMIN)  = -ISORT(IMIN)
      IF(NDONE.LT.NTRY)   GO TO 6
C  
      RETURN
      END
