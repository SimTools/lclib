C  UVZERO    ECSECT
C***************************************************************
C*                                                             *
C* SUBROUTINE UVZERO( NWORD, ARRAY )                           *
C*                                                             *
C*  (Purpose)                                                  *
C*       Zero clear array.    .                                *
C*  (Input)                                                    *
C*       ARRAY : Target array.                                 *
C*       NWORD : # of words ( size of ARRAY.)                  *
C*  (Author)                                                   *
C*       A. Miyamoto    18-Jun-1986                            *
C*                                                             *
C***************************************************************
C23456
      SUBROUTINE UVZERO( NWORD, IARRAY)
      INTEGER*4 IARRAY(NWORD)
      DO 100 I = 1, NWORD
         IARRAY(I) = 0
 100  CONTINUE
CC#ifdef  HP
CC      call vec_xinit(IARRAY, NWORD, 0 )
CC#endif
      RETURN
      END
