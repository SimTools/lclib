CC**********************************************************************
C*                                                                     *
C*==========================**========                                 *
C* Subroutine UCOPY( FROMY, TO, NDIM )                                 *
C*==========================**========           * Output  # Inout     *
C*  (Purpose)                                                          *
C*       R*4/I*4  Array copy to another array                          *
C*                                                                     *
C*  (Input)                                                            *
C*       FROM   :  Value which you want to copy  ( I*4 / R*4 )         *
C*       NDIM   :  Output array's dimension                            *
C*                                                                     *
C*  (Output)                                                           *
C*       TO     :  Array ( 4-byte ) be copied                          *
C*                                                                     *
C*  (Author)                                                           *
C*       T.Y.Tsukamoto       30-Jul-1985         Update for library    *
C*                                                                     *
CC**********************************************************************
   
      SUBROUTINE UCOPY( FROM, TO, NDIM )
   
      INTEGER*4    FROM(*),  TO(*),  NDIM
   
C==================<<  Entry Point  >>================================
   
      DO 1000  J = 1, NDIM
            TO( J )  =  FROM( J )
1000  CONTINUE
   
      RETURN
      END
