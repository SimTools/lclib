CC**********************************************************************
C*                                                                     *
C*===================*****========                                     *
C* Subroutine UZERO( ARRAY, NDIM )                                     *
C*===================*****========               * Output  # Inout     *
C*  (Purpose)                                                          *
C*       Initialize R*4/I*4_array ( Set zero )                         *
C*                                                                     *
C*  (Input)                                                            *
C*       NDIM   :  Output array's dimension                            *
C*                                                                     *
C*  (Output)                                                           *
C*       ARRAY  :  Array ( 4-byte ) be filled 0 / 0.0                  *
C*                                                                     *
C*  (Author)                                                           *
C*       T.Y.Tsukamoto       30-Jul-1985         Update for library    *
C*                                                                     *
CC**********************************************************************
   
      SUBROUTINE UZERO ( ARRAY, NDIM )
   
      INTEGER*4    ARRAY(*)
   
C==================<<  Entry Point  >>================================
   
      DO 1000  J = 1, NDIM
            ARRAY( J )  =  0
1000  CONTINUE
   
      RETURN
      END
