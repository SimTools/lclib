CC**********************************************************************
C*                                                                     *
C*===================*****==============                               *
C* Subroutine UFILL( ARRAY, VALUE, NDIM )                              *
C*===================*****==============         * Output  # Inout     *
C*  (Purpose)                                                          *
C*       Set R*4/I*4  Value  to  another R*4/I*4_array                 *
C*                                                                     *
C*  (Input)                                                            *
C*       VALUE  :  Value which you want to set.                        *
C*       NDIM   :  Output array's dimension                            *
C*                                                                     *
C*  (Output)                                                           *
C*       ARRAY  :  Array ( 4-byte ) be filled VALUE                    *
C*                                                                     *
C*  (Author)                                                           *
C*       T.Y.Tsukamoto       30-Jul-1985         Update for library    *
C*                                                                     *
CC**********************************************************************
   
      SUBROUTINE UFILL ( ARRAY, VALUE, NDIM )
   
      INTEGER*4    ARRAY(*),  VALUE
   
C==================<<  Entry Point  >>================================
   
      DO 1000  J = 1, NDIM
            ARRAY( J )  =  VALUE
1000  CONTINUE
   
      RETURN
      END
