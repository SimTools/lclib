CC**********************************************************************
C*                                                                     *
C*===========================******========                            *
C* Subroutine UI2SWP( INPUT, OUTPUT, NDIM )                            *
C*===========================******========      * Output  # Inout     *
C*  (Purpose)                                                          *
C*       Swap even I*2 word  and  odd I*2 word                         *
C*       Argument INPUT, OUTPUT must be 4 byte integer.                *
C*                                                                     *
C*  (Input)                                                            *
C*       INPUT  :  Value which you want to swap.  ( 4-byte )           *
C*       NDIM   :  Input/Output array's dimension                      *
C*                                                                     *
C*  (Output)                                                           *
C*       OUTPUT :  Array ( 4-byte ) be swapped.                        *
C*                                                                     *
C*  (Author)                                                           *
C*       T.Y.Tsukamoto       30-Jul-1985         Update for library    *
C*                                                                     *
CC**********************************************************************
   
      SUBROUTINE UI2SWP( INPUT, OUTPUT, NDIM )
   
      INTEGER*2    INPUT(*),  OUTPUT(*),  TEMP
   
C=====================================================================
   
      DO 1000  J = 1, NDIM*2-1, 2
            TEMP           =  INPUT( J )
            OUTPUT( J )    =  INPUT( J+1 )
            OUTPUT( J+1 )  =  TEMP
1000  CONTINUE
   
      RETURN
      END
