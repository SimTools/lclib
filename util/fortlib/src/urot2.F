CC*********************************************************************C
C*                                                                     *
C*=========================**============                              *
C* Subroutine UROT2( FROM, TO, THETA )                                 *
C*=========================**============        * Output  # Inout     *
C*  (Purpose)                                                          *
C*       2-dimension Rotation utility.                                 *
C*                                                                     *
C*  (Input)                                                            *
C*       FROM(2) : Current vector ( x, y )                             *
C*       THETA   : Rotation angle                                      *
C*                                                                     *
C*  (Output)                                                           *
C*       TO(2)   : Updated vector.                                     *
C*                                                                     *
C*  (Author)                                                           *
C*       T.Y.Tsukamoto       30_Jul_1985         Update for library    *
C*                                                                     *
CC**********************************************************************
   
      SUBROUTINE UROT2( FROM, TO, THETA )
   
      REAL*4     FROM( 2 ),  TO( 2 )
      REAL*8     DFROM( 2 ), DTHETA
   
C==============<< Entry point >>=====================================
         DTHETA = THETA
   
      DO 1000 I = 1, 2
1000        DFROM( I ) = FROM( I )
   
         TO( 1 ) = DFROM(1)*DCOS(DTHETA) - DFROM(2)*DSIN(DTHETA)
         TO( 2 ) = DFROM(1)*DSIN(DTHETA) + DFROM(2)*DCOS(DTHETA)
   
      RETURN
      END
