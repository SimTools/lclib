CC*********************************************************************C
C*                                                                     *
C*==========================**==========                               *
C* Subroutine UCTRAN( FROM, TO, IFLAG )                                *
C*==========================**==========         * Output   # Inout    *
C*  (Purpose)                                                          *
C*       Coordinate transformation utility routine.                    *
C*       Angle is assumed to be radian unit.                           *
C*       In this routine, double precision calculation was done.       *
C*                                                                     *
C*  (Input)                                                            *
C*       FROM(3) : Position, which you want to transform.  ( real*4 )  *
C*       IFLAG   : Transformation mode selection code. ( See comment ) *
C*          IFLG = 12 :  from 1 to 2       IFLG = 13 :  from 1 to 3    *
C*          IFLG = 21 :  from 2 to 1       IFLG = 23 :  from 2 to 3    *
C*          IFLG = 31 :  from 3 to 1       IFLG = 32 :  from 3 to 2    *
C*         -------  Codes of Coordinate  -------                       *
C*          1 : Cartecian Coordinate    ( X, Y, Z )                    *
C*          2 : Cylindrical Coordinate  ( Radius, Phai, Z )            *
C*          3 : Spherical Coordinate    ( Radius, Phai, Theta )        *
C*                                                                     *
C*  (Output)                                                           *
C*       TO(3)   : Output coordinate.                                  *
C*                                                                     *
C*  (Author)                                                           *
C*       T.Y.Tsukamoto       30-Jul-1985         Update for library    *
C*                                                                     *
CC**********************************************************************
   
      SUBROUTINE UCTRAN( FROM, TO, IFLG )
   
      IMPLICIT     REAL * 8 ( D )
      DIMENSION    FROM( 3 ),  TO( 3 ),  DX( 3 )
   
C------------------<< Statement function >>--------------------------
      DSWA( D1, D2 )     = DSQRT( D1*D1 + D2*D2 )
      DSWA3( D1, D2, D3) = DSQRT( D1*D1 + D2*D2 + D3*D3 )
   
C==================<<  Entry Point  >>================================
   
      DO 1000  J = 1, 3
         TO( J ) = FROM( J )
         DX( J ) = FROM( J )
1000  CONTINUE
   
      IF( IFLG .EQ. 11 )  RETURN
      IF( IFLG .EQ. 22 )  RETURN
      IF( IFLG .EQ. 33 )  RETURN
   
      IF( FROM(1) .EQ. 0.0  .AND.  FROM(2) .EQ. 0.0  .AND.
     &                             FROM(3) .EQ. 0.0   )  RETURN
   
      IF(     IFLG .EQ. 12 )  THEN
            TO( 1 ) = DSWA( DX(1), DX(2) )
         IF( TO(1) .NE. 0.0 )  TO( 2 ) = DATAN2( DX(2), DX(1) )
      ELSEIF( IFLG .EQ. 13 )  THEN
            D1      = DSWA3( DX(1), DX(2), DX(3) )
            TO( 1 ) = D1
         IF( TO(1) .NE. 0.0 )  TO( 2 ) = DATAN2( DX(2), DX(1) )
            TO(3)=0.0
         IF( D1 .NE. 0.0D0 )   TO( 3 ) = DACOS( DX(3) / D1 )
      ELSEIF( IFLG .EQ. 21 )  THEN
            TO( 1 ) = DX( 1 ) * DCOS( DX(2) )
            TO( 2 ) = DX( 1 ) * DSIN( DX(2) )
      ELSEIF( IFLG .EQ. 23 )  THEN
            D1      = DSWA( DX(1), DX(3) )
            TO( 1 ) = D1
            TO( 3 ) = 0.0
         IF( D1 .NE. 0.0D0 )  TO( 3 ) = DACOS( DX(3) / D1 )
      ELSEIF( IFLG .EQ. 31 )  THEN
            TO( 1 ) = DX( 1 ) * DSIN( DX(3) ) * DCOS( DX(2) )
            TO( 2 ) = DX( 1 ) * DSIN( DX(3) ) * DSIN( DX(2) )
            TO( 3 ) = DX( 1 ) * DCOS( DX(3) )
      ELSEIF( IFLG .EQ. 32 )  THEN
            TO( 1 ) = DX( 1 ) * DSIN( DX(3) )
            TO( 3 ) = DX( 1 ) * DCOS( DX(3) )
      ELSE
         PRINT *,'#### UTRANS #### Coordinate indicate flag error : '
     &          , IFLG
         CALL ERRTRA
         RETURN
      ENDIF
   
      RETURN
      END
