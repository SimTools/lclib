CC*********************************************************************C
C*                                                                     *
C*==================                                                   *
C* Subroutine PRUJIN                                                   *
C*==================                                                   *
C*  (Purpose)                                                          *
C*       Initialization of User analysis.                              *
C*                                                                     *
C*  (Input & Output )                                                  *
C*       none.
C*  (Relation)                                                         *
C*       Called from FLANLI
C*                                                                     *
C*  (Author)                                                           *
C*       A. Miyamoto  25-Apr-1987  For FULL system
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRUJIN
C
 
C---------------------------------------------------------------------
C  Write BTOF production version #.
C---------------------------------------------------------------------
      CALL PRPVRS( 'USER:', 101, 870511)
 
 
      RETURN
      END
