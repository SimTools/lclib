C*******************************************************
C*
C* ---------------------------
C*   Subroutine TIMEL( TIMLFT )
C* ---------------------------
C*
C*(Function)
C*  TIMEL subroutine is suppoed to return the remaining CPU time of the
C*  process.   But at this moment, it returns just
C*  a big number.
C*
C*(Author)
C*  A. Miyamoto
C*
C********************************************************

      SUBROUTINE TIMEL(TIMLFT)
      REAL*4 TIMLFT
      TIMLFT = 36400.0
      
      RETURN
      END
