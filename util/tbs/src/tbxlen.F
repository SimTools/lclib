      SUBROUTINE TBxLEN( CHIN, NWS)
C***********************************************************************
C**                                                                    *
C*=================================                                    *
C* Subroutine TBxLEN( CHIN, NWS )                                      *
C*=================================                                    *
C*(Purpose)                                                            *
C*   Get real length of the character string.                          *
C*(Input)                                                              *
C*   CHIN     : Input Character string                                 *
C*(Output)                                                             *
C*   NWS      : Real length of character string                        *
C*(Author)                                                             *
C*       S.Kawabata     July 17 '85                                    *
C*                                                                     *
C***********************************************************************
C
      CHARACTER*(*)  CHIN
      CHARACTER*1    BLK
      DATA BLK /' '/
 
      DO 100  I = 1, LEN(CHIN)
 
         IF( CHIN(I:I) .NE. BLK ) LENC = I
 
  100 CONTINUE
 
      NWS = ( LENC + 3 )/4
 
      RETURN
      END
