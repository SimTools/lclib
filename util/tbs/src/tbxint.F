      SUBROUTINE TBxINT( CHIN, NWS, INTG )
C***********************************************************************
C**                                                                    *
C*=================================                                    *
C* Subroutine TBxINT( CHIN, INTG )                                     *
C*=================================                                    *
C*(Purpose)                                                            *
C*   Convert from the character*32 to the integer array.               *
C*(Input)                                                              *
C*   CHIN     : Input Character string                                 *
C*   NWS      : Input Character string length (in I*4 Unit)            *
C*(Output)                                                             *
C*   INTG     : Output Integer Array                                   *
C*(Author)                                                             *
C*       S.Kawabata     Aug 25 '84                                     *
C*                                                                     *
C***********************************************************************
C
      PARAMETER ( NWORD = 16)
      INTEGER*4     INTG( NWORD )
      CHARACTER*(*)  CHIN
C
      DO 100  I = 1, NWS
C
         I2    = 4*I
         I1    = I2 - 3
C
         CALL TBxCTI( CHIN( I1: I2), INTG( I) )
C
  100 CONTINUE
C
         RETURN
         END
