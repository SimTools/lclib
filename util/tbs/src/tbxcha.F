      SUBROUTINE TBxCHA(  INTG, NWS, CHOUT )
C***********************************************************************
C*                                                                     *
C*======================================                               *
C* Subroutine TBxCHA( INTG, NWS, CHOUT )                               *
C*======================================                               *
C*(Purpose)                                                            *
C*   Convert from the integer type to the character*32 type.           *
C*(Input)                                                              *
C*   INTG     : Input Integer_array                                    *
C*   NWS      : Number of words                                        *
C*(Output)                                                             *
C*   CHOUT    : Output Character_string                                *
C*(Author)                                                             *
C*       S.Kawabata     Aug 25 '84                                     *
C*                                                                     *
C***********************************************************************
C
      PARAMETER ( NWORD = 16)
      INTEGER*4     INTG( NWORD )
      CHARACTER*(*) CHOUT
 
      CHOUT = ' '
      NW    = NWS
      IF( NWS .GT. NWORD) NW = NWORD
      DO 100  I = 1, NW
C
         I2    = 4*I
         I1    = I2 - 3
C
         CALL TBxITC( INTG( I ) , CHOUT( I1: I2) )
C
  100 CONTINUE
C
         RETURN
         END
