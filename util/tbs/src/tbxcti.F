      SUBROUTINE TBxCTI( CHARA, INTGR)
C**********************************************************************C
C*                                                                     *
C* =================================                                   *
C* Subroutine TBxCTI( CHARA, INTGR)                                    *
C* =================================                                   *
C*(Purpose)                                                            *
C*   Convert Character*4 CHARA to Integer*4 INTGR.                     *
C*(Input)                                                              *
C*   CHARA    : Character*4 variable to be converted to Integer.       *
C*(Output)                                                             *
C*   INTGR    : Character*4                                            *
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     Aug 25 '84                                     *
C*                                                                     *
C***********************************************************************
C
       INTEGER*4    ICODE(4)
       CHARACTER*4  CHARA
C
       DO  100  I = 1, 4
#if defined(LITTLE_ENDIAN)
         ICODE( 5-I ) =ICHAR( CHARA( I:I ) )
#else
         ICODE( I ) = ICHAR( CHARA( I:I ) )
#endif
  100  CONTINUE
C
       ICODE( 1 )   = ISHFT( ICODE(1), 24 )
       ICODE( 2 )   = ISHFT( ICODE(2), 16 )
       ICODE( 3 )   = ISHFT( ICODE(3),  8 )
C
       INTGR        = 0
       DO  200  I = 1, 4
C
         INTGR    = INTGR + ICODE( I )
C
  200  CONTINUE
C
         RETURN
         END
