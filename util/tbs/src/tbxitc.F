      SUBROUTINE TBxITC( INTGR, CHARA)
C**********************************************************************C
C**                                                                    *
C*==================================                                   *
C* Subroutine TBxITC( INTGR, CHARA)                                    *
C*==================================                                   *
C*(Purpose)                                                            *
C*   Convert Integer*4 INTGR to Character*4 CHARA.                     *
C*(Input)                                                              *
C*   INTGR    : Integer*4 variable to be converted to Character code.  *
C*(Output)                                                             *
C*   CHARA    : Character*4                                            *
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     Aug 25 '84                                     *
C*                                                                     *
C***********************************************************************
C
#ifdef  UNIX
       CHARACTER*4 INTGR, CHARA
#endif
#ifdef  UNIX
       CHARA = INTGR
#endif
C
#ifdef  MSP
       INTEGER*4    ICODE(4)
#endif
#ifdef  MSP
       CHARACTER*4  CHARA
#endif
#ifdef  MSP
       CHARACTER*1  BLANK
#endif
#ifdef  MSP
       DATA  BLANK / ' ' /
#endif
C
#ifdef  MSP
       ICODE( 1 )   = INTGR
#endif
#ifdef  MSP
       ICODE( 2 )   = ISHFT( INTGR,   8 )
#endif
#ifdef  MSP
       ICODE( 3 )   = ISHFT( INTGR,  16 )
#endif
#ifdef  MSP
       ICODE( 4 )   = ISHFT( INTGR,  24 )
#endif
C
#ifdef  MSP
       DO  100  I = 1, 4
#endif
C
#ifdef  MSP
         ICODE( I )  = ISHFT( ICODE(I), -24 )
#endif
C
#ifdef  MSP
         IF( ICODE( I ) .LE. 64 ) THEN
#endif
#ifdef  MSP
             CHARA( I:I ) = BLANK
#endif
#ifdef  MSP
         ELSE
#endif
#ifdef  MSP
             CHARA( I:I ) = CHAR( ICODE(I) )
#endif
#ifdef  MSP
         ENDIF
#endif
C
#ifdef  MSP
  100  CONTINUE
#endif
C
          RETURN
          END
