CC      CHARACTER*10 TEST/X'C0C1C2C3C4C5C6C7C8C9'/
C       CHARACTER*10 TEST/X'C2C5C7C9D56DD9E4D540'/
C       CALL UCEBC(10,TEST)
C       PRINT *,' STRING=',TEST
C       STOP
C       END
C******************************************************
C*
C*  Subroutine UCEBC( LENGTH, STRING )
C*
C*(Function) 
C*  Translate EBCDIC code to ASC-ii code.
C*
C*(Input)
C*  LENGTH  : Length of input characer in byte.
C*  STRING  : Character string.
C*(Output)
C*  STRING  : String converted to ASC-II
C*
C*(Author)
C*  A.Miyamoto  6-May-1994  UNIX version prepared.
C*
C*****************************************************
C*
      SUBROUTINE UCEBC( LENGTH, STRING )
C*
      CHARACTER*(*) STRING
      CHARACTER*1   TABLE(0:255)
      CHARACTER*4  TABDAT(4,0:15)
      EQUIVALENCE  (TABDAT(1,0),TABLE(0))
C
#ifdef G77_521_OR_GREATER
      DATA ((TABDAT(J,K),J=1,4),K=0,15)
C                 N S S F       D
C                 U O T T   H   E       V F C S S
C00-0F            L H X X \ T \ L \ \ \ T F R O I
     >        /X'00010203',X'5C095C7F',X'5C5C5C0B',X'0C0D0E0F',
C                 D D D D         C
C                 L C C C     B   A E     F G R U
C10-1F            E 1 2 3 \ \ S \ N M \ \ S S S S
     >         X'10111213',X'5C5C085C',X'18195C5C',X'1C1D1E1F',
C                             E E           E A B
C                           L T S           N C E
C20-2F            \ \ \ \ \ F B C \ \ \ \ \ Q K L
     >         X'5C5C5C5C',X'5C0A171B',X'5C5C5C5C',X'5C050607',
C                     S         E         D N   S
C                     Y         O         C A   U
C30-3F            \ \ N \ \ \ \ T \ \ \ \ 4 K \ B
     >         X'5C5C165C',X'5C5C5C04',X'5C5C5C5C',X'14155C1A',
C40-4F           SP \ \ \ \ \ \ \ \ \{{ . < ( + ]
     >         X'205C5C5C',X'5C5C5C5C',X'5C5C5B2E',X'3C282B21',
C50-5F            & \ \ \ \ \ \ \ \ \}} $ * ) ; ^
     >         X'265C5C5C',X'5C5C5C5C',X'5C5C5D24',X'2A293B5E',
C60-6F            - / \ \ \ \ \ \ \ \ ! , % - > ?
     >         X'2D2F5C5C',X'5C5C5C5C',X'5C5C7C2C',X'255F3E3F',
C70-7F            \ \ \ \ \ \ \ \ \ \ : # @ ' = "
     >         X'5C5C5C5C',X'5C5C5C5C',X'5C603A23',X'40273D22',
C80-8F            \ a b c d e f g h i \ \ \ \ \ \ x
     >         X'5C616263',X'64656667',X'68695C5C',X'5C5C5C5C',
C90-9F            \ j k l m n o p q r \ \ \ \ \ \ x
     >         X'5C6A6B6C',X'6D6E6F70',X'71725C5C',X'5C5C5C5C',
CA0-AF            \ ~ s t u v w x y z \ \ \ \ \ \ x
     >         X'5C7E7374',X'75767778',X'797A5C5C',X'5C5C5C5C',
CB0-BF            \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ x
     >         X'5C5C5C5C',X'5C5C5C5C',X'5C5C5C5C',X'5C5C5C5C',
CC0-CF            { A B C D E F G H I \ \ \ \ \ \ x
     >         X'7B414243',X'44454647',X'48495C5C',X'5C5C5C5C',
CD0-DF            } J K L M N O P Q R \ \ \ \ \ \ x
     >         X'7D4A4B4C',X'4D4E4F50',X'51525C5C',X'5C5C5C5C',
CE0-CEF           \ \ S T U V W X Y Z \ \ \ \ \ \ x
     >         X'5C5C5354',X'55565758',X'595A5C5C',X'5C5C5C5C',
C                                               D
C                                               E
C                 0 1 2 3 4 5 6 7 8 9 \ \ \ \ \ L
     >         X'30313233',X'34353637',X'38395C5C',X'5C5C5CFF'/
C
C
#else
C
      DATA (TABDAT(K),K=0,15)
C                 N S S F       D
C                 U O T T   H   E       V F C S S
C00-0F            L H X X \ T \ L \ \ \ T F R O I
     >        /X'000102035C095C7F5C5C5C0B0C0D0E0F',
C                 D D D D         C
C                 L C C C     B   A E     F G R U
C10-1F            E 1 2 3 \ \ S \ N M \ \ S S S S
     >         X'101112135C5C085C18195C5C1C1D1E1F',
C                             E E           E A B
C                           L T S           N C E
C20-2F            \ \ \ \ \ F B C \ \ \ \ \ Q K L
     >         X'5C5C5C5C5C0A171B5C5C5C5C5C050607',
C                     S         E         D N   S
C                     Y         O         C A   U
C30-3F            \ \ N \ \ \ \ T \ \ \ \ 4 K \ B
     >         X'5C5C165C5C5C5C045C5C5C5C14155C1A',
C40-4F           SP \ \ \ \ \ \ \ \ \{{ . < ( + ]
     >         X'205C5C5C5C5C5C5C5C5C5B2E3C282B21',
C50-5F            & \ \ \ \ \ \ \ \ \}} $ * ) ; ^
     >         X'265C5C5C5C5C5C5C5C5C5D242A293B5E',
C60-6F            - / \ \ \ \ \ \ \ \ ! , % - > ?
     >         X'2D2F5C5C5C5C5C5C5C5C7C2C255F3E3F',
C70-7F            \ \ \ \ \ \ \ \ \ \ : # @ ' = "
     >         X'5C5C5C5C5C5C5C5C5C603A2340273D22',
C80-8F            \ a b c d e f g h i \ \ \ \ \ \ x
     >         X'5C6162636465666768695C5C5C5C5C5C',
C90-9F            \ j k l m n o p q r \ \ \ \ \ \ x
     >         X'5C6A6B6C6D6E6F7071725C5C5C5C5C5C',
CA0-AF            \ ~ s t u v w x y z \ \ \ \ \ \ x
     >         X'5C7E737475767778797A5C5C5C5C5C5C',
CB0-BF            \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ x
     >         X'5C5C5C5C5C5C5C5C5C5C5C5C5C5C5C5C',
CC0-CF            { A B C D E F G H I \ \ \ \ \ \ x
     >         X'7B4142434445464748495C5C5C5C5C5C',
CD0-DF            } J K L M N O P Q R \ \ \ \ \ \ x
     >         X'7D4A4B4C4D4E4F5051525C5C5C5C5C5C',
CE0-CEF           \ \ S T U V W X Y Z \ \ \ \ \ \ x
     >         X'5C5C535455565758595A5C5C5C5C5C5C',
C                                               D
C                                               E
C                 0 1 2 3 4 5 6 7 8 9 \ \ \ \ \ L
     >         X'303132333435363738395C5C5C5C5CFF'/
C
C
#endif


      DO 100 I = 1, LENGTH
        STRING(I:I)=TABLE(ICHAR(STRING(I:I)))
 100  CONTINUE

      RETURN
      END
