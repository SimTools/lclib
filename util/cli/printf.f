C   18/06/88 806192010  MEMBER NAME  PRINTF   (FORT)     M  FORTRAN
C
C PRINTF : A.Shirahashi, Univ. of Tokyo
C
      SUBROUTINE PRINTF( LUN, S,
     &                   V1,V2,V3,V4,V5,V6,V7,V8,V9 )
C
C ARG
      INTEGER * 4  LUN
      CHARACTER * (*)  S
      REAL * 4  V1, V2, V3, V4, V5, V6, V7, V8, V9
C
C VAR
      REAL * 4  ARGV(9)
      REAL * 4  ARGI(9)
      EQUIVALENCE( ARGV,ARGI )
      CHARACTER * 132  SO
      CHARACTER * 16  STRING
C
C FUNC
      INTEGER * 4  NARG, LENRD
C
C BEGIN
      N = MIN( NARG(0)-2,9 )
      IF( N.GE.1 ) ARGV(1) = V1
      IF( N.GE.2 ) ARGV(2) = V2
      IF( N.GE.3 ) ARGV(3) = V3
      IF( N.GE.4 ) ARGV(4) = V4
      IF( N.GE.5 ) ARGV(5) = V5
      IF( N.GE.6 ) ARGV(6) = V6
      IF( N.GE.7 ) ARGV(7) = V7
      IF( N.GE.8 ) ARGV(8) = V8
      IF( N.GE.9 ) ARGV(9) = V9
C
      IS = 0
      IO = 0
      IV = 0
100   CONTINUE
      IF( IS.GE.LEN(S) ) GOTO 800
      IP = INDEX( S(IS+1:),'%' )
      IF( IP.EQ.0 ) GOTO 800
      IF( IP.GE.2 ) THEN
        SO(IO+1:IO+IP-1) = S(IS+1:IS+IP-1)
        IO = IO + IP - 1
      END IF
      IN = IS+IP+1
      IN0 = IN
200   CONTINUE
      IF( INDEX( '.0123456789',S(IN:IN) ) .GT. 0 ) THEN
        IN = IN + 1
        GOTO 200
      END IF
      IF( S(IN:IN).EQ.'f' .OR. S(IN:IN).EQ.'F' ) THEN
        IF( IN.EQ.IN0 ) THEN
          NW = 0
          ND = 6
        ELSE
          INP = INDEX( S(IN0:IN-1),'.' )
          IF( INP.EQ.0 ) THEN
            READ( S(IN0:IN-1),* ) NW
            ND = 6
          ELSE
            IF( INP.EQ.1 ) THEN
              NW = 0
            ELSE
              READ( S(IN0:IN0+INP-2),* ) NW
            END IF
            READ( S(IN0+INP:IN-1),* ) ND
          END IF
        END IF
        IV = IV + 1
        CALL CLVTOS( ARGV(IV),STRING,NW,ND )
        L = LENRD( STRING )
        IF( NW.EQ.0 ) THEN
          NW = L
        END IF
        IF( NW.GT.L ) THEN
          SO(IO+1:IO+NW-L) = ' '
          SO(IO+NW-L+1:IO+NW) = STRING(:L)
        ELSE
          SO(IO+1:IO+L) = STRING(:L)
        END IF
        IO = IO + NW
      ELSE
        IO = IO + 1
        SO(IO:IO) = S(IN:IN)
      END IF
      IS = IN
      GOTO 100
800   CONTINUE
      L = LEN( S ) - IS
      IF( L.GT.0 ) THEN
        SO(IO+1:IO+L) = S(IS+1:IS+L)
        IO = IO + L
      END IF
      WRITE( LUN,* ) SO(:IO)
C
      RETURN
      END
