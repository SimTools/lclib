C   11/11/86 701091822  MEMBER NAME  COMTST   (FORT)     M  FORTRAN
C
      INTEGER   * 4  NVAL   / 5 /
      INTEGER   * 4  VALUEI( 5 )
      REAL      * 4  VALUER( 5 )
      LOGICAL   * 4  VALUEL( 5 )
      EQUIVALENCE( VALUEI,VALUER,VALUEL )
      CHARACTER * 5  VARTYP / 'IIRRL' /
      CHARACTER * 30 HELP ( 5 )
      DATA  HELP   / 'korewa ichi', 'ni', 'san', 'yon', 'go desuyo'/
      CHARACTER * 5  NAMES ( 5 ) / 'ONE', 'TWO', 'THREE', 'FOUR',
     #                             'FIVE' /
      INTEGER * 4    NREPL, LANS(0:5)
      CHARACTER * 3  ANS
       include 'comode.inc'
      CALL INICOM ( 'COM' )
C
      VALUEI( 1 ) = 1
      VALUEI( 2 ) = 2
      VALUER( 3 ) = -121.0
      VALUER( 4 ) = 0.0
      VALUEL( 5 ) = .TRUE.
      CALL CHVAL( 'CHVAL',NVAL,NAMES,HELP,VARTYP,VALUEI )
      STOP
      END
