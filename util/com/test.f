C   04/11/87 711041655  MEMBER NAME  TEST     (FORT)     M  FORTRAN
C
C CONST
      PARAMETER( MAXVAL = 6 )
C
C VAR
      INTEGER   *  4  NVAL
      CHARACTER * 16  NAMES( MAXVAL )
      CHARACTER * 64  HELP ( MAXVAL )
C
      DATA NVAL  / MAXVAL /
      DATA NAMES / 'ROOTS',
     &             'LAMBDA',
     &             'Q0SQUARE',
     &             'NFLAVOR',
     &             'DELTA',
     &             'SCHEME' /
C
      REAL    * 4  RARRAY( MAXVAL )
      INTEGER * 4  IARRAY( MAXVAL )
      EQUIVALENCE( RARRAY,IARRAY )
C
C BEGIN
      RARRAY(1) = ROOTS
      RARRAY(2) = FLAMD
      RARRAY(3) = Q02
      IARRAY(4) = NFLAV
      RARRAY(5) = DELTA
      RARRAY(6) = SCHEME
C
      CALL CHVAL( 'NLL-QCD Generater - Set Parameters',
     &            NVAL,NAMES,HELP,'R(5)RRI(10)RR',RARRAY )
C
      ROOTS = RARRAY(1)
      FLAMD = RARRAY(2)
      Q02 = RARRAY(3)
      NFLAV = IARRAY(4)
      DELTA = RARRAY(5)
      SCHEME = RARRAY(6)
C
      STOP
      END
