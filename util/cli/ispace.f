C   12/11/86 611122051  MEMBER NAME  ISPACE   (FORT)     M  FORTRAN
C
      LOGICAL FUNCTION ISPACE( C )
C
C ARG
C     (Input)
      CHARACTER * ( * )  C
C
C CONST
      CHARACTER * 10  NUMBER
      CHARACTER * 26  UALPHA
      CHARACTER * 26  LALPHA
      CHARACTER * 33  SYMBOL
C
      DATA NUMBER / '0123456789' /
      DATA UALPHA / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      DATA LALPHA / 'abcdefghijklmnopqrstuvwxyz' /
      DATA SYMBOL / '-^$@;: ,./|!"#\%&''() =~|`{+*}<>?_' /
C
C BEGIN
      IF( INDEX( UALPHA//LALPHA//NUMBER//SYMBOL,C(1:1) ) .EQ. 0 ) THEN
        ISPACE = .TRUE.
      ELSE
        ISPACE = .FALSE.
      END IF
C
      RETURN
      END
