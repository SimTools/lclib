C   05/07/85 803111423  MEMBER NAME  AMBRD    (FORT)     M  FORTRAN
C
      SUBROUTINE AMBRD(TABL,NTAB,ELEM,IT)
C
      CHARACTER*(*) TABL(NTAB),ELEM
      CHARACTER TEST*64,COMP*64,TUPP*64
C
C  String maching routine, return the table element
C
      NCHA = LEN(ELEM)
      NCHE = 0
      COMP = ELEM(1:NCHA)
      CALL STRUPC( NCHA,COMP )
      NCHE = LKBRD(COMP(1:NCHA),0)
C
C COMPARE
C
      NPAR = 0
      DO 10 I=1,NTAB
        TEST = TABL(I)(1:NCHA)
        CALL STRUPC( NCHA,TEST )
        NTMP = LKBRD(TEST(1:NCHA),0)
        IF(COMP(1:NCHA).EQ.TEST(1:NCHA)) THEN
          IT=I
          ELEM=TABL(IT)
          RETURN
        ENDIF
        TUPP=TEST
        TEST(1:NCHE) = COMP(1:NCHE)
        IF(TEST(1:NCHA).EQ.TUPP(1:NCHA)) THEN
          NPAR = NPAR+1
          IPAR=I
        ENDIF
10    CONTINUE
C
C   Match(It>0), no match (It=0) ambiguous (It<0)
C
      IF(NPAR.EQ.1) THEN
        IT = IPAR
        ELEM=TABL(IT)
        RETURN
      ELSE
        IT=0
        IF(NPAR.EQ.0) RETURN
        IT = -1
      ENDIF
      RETURN
      END
