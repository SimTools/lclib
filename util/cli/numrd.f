C   17/06/86 711302152  MEMBER NAME  NUMRD    (FORT)     M  FORTRAN
C
       SUBROUTINE NUMRD(F,IRAD,K)
C
       REAL*8 F,RES,UNIT,BASE
       LOGICAL CHANGD,TERMIN,EXPON,FRCTN
       CHARACTER K,K2,DIGIT*16
       DATA DIGIT /'0123456789ABCDEF'/
C
C..... DECODE REAL*8 FLOATING
C
       CHANGD = .FALSE.
       TERMIN = .FALSE.
       EXPON = .FALSE.
       FRCTN = .FALSE.
       SIG = 1.
       RES = 0.
       IRES = 0
       ISIG = 1
       UNIT = 1.
3      CALL CHARD(K,K2)
CCC     CALL STR$UPCASE(K,K)
       IF (K.EQ.'\\') RETURN
       IF (K.EQ.',' .OR. K.EQ.CHAR(13)) GOTO 7
       IF (K.EQ.' ' .AND. CHANGD) GOTO 9
       IF (K.EQ.' ' .AND. .NOT.CHANGD) GOTO 3
       IF (TERMIN .OR. K.EQ.'+') GOTO 3
       M = INDEX(DIGIT(1:IRAD),K)-1
       IF (EXPON) GOTO 4
       IF (M.GE.0) GOTO 11
       IF (K.EQ.'-') THEN
         CHANGD = .TRUE.
         SIG = -1.
         GOTO 3
       ELSE IF (K.EQ.'.') THEN
         CHANGD = .TRUE.
         FRCTN = .TRUE.
         GOTO 3
       END IF
       IF (CHANGD .AND. (K.EQ.'D' .OR. K.EQ.'E')) THEN
         EXPON = .TRUE.
         GOTO 3
       END IF
       IF (.NOT.CHANGD) GOTO 3
13     TERMIN = .TRUE.
       GOTO 3
11     CHANGD = .TRUE.
       IF (.NOT.FRCTN) GOTO 8
       UNIT = UNIT/IRAD
       RES = RES+UNIT*M
       GOTO 3
8      RES = RES*IRAD+M
       GOTO 3
4      IF (K.EQ.'-') THEN
         ISIG = -1
         GOTO 3
       END IF
       IF (M.LT.0) GOTO 13
       IRES = IRES*IRAD+M
       GOTO 3
9      IF (K2.NE.' ') GOTO 7
       CALL CHARD(K,K2)
       GOTO 9
7      BASE = IRAD
       RES = SIG*RES*BASE**(ISIG*IRES)
       IF (CHANGD) F = RES
       RETURN
       END
