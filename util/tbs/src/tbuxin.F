C   10/07/89 306281108  MEMBER NAME  TBUXIN   (FORT)     M  FORTRAN
C+
C File : TBUXIN
C Description : initialization for TBWUNX
C
C Author : Ryosuke Itoh, TOPAZ, KEK
C Date : 10 - JUL - 1989
C-
C
      SUBROUTINE TBUXIN(IU, MODE)
C
C (Input)
C  IU    : Unit # to read MAP information
C  MODE  : = 1 to list map.
C
C COMMON
C
#include "tbfmap.inc"
C
C VARIABLES
C
      CHARACTER * 80   LINBUF
      INTEGER * 4      LBUF
      INTEGER * 4      NR, NE
      INTEGER * 4      I, J, K
      LOGICAL * 4      INOPEN
C
C MAIN
C
      INQUIRE ( UNIT=IU, OPENED=INOPEN )
      IF ( .NOT. INOPEN ) THEN
C*        PRINT *, '#### Standard Mapping File assumed.'
#ifdef  MSP
        OPEN ( UNIT=IU, FILE='T#PR.TBSTOVAX.FORMAT(VAXBANK)',
     >         STATUS='SHR',ACTION='READ')
#endif
#ifdef  UNIX
#ifndef HIUXF77
             CALL GETENV('TBSTOUNX_MAPFILE',LINBUF)
#else
	     call getenv('TBSTOUNX_MAPFILE',16,LINBUF,80)
#endif
             IF( LINBUF(1:3).EQ.'   ') THEN

#endif
#ifdef  SUN
        LINBUF ='/topaz2/toptape/TOPAZLIB'//
     >         '/TBS/TBSTOUNX/VAXBANK.format'
#endif
#ifdef  HP
         LINBUF = '/proj/phys/lclib/95a/util/tbs/VAXBANK.format'
#endif
#ifdef  MACHTEN_PPC
         LINBUF = '/proj/phys/lclib/pro/util/tbs/VAXBANK.format'
#endif
#ifdef  HIUXF77
         LINBUF = '/.../ccjlc.kek.jp/fs/g/ofl1/lclib/'//
     >            '96a/util/tbs/VAXBANK.format'
#endif
            ENDIF
#ifdef  HP
        OPEN(UNIT=IU,FILE=LINBUF, STATUS='OLD',READONLY)
#endif
#ifdef  SUN
        OPEN(UNIT=IU,FILE=LINBUF, STATUS='OLD')
#endif
#ifdef  MACHTEN_PPC
        OPEN(UNIT=IU,FILE=LINBUF, STATUS='OLD')
#endif
#ifdef  HIUXF77
        OPEN(UNIT=IU,FILE=LINBUF, STATUS='OLD')
#endif
#ifdef  UNIX
            WRITE(6,'(A,A)') ' TBUXIN .. Map file is',
#endif
#ifdef  UNIX
     >    LINBUF(:LNBLNK(LINBUF))
#endif
      ELSE
        PRINT *, '#### Mapping File was loaded from FT03.'
      END IF
10    CONTINUE
        READ ( IU, '(A)', END=99 ) LINBUF
CC      LBUF = LENRD ( LINBUF )
        LBUF = 80
        DO 100 I = LBUF, 1, -1
          IF( LINBUF(I:I).NE.' ') GO TO 110
100     CONTINUE
        LBUF = 1
110     LBUF = I + 1
        IF ( LINBUF(1:1) .EQ. '!' .OR. LINBUF(1:1) .EQ. ';' ) THEN
          GOTO 10
        ELSE IF ( LINBUF(1:1) .EQ. ' ' ) THEN
          NMAP = NMAP + 1
          READ ( LINBUF(2:LBUF), * ) MAPID(NMAP), MAPBNK(NMAP)
CCC       LBNK = LKBRD ( MAPBNK(NMAP), ' ' )
          CALL UCUPER ( 64, MAPBNK(NMAP) )
        ELSE IF ( LINBUF(1:1) .EQ. '+' ) THEN
          MAPMEL ( NMAP ) = MAPMEL ( NMAP ) + 1
          NE = MAPMEL ( NMAP )
          READ ( LINBUF(2:LBUF), * ) MAPELM(NMAP,NE)
        ELSE IF ( LINBUF(1:1) .EQ. '-' ) THEN
          NRANGE(NMAP,MAPMEL(NMAP)) = NRANGE(NMAP,MAPMEL(NMAP)) + 1
          NR = NRANGE ( NMAP, NE )
          READ ( LINBUF(2:LBUF), * ) (RANGE(NMAP,NE,NR,I),I=1,2),
     >                               MAPTYP(NMAP,NE,NR)
        ELSE
          PRINT *, 'Error in Reading Map File'
          PRINT *, LINBUF(1:LBUF)
          STOP
        END IF
      GOTO 10
99    CONTINUE
      CLOSE ( UNIT=IU )
C
      IF( MODE.NE.0 ) THEN
      PRINT *
      PRINT *, '********** UNIX MAP **********'
      PRINT *
      DO 11 I = 1, NMAP
        PRINT *, MAPID(I), MAPBNK(I)
        DO 20 J = 1, MAPMEL (I)
          PRINT *, '---- Element : ', MAPELM(I,J)
          DO 30 K = 1, NRANGE(I,J)
            PRINT *, RANGE(I,J,K,1), RANGE(I,J,K,2), MAPTYP(I,J,K)
30        CONTINUE
20      CONTINUE
11    CONTINUE
      ENDIF
 
      RETURN
      END
