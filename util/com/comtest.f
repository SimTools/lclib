C   11/11/86 612041907  MEMBER NAME  COMTEST  (FORT)     M  FORTRAN
C
      INTEGER * 4  NVAL   / 5 /
      INTEGER * 4     VALUES ( 5 ) / 1, 2, 3, 4, 5 /
      CHARACTER * 30 HELP ( 5 )
      DATA  HELP   / 'korewa ichi', 'ni', 'san', 'yon', 'go desuyo'/
      CHARACTER * 5  NAMES ( 5 ) / 'ONE', 'TWO', 'THREE', 'FOUR',
     #                             'FIVE' /
      INTEGER * 4    NREPL, LANS(0:5)
      CHARACTER * 3  ANS
       include 'comode.inc'
      CALL INICOM ( 'COM' )
      PRINT *, 'BATCH = ', BATCH
      CALL HLPLIB ( 'TRSI.COM.HELP' )
C
1     CONTINUE
      CALL MODVAL ( '*** MODVAL ***', NVAL, NAMES, '\COM', 'I', VALUES )
      PRINT *, 'BATCH = ', BATCH
      PRINT *, 'VALUE = ', VALUES
      DO 10 I = 1, 10000
        IF ( ICOMER .EQ. 2 ) GOTO 20
        IF ( MOD ( I, 100 ) .EQ. 0 ) PRINT *, 'EVENT = ', I
 10   CONTINUE
      CALL INQUIR ( '*** INQUIR ***', NVAL, NAMES, HELP, 1, LANS )
      PRINT *, 'BATCH = ', BATCH
      PRINT *, 'LANS = ', LANS
      IF ( LANS(1) .EQ. 5 ) STOP
      CALL SHOWIT ( '*** SHOWIT ***', NVAL, NAMES, HELP )
      PRINT *, 'BATCH = ', BATCH
      CALL INQUIR ( '*** INQUIR ***', NVAL, NAMES, HELP, 2, LANS )
      PRINT *, 'BATCH = ', BATCH
      PRINT *, 'LANS = ', LANS
      IF ( LANS(1) .EQ. 5 ) STOP
      CALL SHOWIT ( '*** SHOWIT ***', NVAL, NAMES, HELP )
      PRINT *, 'BATCH = ', BATCH
      GOTO 1
20    CONTINUE
      CALL TELLIT ( 'CTRL-C PRESSED........')
      ICTRL = ICTRL + 1
      IF ( ICTRL .GT. 10 ) STOP
      CALL TXTRD ( '?ARE YOU STOP YOUR PROCESS ', ANS )
      IF ( ANS(1:1) .EQ. 'N' ) GOTO 1
      STOP
      END
