C   11/09/87 709112135  MEMBER NAME  TESTHIST (FORT)     M  FORTRAN
C
      COMMON /HCOM/ H(30000)
C
      LOGICAL * 4  LSTAT, QOPEN
      CHARACTER * 44  DSNAME
C
      CALL HINIT(30000)
C
      CALL HDEF2( 1,'I*2',100,100,-10.0,-10.0,0.2,0.2,'TEST HIST@' )
      DO 100 I = -50,50
        DO 200 J = -50,50
          N = SQRT( 5000.0 ) - SQRT( ABS(FLOAT(I*I+J*J+10*I+10*J)) )
          DO 300 K = 1, N
            CALL HCUM2( 1,FLOAT(I)*0.2+0.1,FLOAT(J)*0.2+0.1,1.0 )
300       CONTINUE
200     CONTINUE
        PRINT *, I,J
100   CONTINUE
C
      LSTAT = QOPEN( '?Output File Name',1,DSNAME,
     &               'WRITE','UNFORMATTED','NEW' )
      CALL HWRITE( 'ALL ',1 )
      CLOSE( 1 )
      STOP
      END
