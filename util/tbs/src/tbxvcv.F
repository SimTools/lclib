C
C----------
C TBxVCV
C----------
C
      SUBROUTINE TBxVCV ( IDREC, BANK, NELM,
     >                    NWDS, CBUF, HBUF, IBUF, RBUF, DEBUG )
C
C ARGUMENTS
C
      INTEGER * 4      IDREC
      CHARACTER * (*)  BANK
      INTEGER * 4      NELM
      INTEGER * 4      NWDS
C
      CHARACTER * 1    CBUF(4,*)
      INTEGER * 2      HBUF(2,*)
      INTEGER * 4      IBUF(*)
      REAL * 4         RBUF(*)
C
      INTEGER * 4      DEBUG
C
C VARIABLES
C
      REAL * 4         RCNV ( 2 )
      REAL * 8         DCNV
      EQUIVALENCE      ( RCNV, DCNV )
C
      INTEGER * 4      I, PNTR(2)
      INTEGER * 4      TYPE
C
C FUNCTIONS
C
      INTEGER * 4      TBxVDT
C
C MAIN
C
C     PRINT *, '*** VCV, NELM = ', NELM
      CALL TBxVPT ( IDREC, BANK, NELM, PNTR )
      IF ( PNTR(1) .EQ. 0 ) THEN
        PRINT *, '%TBWVAX-F, Error in VAX Format Definition!!'
        STOP
      END IF
      I = 1
      DO 10 WHILE ( I .LE. NWDS )
        TYPE = TBxVDT ( PNTR, I )
        IF ( TYPE .EQ. 1 ) THEN
          IF ( DEBUG .EQ. 1 )
     >      PRINT '(1X,A,4A1)', 'EBCDIC Conversion:',(CBUF(JJ,I),JJ=1,4)
          CALL UCEBC ( 4, CBUF(1,I) )
          I = I + 1
        ELSE IF ( TYPE .EQ. 2 ) THEN
          IF ( DEBUG .EQ. 1 )
     >      PRINT *, 'Int*2 Conversion: ', HBUF(1,I)
#ifdef  VAX
     CALL TBxSW2 ( HBUF(1,I), 2 )
#endif
          I = I + 1
        ELSE IF ( TYPE .EQ. 3 ) THEN
          IF ( DEBUG .EQ. 1 )
     >      PRINT *, 'Int*4 Conversion: ', IBUF(I)
#ifdef  VAX
     CALL TBxSW4 ( IBUF(I), 1 )
#endif
          I = I + 1
        ELSE IF ( TYPE .EQ. 4 ) THEN
          IF ( DEBUG .EQ. 1 )
     >      PRINT *, 'R*4 Conversion: ', RBUF(I)
          CALL TBxR4U ( RBUF(I), 1 )
          I = I + 1
        ELSE IF ( TYPE .EQ. 5 ) THEN
          RCNV ( 1 ) = RBUF ( I )
          RCNV ( 2 ) = RBUF ( I+1 )
          IF ( DEBUG .EQ. 1 )
     >      PRINT *, 'R*8 Conversion: ', DCNV
          CALL TBxR8U ( DCNV, 1 )
          RBUF ( I ) = RCNV ( 1 )
          RBUF ( I+1 ) = RCNV ( 2 )
          I = I + 2
        END IF
10    CONTINUE
      RETURN
      END
