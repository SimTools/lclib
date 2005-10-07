C   04/07/85 810252331  MEMBER NAME  TVCLR    (FORT)     M  FORTRAN
C
C   TVCLR  -- Miscellaneous Terminal I/O routines --
C
C     SUBROUTINE TVCLR
C     ENTRY TVHOME
C     SUBROUTINE TVLOC( ROW,COL )
C     SUBROUTINE TVLOC1( ROW, COL )
C     SUBROUTINE TVPROM( STRING )
C     SUBROUTINE TVEDIT( STRING, BUFFER, LENGTH, AID )
C     SUBROUTINE TVPEDT( STRING,BUFFER,LENGTH )
C     SUBROUTINE TVCEOL( ROW )
C     SUBROUTINE TVADRS( ROW,COL,BAH,BAL )
C     SUBROUTINE TVSRDA( BAH,BAL,ROW,COL )
C     SUBROUTINE TVPUTL( MODE,BUF,LBUF,/ROW/,/COL/,/TEXT/ )
C     SUBROUTINE TVINPT( AID,CSRPOS,
C     SUBROUTINE TVMLIN( ROW,COL,FIELD1,FIELD2 )
C
C
      SUBROUTINE TVCLR
C
      CHARACTER STR*8
      LOGICAL*1 LSTR(8)
      EQUIVALENCE (LSTR,STR)
CCC   DATA LSTR/Z43,Z11,Z40,Z40,Z3C,Z5D,Z7F,Z00/
CMSP      DATA LSTR/Z40,Z11,Z40,Z40,Z3C,Z5D,Z7F,Z00/
C
C     PRINT *, '*TVCLR'
      CALL TPUT(STR,8,'F,W',IRET)
C%%%  PRINT *, '***** TVCLR *****'
      RETURN
C
C
      ENTRY TVHOME
C
C     PRINT *, '*TVHOME'
      CALL TPUT(STR,4,'F,W',IRET)
      RETURN
      END
C
CC**********************************************************************
C*  SUBROUTINE TVLOC
CC**********************************************************************
C
      SUBROUTINE TVLOC( ROW,COL )
C
C ... Locate Cursor
C     by A.Shirahashi, 3-DEC-1986
C
C ARG
      INTEGER  ROW,COL
C
C COOMON
       include '_tvdisp.inc'
      EXTERNAL TVBLK
C
C BEGIN
#if defined(HIUXF77) || defined(__GFORTRAN__)
      CALL TVBLK
#endif
C     PRINT *, '*TVLOC'
      ROWS = MIN( ROW + 1, 24 )
      COLS = MIN( COL + 1, 79 )
      CALL TVLOC1( ROWS, COLS )
      LTVLOC = .TRUE.
C
      RETURN
      END
C
CC**********************************************************************
C*  SUBROUTINE TVLOC1
CC**********************************************************************
C
      SUBROUTINE TVLOC1( ROW, COL )
C
C ARG
      INTEGER * 4  ROW, COL
C
C CONST
       include '_tvcmd.inc'
C
C VAR
      LOGICAL * 1  LBUF(5)
      CHARACTER * 5  CBUF
      EQUIVALENCE( CBUF,LBUF )
      INTEGER * 4  LOC, L1, L2
C
C BEGIN
C     PRINT *, '*TVLOC1'
C     ... write control code
      LBUF(1) = LWCC
C     ... set buffer address
      LBUF(2) = LSBA
      CALL TVADRS( ROW,COL,LBUF(3),LBUF(4) )
C     ... insert cursor
      LBUF(5) = LIC
C
      CALL TPUT( CBUF,5,'F,W',IRET )
C
      RETURN
      END
C
CC**********************************************************************
C*  SUBROUTINE TVPROM
CC**********************************************************************
C
      SUBROUTINE TVPROM( STRING )
C
C ... put a prompt string on terminal
C     3-DEC-86, A.Shirahashi
C
C ARG
      CHARACTER * (*)  STRING
C
C COOMON
       include '_tvdisp.inc'
      EXTERNAL TVBLK
C
C CONST
       include '_tvcmd.inc'
C
C VAR
      CHARACTER * 80  WORKS
      INTEGER   *  4  LS, LP
C
C BEGIN
#if defined(HIUXF77) || defined(__GFORTRAN__)
      CALL TVBLK
#endif

C     PRINT *, '*TVPROM'
C     ... locate cursor
      IF( LTVLOC ) CALL TVLOC1( ROWS,COLS )
C     ... put a prompt string
      LS = 0
      WORKS(LS+1:) = WCC
      LS = LS + 1
      WORKS(LS+1:) = SF//PH
      LS = LS + 2
      LP = LEN( STRING )
      IF( LP .GT. 72 ) LP = 72
      WORKS(LS+1:) = STRING(1:LP)
      LS = LS + LP
      WORKS(LS+1:) = SF//UNPL
      LS = LS + 2
      WORKS(LS+1:) = IC
      LS = LS + 1
      CALL TPUT( WORKS(1:LS),LS,'F,W',IRET )
      CALL TVCEOL( ROWS )
      RETURN
C
      END
C
CC**********************************************************************
C*  SUBROUTINE TVEDIT
CC**********************************************************************
C
      SUBROUTINE TVEDIT( STRING, BUFFER, LENGTH, AID )
C
C ... put a string in edit mode
C     27-JAN-87, A.Shirahashi
C     10-FEB-88, A.Shirahashi, return AID code
C
C ARG
C     (Input)
      CHARACTER * (*)  STRING
C     (Input/Output)
      CHARACTER * (*)  BUFFER
      INTEGER   *  4   LENGTH
C     (Output)
      INTEGER   *  4   AID
C
C COOMON
       include '_tvdisp.inc'
      EXTERNAL TVBLK
C
C CONST
       include '_tvcmd.inc'
C
C VAR
      INTEGER   *  4  CSRPOS(2), NFIELD, FLDPOS(2)
C
C BEGIN
#if defined(HIUXF77) || defined(__GFORTRAN__)
      CALL TVBLK
#endif

C     PRINT *, '*TVEDIT'
C     ... locate cursor
      IF( LTVLOC ) CALL TVLOC1( ROWS,COLS )
C     ... put a string
      CALL TVPEDT( STRING,BUFFER,LENGTH )
      CALL TVINPT( AID,CSRPOS,1,NFIELD,FLDPOS,LENGTH,BUFFER )
      RETURN
C
      END
C
C
      SUBROUTINE TVPEDT( STRING,BUFFER,LENGTH )
C
C ARG
      CHARACTER * (*)  STRING
      CHARACTER * (*)  BUFFER
      INTEGER   *  4   LENGTH
C
C COOMON
       include '_tvdisp.inc'
      EXTERNAL TVBLK
C
C CONST
       include '_tvcmd.inc'
C
C VAR
      CHARACTER * 80  WORKS
C
C BEGIN
#if defined(HIUXF77) || defined(__GFORTRAN__)
      CALL TVBLK
#endif

C     PRINT *, '*TVPEDT'
      LS = 0
      WORKS(LS+1:) = WCC
      LS = LS + 1
      WORKS(LS+1:) = SF//PH
      LS = LS + 2
      LP = LEN( STRING )
      IF( LP .GT. 72 ) LP = 72
      WORKS(LS+1:) = STRING(1:LP)
      LS = LS + LP
      WORKS(LS+1:) = SF//UNPL
      LS = LS + 2
      WORKS(LS+1:) = IC
      LS = LS + 1
      WORKS(LS+1:) = BUFFER( 1:LENGTH )
      LS = LS + LENGTH
      WORKS(LS+1:LS+1) = RA
      LS = LS + 1
      CALL TVADRS( ROWS,80,WORKS(LS+1:LS+1),WORKS(LS+2:LS+2) )
      LS = LS + 2
      WORKS(LS+1:LS+1) = NULL
      LS = LS + 1
      CALL TPUT( WORKS(1:LS),LS,'F,W',IRET )
      RETURN
      END
C
CC**********************************************************************
C*  SUBROUTINE TVCEOL
CC**********************************************************************
C
      SUBROUTINE TVCEOL( ROW )
C
C ARG
      INTEGER * 4  ROW, COL
C
C CONST
       include '_tvcmd.inc'
C
C VAR
      CHARACTER * 5  CBUF
      LOGICAL   * 1  LBUF(5)
      EQUIVALENCE( CBUF,LBUF )
C
C BEGIN
C     PRINT *, '*TVCEOL'
      LS = 0
      LBUF(LS+1) = LWCC
      LS = LS + 1
      LBUF(LS+1) = LRA
      LS = LS + 1
      CALL TVADRS( ROW,80,LBUF(LS+1),LBUF(LS+2) )
      LS = LS + 2
      LBUF(LS+1) = LNULL
      LS = LS + 1
      CALL TPUT( CBUF(1:LS),LS,'F,W',IRET )
      RETURN
      END
C
CC**********************************************************************
C*  SUBROUTINE TVADRS
CC**********************************************************************
C
      SUBROUTINE TVADRS( ROW,COL,BAH,BAL )
C
C ARG
      INTEGER * 4  ROW, COL
      LOGICAL * 1  BAH, BAL
C
C CONST
      LOGICAL * 1  BAT(0:63)
C
CMSP      DATA BAT / Z40,ZC1,ZC2,ZC3,ZC4,ZC5,ZC6,ZC7,ZC8,ZC9,
CMSP     &           Z4A,Z4B,Z4C,Z4D,Z4E,Z4F,Z50,ZD1,ZD2,ZD3,
CMSP     &           ZD4,ZD5,ZD6,ZD7,ZD8,ZD9,Z5A,Z5B,Z5C,Z5D,
CMSP     &           Z5E,Z5F,Z60,Z61,ZE2,ZE3,ZE4,ZE5,ZE6,ZE7,
CMSP     &           ZE8,ZE9,Z6A,Z6B,Z6C,Z6D,Z6E,Z6F,ZF0,ZF1,
CMSP     &           ZF2,ZF3,ZF4,ZF5,ZF6,ZF7,ZF8,ZF9,Z7A,Z7B,
CMSP     &           Z7C,Z7D,Z7E,Z7F /
C
C VAR
      INTEGER * 4  LOC, L1, L2
C
C BEGIN
C     PRINT *, '*TVADRS'
      LOC = (ROW - 1) * 80 + COL - 1
      L1 = LOC / 64
      L2 = LOC - L1 * 64
      BAH = BAT(L1)
      BAL = BAT(L2)
C
      RETURN
      END
C
C
      SUBROUTINE TVSRDA( BAH,BAL,ROW,COL )
C
C ARG
      LOGICAL * 1  BAH, BAL
      INTEGER * 4  ROW, COL
C
C BEGIN
C     PRINT *, '*TVSRDA'
CMSP      L1 = MOD( BAH,16 ) + ( MOD( BAH/16,8 ) - 4 ) * 16
CMSP      L2 = MOD( BAL,16 ) + ( MOD( BAL/16,8 ) - 4 ) * 16
CMSP      LOC = L1 * 64 + L2
CMSP      ROW = LOC / 80
CMSP      COL = LOC - ROW * 80
CMSP      ROW = ROW + 1
CMSP      COL = COL + 1
      RETURN
      END
C
C
      SUBROUTINE TVPUTL( MODE,BUF,LBUF,ROW,COL,TEXT )
C
C ARG
      INTEGER * 4  ROW, COL, LBUF
      CHARACTER * ( * )  MODE, TEXT, BUF
C
C CONST
       include '_tvcmd.inc'
C
C BEGIN
C     PRINT *, '*TVPUTL'
      IF( MODE .EQ. 'WCC' ) THEN
        BUF(1:1) = WCC
        LBUF = 1
        RETURN
      ELSE IF( MODE .EQ. 'PUT' ) THEN
        CALL TPUT( BUF,LBUF,'F,W',IRET )
        RETURN
      END IF
C
      BUF(LBUF+1:LBUF+1) = SBA
      LBUF = LBUF + 1
      IF( MODE .EQ. 'NONE' ) THEN
        CALL TVADRS( ROW,COL+1,BUF(LBUF+1:LBUF+1),BUF(LBUF+2:LBUF+2) )
        LBUF = LBUF + 2
      ELSE
        CALL TVADRS( ROW,COL,BUF(LBUF+1:LBUF+1),BUF(LBUF+2:LBUF+2) )
        LBUF = LBUF + 2
        BUF(LBUF+1:LBUF+1) = SF
        LBUF = LBUF + 1
        IF( MODE .EQ. 'UL' ) THEN
          BUF(LBUF+1:LBUF+1) = UNPL
        ELSE IF( MODE .EQ. 'UH' ) THEN
          BUF(LBUF+1:LBUF+1) = UNPH
        ELSE IF( MODE .EQ. 'PL' ) THEN
          BUF(LBUF+1:LBUF+1) = PL
        ELSE IF( MODE .EQ. 'PH' ) THEN
          BUF(LBUF+1:LBUF+1) = PH
        END IF
        LBUF = LBUF + 1
      END IF
      BUF(LBUF+1:LBUF+LEN(TEXT)) = TEXT
      LBUF = LBUF + LEN(TEXT)
C
      RETURN
      END
C
C
      SUBROUTINE TVINPT( AID,CSRPOS,
     &                   MAXSIZ,NFIELD,FLDPOS,LENGTH,BUFFER )

C
C ARG
      INTEGER   *  4   AID, CSRPOS(2), NFIELD
      INTEGER   *  4   FLDPOS(2,MAXSIZ), LENGTH(MAXSIZ)
      CHARACTER * (*)  BUFFER(MAXSIZ)
C
C CONST
       include '_tvcmd.inc'
C
C VAR
      CHARACTER * 1024  CBUF
#if defined(HIUXF77) || defined(__GFORTRAN__)
      LOGICAL      * 1     LBUF(1024)
#else
      BYTE      * 1     LBUF(1024)
#endif
      EQUIVALENCE( CBUF,LBUF )
C
C BEGIN
C     PRINT *, '*TVINPT'
      CALL TGET( CBUF,1024,'A,W',IRET,LAST )
#if defined(HIUXF77) || defined(__GFORTRAN__)
      AID = LBUF(1)
#else
      AID = INT(LBUF(1))
#endif
      CALL TVSRDA( LBUF(2),LBUF(3),CSRPOS(1),CSRPOS(2) )
C
      NFIELD = 0
      IP = 4
      IF( IP.GT.LAST ) RETURN
C
100   CONTINUE
      NFIELD = NFIELD + 1
      CALL TVSRDA( LBUF(IP+1),LBUF(IP+2),
     &             FLDPOS(1,NFIELD),FLDPOS(2,NFIELD) )
      IP = IP + 3
      IB = 1
      LENGTH(NFIELD) = 0
      BUFFER(NFIELD) = ' '
C
110   CONTINUE
      IF( IP.GT.LAST ) RETURN
      IF( CBUF(IP:IP).EQ.SBA  ) GOTO 100
      BUFFER(NFIELD)(IB:IB) = CBUF(IP:IP)
      LENGTH(NFIELD) = IB
      IB = IB + 1
      IP = IP + 1
      GOTO 110
C
      END
C
C
      SUBROUTINE TVMLIN( ROW,COL,FIELD1,FIELD2 )
C
      INTEGER * 4  ROW,COL
      CHARACTER * (*)  FIELD1, FIELD2
C
C CONST
       include '_tvcmd.inc'
C
C VAR
      CHARACTER * 1  BAH1,BAL1,BAH2,BAL2
      CHARACTER * 80  WORKS
C
C BEGIN
C     PRINT *, '*TVMLIN'
      CALL TVADRS( ROW,COL,BAH1,BAL1 )
      CALL TVADRS( ROW,COL+LEN(FIELD1)+1,BAH2,BAL2 )
      WORKS = WCC//SBA//BAH1//BAL1//SF//UNPL//FIELD1
     &           //SBA//BAH2//BAL2//SF//PH//FIELD2
      L = 11 + LEN( FIELD1 ) + LEN( FIELD2 )
      CALL TPUT( WORKS,L,'F,W',IRET )
      RETURN
      END
C
#if defined(HIUXF77) || defined(__GFORTRAN__)
      SUBROUTINE TVBLK
#else
      BLOCK DATA TVBLK
#endif
C
       include '_tvdisp.inc'
C
      DATA LTVLOC / .FALSE. /
C
      END
