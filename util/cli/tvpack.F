C   03/07/85 905301936  MEMBER NAME  TVPACK   (FORT)     M  FORTRAN
C
C   TVPACK  -- Miscellaneous Terminal I/O routines --
C
C   This module should be compiled with option 'ALIAS CPRM 'NAME''
C
C     SUBROUTINE TVBGN
C     ENTRY TVWIPE
C     ENTRY TVWIP
C     ENTRY TVNEXT(IPLOTN)
C     ENTRY TVEND
C     ENTRY TVSET( IHOLL,/ARG1/,/ARG2/ )
C     SUBROUTINE TVGCLR
C     SUBROUTINE UGXERR(LEVEL,SNAME,INDEX)
C     SUBROUTINE TVPLOT( X,Y,N,/IDUMMY/ )
C     SUBROUTINE TVPMRK( X,Y,N,MARK,SIZE,COLOR )
C     SUBROUTINE TVDRAW( X,Y,N,/IDUMMY/ )
C     SUBROUTINE TVDDRW(CH,X,Y,N)
C     SUBROUTINE TVTEXT(X,Y,LTXT,N,/SIZE/)
C     SUBROUTINE TVBGFR(I)
C     SUBROUTINE TVCSET(I)
C     SUBROUTINE TVRNG(A,XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE TVCAL(N,X1,X2,Y1,Y2)
C
        SUBROUTINE TVBGN
C
C.....  contains the graphic elemnt used by HPLOT to output in U.G.S.
C
      COMMON/HPLBUF/NSIZE,ELMNT(10000)
C
C.....  color signaling common
C
      COMMON /HPLCLR/ COL(4)
      CHARACTER*7 COL
C     COMMON /HPOPTN/ MARK1,MARK2,SIZE1,SIZE2,LSTYLE
       include '_hpoptn.inc'
C
C   TV subroutines common area
C
       include '_tvcomm.inc'
C     COMMON /TVCOMM/ AREA(2,2),WINDOW(2,2),I1,I2,IFONT,TSIZE,IFLUSH
      EXTERNAL  UGXERR, HTCBLK, HCOBLK, HBUBLK
C
CYM
      DIMENSION XARRAY(2),IARRAY(8)
CYM   DIMENSION III(5)
CYM   DATA IX/1HX/
CYM     INTEGER   IGFLG
CYM
        CHARACTER * 8  DEVICE
        PARAMETER (FOTHRD = 4.0/3.0)
        PARAMETER (ONSXTH = 1.0/6.0)
        PARAMETER (SESXTH = 7.0/6.0)
C       DATA AREA /0.0,0.0,FOTHRD,1.0/
C       DATA WINDOW /-ONSXTH,0.0,SESXTH,1.0/
        LOGICAL LWIPE, LSCREN, LFIRST, LOPEN, LCONT, LWRIT
        LOGICAL LVERS, CVERS
        DATA LWIPE /.TRUE./, LFIRST /.TRUE./, LWRIT /.FALSE./
        DATA LVERS /.FALSE./
C       DATA COL /'WHITE','RED','GREEN','BLUE'/
C       DATA NSIZE /10000/
CYM     CHARACTER*60 PARM
        CHARACTER*60 PARM,IHOLL*(*)
C
        PARAMETER( MAXHOL = 19 )
        CHARACTER * 4  IHOLLS( MAXHOL )
        DATA IHOLLS / 'RIGH','UP  ','XLEF','XCEN','XRIG',
     &                'WIPE','NWIP','DUPL','SIMP','BASI',
     &                'SMAL','MEDI','LARG','DSPC','XWIN',
     &                'YWIN','MARK','SMBL','LSTY' /
C
C.....  select device and open
C
        IF (LWRIT) CALL UGWRIT(' ',0,ELMNT)
        LWRIT = .FALSE.
        CALL TVOPTN(PARM,ISEL,LOPEN,LFIRST,LSCREN)
        CVERS = PARM(1:5).EQ.'LASER'
CYM     IF (CVERS) LVERS = .TRUE.
CYM     IF (CVERS) IVERS = ISEL
        IF (CVERS) THEN
           LVERS = .TRUE.
           IVERS = ISEL
           CALL UGINFO('OPENDEV',DEVICE,IARRAY,XARRAY)
           NOPEN=IARRAY(1)
           IF(NOPEN.LE.1)GO TO 11
           DO 10 I=1,NOPEN
           IF(IARRAY(I+1).EQ.ISEL)GO TO 12
10         CONTINUE
11         CALL UGOPEN('LASER',ISEL)
           GO TO 12
        ENDIF
        IF (.NOT.LOPEN) CALL UGOPEN(PARM,ISEL)
12      CALL UGSLCT(' ',ISEL)
C       IF (CGRIN) CALL UGDEFL('SOFTGN')
        LFIRST = .FALSE.
        CALL UGINIT('CLEAR',ELMNT,NSIZE)
        LWRIT = .TRUE.
        RETURN
C
C.....  write and wipe the buffer
C
        ENTRY TVWIPE
C%%%    PRINT *,'*** TVWIPE, NSIZE=',NSIZE
        LWRIT = .TRUE.
        IF( LWIPE ) THEN
          CALL UGWRIT(' ',0,ELMNT)
          LWRIT = .FALSE.
          IF( .NOT. CVERS ) THEN
            CALL UGINIT('CLEAR',ELMNT,NSIZE)
          END IF
        END IF
C%%%    PRINT *, '***** TVWIPE *****'
        RETURN
C
C.....  wipe the buffer
C
        ENTRY TVWIP
        LWRIT = .FALSE.
        CALL UGINIT('CLEAR',ELMNT,NSIZE)
C%%%    PRINT *, '***** TVWIP *****'
        RETURN
C
C.....  next picture
C
        ENTRY TVNEXT(IPLOTN)
        CALL UGWRIT(' ',0,ELMNT)
        LWRIT = .FALSE.
        IF (IPLOTN.GT.0) NPICT = IPLOTN
        IF (IPLOTN.GT.1) THEN
          IF (LSCREN) THEN
            CALL TVOINS(LCONT)
            IF (.NOT.LCONT)  GO TO 500
          ELSE
            WRITE(6,1104) IPLOTN-1
1104        FORMAT (' PICTURE NUMBER',I4,' IS BEING COMPLETED')
          END IF
        END IF
        CALL UGPICT('CLEAR',0)
        CALL UGINIT('CLEAR',ELMNT,NSIZE)
C%%     CALL UGDSPC('PUT',FOTHRD,1.0,0.0)
C%%     CALL UGWDOW('PUT',AREA,WINDOW)
C%%%    PRINT *, '***** TVNEXT *****'
        RETURN
C
C.....  close all devices
C
        ENTRY TVEND
        CALL UGWRIT(' ',0,ELMNT)
        IF (LSCREN) THEN
          CALL TVOINS(LCONT)
        ELSE
          IF (NPICT.GT.0) WRITE(6,1104) NPICT
        END IF
500     IF (LVERS) THEN
          LVERS = .FALSE.
          CALL UGSLCT(' ',IVERS)
          CALL UGPICT('CLEAR',0)
          CALL UGINIT('CLEAR',ELMNT,NSIZE)
          CALL UGWRIT(' ',0,ELMNT)
        END IF
        CALL UGCLOS('ALL')
        LFIRST = .TRUE.
        NPICT = 0
        RETURN
C
C -------
C  TVSET
C -------
        ENTRY TVSET( IHOLL,ARG1,ARG2 )
C
        DO 100 I = 1, MAXHOL
          IF( IHOLL(1:4) .EQ. IHOLLS(I) ) THEN
            GOTO (101,102,103,104,105,106,107,108,109,110,
     &            111,112,113,114,115,116,117,118,119),I
          END IF
100     CONTINUE
C%%     PRINT '(1X,A,A)', '%TVSET, Undefined Keyword: ',IHOLL
        RETURN
C
101     I1 = 1
        RETURN
102     I1 = 2
        RETURN
103     IFLUSH = -1
        RETURN
104     IFLUSH = 0
        RETURN
105     IFLUSH = 1
        RETURN
106     LWIPE = .TRUE.
        RETURN
107     LWIPE = .FALSE.
        RETURN
108     IFONT = 3
        CALL UGFONT('DUPLEX')
        CALL UGDEFL('SOFTGN')
        CALL UGDEFL('NOFXSIZ')
        CALL HPLOPT('HARD',1)
        TSIZE = 0.30
        RETURN
109     IFONT = 2
        CALL UGFONT('SIMPLEX')
        CALL UGDEFL('NOFXSIZ')
        CALL HPLOPT('HARD',1)
        TSIZE = 0.30
        RETURN
110     IFONT = 1
        CALL HPLOPT('HARD',1)
        TSIZE = 0.30
        RETURN
111     TSIZE = 0.30
        RETURN
112     TSIZE = 0.40
        RETURN
113     TSIZE = 0.50
        RETURN
114     IF( ARG1.GT.0.0 ) ASPECT(1) = ARG1
        IF( ARG2.GT.0.0 ) ASPECT(2) = ARG2
        RETURN
115     IF( ARG1.GE.0.0 .AND. ARG1.LT.ARG2 .AND. ARG2.LE.1.0 ) THEN
          WDOWSZ(1,1) = ARG1
          WDOWSZ(1,2) = ARG2
        END IF
        RETURN
116     IF( ARG1.GE.0.0 .AND. ARG1.LT.ARG2 .AND. ARG2.LE.1.0 ) THEN
          WDOWSZ(2,1) = ARG1
          WDOWSZ(2,2) = ARG2
        END IF
        RETURN
117     MARK2 = INT( ARG1 )
        IF( MARK2.GE.0 .AND. MARK2.LE.9 ) THEN
          MARK2 = MARK2 + 1
        ELSE
          MARK2 = 0
        END IF
        SIZE2 = ARG2
        RETURN
118     MARK1 = INT( ARG1 )
        IF( MARK1.GE.0 .AND. MARK1.LE.9 ) THEN
          MARK1 = MARK1 + 1
        ELSE
          MARK1 = 0
        END IF
        SIZE1 = ARG2
        RETURN
119     LSTYLE = INT( ARG1 )
        RETURN
C
        END
C
C.....  Check actual device
C
        LOGICAL FUNCTION TVTYPE(CHECK)
        CHARACTER DEVICE*8, CHECK*(*)
        CALL UGINFO('DEVTYPE',DEVICE,ITEM2,TEM3)
        TVTYPE = DEVICE.EQ.CHECK
        END
C
C --------
C  TVGCLR
C --------
      SUBROUTINE TVGCLR
C
      CALL UGPICT( 'CLEAR',0 )
      RETURN
      END
C
C.....  Error handling
C
      SUBROUTINE UGXERR(LEVEL,SNAME,INDEX)
C
      COMMON/HPLBUF/NSIZE,ELMNT(10000)
      CHARACTER*8 SNAME
C
      IF (INDEX.EQ.11) THEN
        CALL UGWRIT(' ',0,ELMNT)
        CALL UGINIT('CONTINUE',ELMNT,NSIZE)
        LEVEL=0
      END IF
      RETURN
      END
C
C --------
C  TVPLOT
C --------
      SUBROUTINE TVPLOT( X,Y,N,IDUMMY )
C
      COMMON /HPLBUF/ NSIZE,ELMNT(10000)
C
      COMMON /HPLCLR/ COL(4)
      CHARACTER*7 COL
      DIMENSION X(*),Y(*)
C
      IF( N.LE.0 ) RETURN
      CALL UGPMRK( COL(4),X,Y,N,ELMNT )
      RETURN
      END
C
C --------
C  TVPMRK
C --------
      SUBROUTINE TVPMRK( X,Y,N,MARK,SIZE,COLOR )
C
      COMMON /HPLBUF/ NSIZE,ELMNT(10000)
C
      CHARACTER * (*)  COLOR
      INTEGER * 4  MARK
      REAL * 4  SIZE
      DIMENSION  X(*),Y(*)
      CHARACTER*32  OPTION
C
      IF( N.LE.0 ) RETURN
      IF( MARK.GE.1 .AND. MARK.LE.10 ) THEN
        IF( SIZE.GT.0.0 ) THEN
          WRITE( OPTION,'(A,6H,MARK=,I1,6H,SIZE=F6.4)' )
     &           COLOR,MARK-1,SIZE
        ELSE
          WRITE( OPTION,'(A,6H,MARK=,I1)' ) COLOR,MARK-1
        END IF
      ELSE
        OPTION = COLOR
      END IF
      JUNK = LKBRD( OPTION,0 )
      CALL UGPMRK( OPTION,X,Y,N,ELMNT )
      RETURN
      END
C
C --------
C  TVDRAW
C --------
      SUBROUTINE TVDRAW( X,Y,N,IDUMMY )
C
      COMMON/HPLBUF/NSIZE,ELMNT(10000)
C
C     COLOR SIGNALING COMMON
C
      COMMON/HPLCLR/COL(4)
      CHARACTER*7 COL
      DIMENSION X(*),Y(*)
C     DIMENSION XX(100),YY(100)
C
      IF(N.LE.0)  RETURN
C     DO II=1,N
C       XX(II) = X(II)/30.
C       YY(II) = Y(II)/30.
C     END DO
C%%   PRINT *, 'TVDRAW: COL(2)=',COL(2)
      CALL UGPLIN(COL(2),X,Y,N,1,1,ELMNT)
C%%   CALL TVWIPE
      RETURN
      END
C
C --------
C  TVDDRW
C --------
      SUBROUTINE TVDDRW(CH,X,Y,N)
C
      COMMON/HPLCLR/COL(4)
      CHARACTER*7 COL
      COMMON/HPLBUF/NSIZE,ELMNT(10000)
C
      DIMENSION X(2),Y(2)
      CHARACTER CH*(*)
      CHARACTER*8 OPT
C
      IF(N.LE.0)  RETURN
      ICOL=4
      IF(CH.EQ.'DASHED') ICOL=3
      IF(CH.EQ.'SOLID') ICOL=2
      IF(CH.EQ.'DOTDASH') ICOL=1
      OPT = CH
      CALL UGPLIN(COL(ICOL)//','//OPT,X,Y,N,1,1,ELMNT)
C%%   CALL TVWIPE
      RETURN
      END
C
      SUBROUTINE TVTEXT(X,Y,LTXT,N,SIZE)
C
C.....  contains the graphic elemnt used by HPLOT to output in U.G.S.
C
      COMMON/HPLBUF/NSIZE,ELMNT(10000)
C
C     COLOR SIGNALING COMMON
C
      COMMON/HPLCLR/COL(4)
      CHARACTER*7 COL
C       TV subroutines common area
C
       include '_tvcomm.inc'
C     COMMON /TVCOMM/ AREA(2,2),WINDOW(2,2),I1,I2,IFONT,TSIZE,IFLUSH
C
#if defined(HIUXF77) || defined(__GFORTRAN__)
      LOGICAL*1 LTXT(N)
#else
      BYTE*1 LTXT(N)
#endif
      INTEGER ITXT
      CHARACTER*80 PSTR,SSTR
      CHARACTER*11 CSIZ
      CHARACTER*11 ANGL(0:2)
      CHARACTER* 7 POSI(-1:1)
C     DATA IFONT /1/
      DATA ANGL/'ANGLE=00.0,','ANGLE=00.0,','ANGLE=90.0,'/
      DATA POSI/'LEFT,','CENTER,','RIGHT,'/
      CHARACTER ASCIIP*128, ASCIIS*128
C
      DATA ASCIIP(1:32)   /'GABCDEFGHIDKLMNOPQRSTUPWXYZSMHN+'/
      DATA ASCIIS(1:32)   /'FGGGGGGGGFGGGGGGGGGGGGFGGGGFMMMM'/
      DATA ASCIIP(33:64)  /' EQPD0+A()*+,-./0123456789.,L=GU'/
      DATA ASCIIS(33:64)  /' PPSSSSP                  PPM MP'/
      DATA ASCIIP(65:96)  /'AABCDEFGHIJKLMNOPQRSTUVWXYZ(/)TU'/
      DATA ASCIIS(65:96)  /'S                          SSSSS'/
      DATA ASCIIP(97:128) /'XABCDEFGHIJKLMNOPQRSTUVWXYZLVRSI'/
      DATA ASCIIS(97:128) /'SLLLLLLLLLLLLLLLLLLLLLLLLLLSSSMM'/
C
C%%   PRINT *, 'TVTEXT CALLED, IFONT=',IFONT
C%%   PRINT '(1H ,80A1)', (LTXT(I),I=1,N)
C
      IF(N.LE.0) RETURN
      IF(N.GT.80) N=80
C%%   CALL FLTRD( '?TEXT SIZE:',TSIZE )
C%%   PRINT *, 'TEXT SIZE=',TSIZE
C
      IF( NARGS(0) .EQ. 5 ) THEN
        WRITE(CSIZ,11) SIZE
      ELSE
        WRITE(CSIZ,11) TSIZE
      END IF
11    FORMAT('SIZE=',F5.3,',')
C
      IF (IFONT.EQ.1) THEN
        DO 111 I=1,N
          ITXT = LTXT(I)
          PSTR(I:I) = CHAR(ITXT)
111     CONTINUE
        CALL UGTEXT( CSIZ//ANGL(I1)//POSI(IFLUSH)//COL(1),
     &               X,Y,PSTR(1:N),ELMNT)
      ELSE
        DO 112 I=1,N
          ITXT = LTXT(I)
          PSTR(I:I) = CHAR(ITXT)
          SSTR(I:I) = ' '
C%%       J = LTXT(I)+1
C%%       IF( J.GE.1 .AND. J.LE.128 ) THEN
C%%         PSTR(I:I) = ASCIIP(J:J)
C%%         SSTR(I:I) = ASCIIS(J:J)
C%%       ELSE
C%%         PSTR(I:I) = ' '
C%%         SSTR(I:I) = ' '
C%%       END IF
112     CONTINUE
C%%     PRINT *, 'P TEXT:',PSTR(1:N)
C%%     PRINT *, 'S TEXT:',SSTR(1:N)
        CALL UGXTXT( CSIZ//ANGL(I1)//POSI(IFLUSH)//COL(1),
     &               X,Y,PSTR(1:N),SSTR(1:N),ELMNT)
      END IF
C%%   CALL TVWIPE
      RETURN
      END
C
      SUBROUTINE TVBGFR(I)
      RETURN
      END
C
      SUBROUTINE TVCSET(I)
      RETURN
      END
C
C
C ---------
C   TVRNG
C ---------
      SUBROUTINE TVRNG(A,XMIN,YMIN,XMAX,YMAX)
C
      COMMON/HPLBUF/NSIZE,ELMNT(10000)
       include '_tvcomm.inc'
C..
C..     UP TO NOW DUMMY ....
C..
      INTEGER*4 A
      CALL UGPICT('CLEAR',0)
      CALL UGINIT('CLEAR',ELMNT,NSIZE)
      CALL UGDSPC('PUT',XMAX*ASPECT(1),YMAX*ASPECT(2),0.0)
      AREA(1,1) = XMAX*ASPECT(1)*WDOWSZ(1,1)
      AREA(2,1) = YMAX*ASPECT(2)*WDOWSZ(2,1)
      AREA(1,2) = XMAX*ASPECT(1)*WDOWSZ(1,2)
      AREA(2,2) = YMAX*ASPECT(2)*WDOWSZ(2,2)
      WINDOW(1,1) = 0.0
      WINDOW(2,1) = 0.0
      WINDOW(1,2) = XMAX
      WINDOW(2,2) = YMAX
      CALL UGWDOW( 'PUT',AREA,WINDOW )
      RETURN
      END
C
      SUBROUTINE TVCAL(N,X1,X2,Y1,Y2)
C..
C..     UP TO NOW DUMMY
C..
      RETURN
      END
