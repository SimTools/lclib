C***********************************************************************
C* 
C*  ----------------------------------------------------------=======
C*  Subroutine Buildm( INFILE, OUTDDN, NORMOV, RMVSTR, LRMOVS,
C*                     EXPSTR, EEXPST, PRPTST, IRET )
C*  ----------------------------------------------------------=======
C* 
C*(Function)
C*   Main subrutine of BUILD
C*   Basic functions are,
C*    (1) Interprete =EXPAND, ==EXPAND, and %... string.
C*    (2) Open/Close file specified in =EXPAND statement.
C*    (3) Output expanded results.
C* 
C*(Input)
C*   INFILE : Input data set name, fully qualified data set name.
C*   OUTDDN : Output data set name.
C*   NORMOV : Number of characters to remove.
C*   RMVSTR : Character array containing string to be removed.
C*   LRMOVS : Integer array containing the length of each remove string.
C*   EXPSTR : String to triger =EXPAND statement.
C*   EEXPST : String to triger ==EXPAND string.
C*   PRPTST : Character to triger % function.
C* 
C*(Output)
C*   IRET   : Return code.  ^= 0, when error happens.
C* 
C*(Author)
C*   A. Miyamoto   23-Oct-1985
C* 
C*(Update record.)
C*   A. Miyamoto   23-Oct-1985
C*      Fully use of DMACRO and DYNAMYT libraries for
C*      Block read/write and BPAM access.
C*   A. Miyamoto   19-Jun-1986
C*      Add following options.
C*      (1) Add data base function ( Call BLDDB when used by BAT ).
C*      (2) Read first input from DD, when input file name is,
C*          *ddname format.
C*      (4) Support PS file EXPAND.
C*      (4) Override Expand data set name, which is specified by
C*          =EXPAND command.
C* 
C*   A. Miyamoto  15-Apr-1994  
C*      Unix version is prepared.  The program is completely different,
C*      But its function is same as those working on FACOM MSP.
C*      In this version, each record is read line-by-line,
C*      While FACOM version makes a block read to increase read time.
C*     
C*      ~/.buildrc contains the dictionary to convert FACOM's file name
C*      to proper directory name. 
C*
C***********************************************************************
C  
      SUBROUTINE BUILDM( INFILE, OUTDDN, NORMOV, RMVSTR, LRMOVS,
     >                   EXPSTR, EEXPST, PRPTST, IRET )
C  
C-----------------------------------------------------------------------
C Declare dummy arguments.
C-----------------------------------------------------------------------
C  
      CHARACTER*(*) INFILE, EXPSTR, EEXPST, RMVSTR(*)
      CHARACTER*1   PRPTST*1, OUTDDN*8
      INTEGER*4     LRMOVS(*)
C  
C-----------------------------------------------------------------------
C Declare buffer for =EXPAND files.
C-----------------------------------------------------------------------
C  
      PARAMETER  (MX_STK=10)
C  
C     MX_STK     : =EXPAND nest maximum.
C  
      CHARACTER   DSNSTK(MX_STK)*100, MEMSTK(MX_STK)*44
      INTEGER*4   IBMODE(MX_STK), LATCPT(MX_STK),
     >            LDSSTK(MX_STK), LMESTK(MX_STK)
C  
C     DSNSTK(j)  : Stack for current directory.
C     MEMSTK(j)  : Stack for current file name.
C     IBMODE(j)  : =1 for ==EXPAND files.
C     LATCPT(j)  : Pointer to DCB data for J-th data sets.
C     LDSSTK(j)  : character length of j-th data set.
C     LMESTK(j)  : character length of j-th member.
C     LTTR(j)    : Stacks the relative address of the last read.
C  
C-----------------------------------------------------------------------
   
C-----------------------------------------------------------------------
C  
      PARAMETER   (MX_BUF = 50,
     >             NRECL  = 80,
     >             NOBLKS = 3120,
     >             NOLINE = NOBLKS/NRECL )
C  
C     MX_BUF   : Maximum line number of the buffer.
C     NRECL    : Record length.
C     NOBLKS   : Block size of output buffer.
C     NOLINE   : Output buffer line length.
C  
      CHARACTER*80  OUTPUT,         INPUT
C  
C     OUTPUT     : Output data buffer.
C     INPUT      : Input data buffer.
C     LINSTK(j)  : Curent line # being processed for j-th data set.
C     LINMAX(j)  : Maximum line # of j-th data set.
C  
C-----------------------------------------------------------------------
C Stack for input data set and DD name.
C-----------------------------------------------------------------------
C  
      PARAMETER   (MX_ATC = 20)
C  
C     MX_ATC     : Maximum number of PO file, to be attached.
C  
      CHARACTER*100  ATCDSN(0:MX_ATC)
      CHARACTER*8   ATCDDN(0:MX_ATC)
C      INTEGER*4     ATCDCB(50, 0:MX_ATC), OUTDCB(50)
      INTEGER*4     NUMATC, LATCDS(0:MX_ATC)
C  
      DATA   NUMATC/0/
      DATA   ATCDDN/'BUILDLIB',
     >              'BUILD001','BUILD002','BUILD003','BUILD004',
     >              'BUILD005','BUILD006','BUILD007','BUILD008',
     >              'BUILD009','BUILD010','BUILD011','BUILD012',
     >              'BUILD013','BUILD014','BUILD015','BUILD016',
     >              'BUILD017','BUILD018','BUILD019','BUILD020'/
C  
C     ATCDSN(l)   : Data set name attached to l-th DD.
C     ATCDDN(l)   : DD name.
C     ATCDCB(l)   : DCB data for l-th dd.
C     OUTDCB      : Output file DCB data.
C     NUMATC      : # of files currently being attached.
C     LATCDS(l)   : Character length of attached data set name.
C     l=0 for BUILD LIBRARY data set
C  
      INTEGER*4     NATCUN(MX_ATC)
C     NATCUN(l)   : Unit number for l-th file (51 to 70 is used)
C  
C-----------------------------------------------------------------------
C Declare additional variables.
C-----------------------------------------------------------------------
C  
      CHARACTER*80   WORK,WORK2, REST, DEFLT, BLANK
      CHARACTER*16   WEXP, WEEXP
      DATA           BLANK/'      '/
C  
C  For BATBLD program.
C  
CBAT       CHARACTER*8    JOBNAM
CBAT       IJOBC = 1
      DATA NOUNIT/1/
      CHARACTER*255  TMPDSN

#ifdef HIUXF77
      logical fileex
#endif

C  
C=============<  Entry Point >==========================================
C
C ... Get File name conversion table      
C
      CALL CNVNAM('INITIALIZE',TMPDSN, IDUMY)
C  
C  (1) Initialize stack for input data set.
C  
CFACOM      CALL DCBS(OUTDCB,OUTDDN,'PD','FB',3120,80,'PS')
CFACOM      CALL OPENS(OUTDCB,'OUTPUT')
      IRET = -1
      IOS = 0

      OPEN(NOUNIT,FILE=OUTDDN,FORM='FORMATTED',
     >     ACCESS='SEQUENTIAL',STATUS='NEW',IOSTAT=IOS)
      IF( IOS.NE.0 ) THEN
         PRINT *,'%Error in BUILDM.. Error to open ',
     >   'output file :',OUTDDN
         PRINT *,' IOSTAT =',IOS
         IRET = -1
         RETURN
      ENDIF
C  
C  
C  
      LIN    = LEN(INFILE)
      LEXP   = LEN(EXPSTR)
      LEEXP  = LEN(EEXPST)
      WEXP   = EXPSTR
      WEEXP  = EEXPST
      ISTK   = 1
      LATCDS(0)    = 0
      IBMODE(ISTK) = 0
      NINDDN = 0
C  
C (1.2) Open first file.
C       NINDDN = 0 ; when file name is specified.
C              = 1 ; when read from
C  
      IF(INFILE(1:1).NE.'*') THEN
        CALL LBSDSN(INFILE(1:LIN),TMPDSN,MEMSTK(ISTK),
     >              LTMP, LMESTK(ISTK) )
        CALL CNVNAM(TMPDSN(:LTMP),DSNSTK(ISTK),LDSSTK(ISTK))
      ELSE
        IF(LIN.EQ.1) THEN
          ATCDDN(ISTK) = 'BUILDIN'
        ELSE
          ATCDDN(ISTK) = INFILE(2:LIN)
        ENDIF
        LATCDS(ISTK) = 0
        DSNSTK(ISTK) = 'File attached to a DD '//ATCDDN(ISTK)
        LDSSTK(ISTK) = 30
        MEMSTK(ISTK) = ' '
        LMESTK(ISTK) = 0
        NINDDN = 1
      ENDIF
C  
C-------------------------------------------------------------------
C (2) Search whether specified data set is opened before or not.
C-------------------------------------------------------------------
200   CONTINUE
C  
C (2.1) Change prefix for use in NAGOYA.
C  
CNGY      IF(DSNSTK(ISTK)(1:2).EQ.'T#') THEN
CNGY         IF(LDSSTK(ISTK).GT.36) THEN
CNGY           PRINT *,'%Error BUILD..Too long data set name to add ',
CNGY     >             ' prefix TRISTAN. '
CNGY           PRINT *,'              DSN =',DSNSTK(ISTK)
CNGY           GO TO 9000
CNGY         ENDIF
CNGY         WORK   = 'TRISTAN.'//DSNSTK(ISTK)(1:LDSSTK(ISTK))
CNGY         LDSSTK(ISTK) = LDSSTK(ISTK) + 8
CNGY         DSNSTK(ISTK) = WORK
CNGY      ENDIF
C  
C (2.1.2) Search member in the library data set first.
C  
210   CONTINUE
C     PRINT *,'Try to find library 210 LATDS(0),LMEMSTK()=',
C    .         LATCDS(0),LMESTK(ISTK)
C     PRINT *,' NINDDN,ISTK=',NINDDN,ISTK
      IF(LATCDS(0).EQ.0.OR.LMESTK(ISTK).EQ.0) GO TO 230
      IF(NINDDN.NE.0.AND.ISTK.EQ.1)            GO TO 230
C      CALL FIND(ATCDCB(1,0),MEMSTK(ISTK),ILL)
C     PRINT *,'FIND ',MEMSTK(ISTK),' ILL=',ILL
C
C  Check Existence and Read permision of the file.
C
      TMPDSN=ATCDSN(0)(:LATCDS(0))//MEMSTK(ISTK)
#ifdef HIUXF77
      INQUIRE(file=TMPDSN,exist=fileex)
      if( fileex) then
         ill=0
      else
         ill=1
      endif
#endif
#ifndef HIUXF77
#ifdef MACHTEN_PPC
      print *, 'TMPDSN=',TMPDSN(:LEN(TMPDSN))
      ILL = 1
#else
      ILL = access(TMPDSN,'r')
#endif
#endif
      
      IF(ILL.EQ.0) THEN
        DSNSTK(ISTK) = ATCDSN(0)
        LDSSTK(ISTK) = LATCDS(0)
        LATCPT(ISTK) = 0
        GO TO 410
      ELSE
        GO TO 230
C      ELSE
C        PRINT *,'%Error BUILD .. I/O error to find member ',
C     .           MEMSTK(ISTK)(:LMESTK(ISTK)),' in the Library data ',
C     .          'set ',ATCDSN(0)(:LATCDS(0))
C        GO TO 9000
      ENDIF
C  
C (2.2) Search previously opened data set.
C  
230   CONTINUE
C     PRINT *,' Going to open DSN=',DSNSTK(ISTK)
C      DO 240 I = 1, NUMATC
C         IF(LDSSTK(ISTK).NE.LATCDS(I)) GO TO 240
C         IF(DSNSTK(ISTK)(1:LDSSTK(ISTK)).EQ.
C     >      ATCDSN(I)(1:LATCDS(I)) )    THEN
C            LATCPT(ISTK) = I
C           PRINT *,' DSN ',DSNSTK(ISTK)(1:LDSSTK(ISTK)),' already ',
C    >              ' opened.'
C           PRINT *,' ISTK, NUMATC =',ISTK, NUMATC
C            GO TO 300
C         ENDIF
240   CONTINUE
C  
C (2.3) Attach new data set, and open it.
C  
C      NUMATC = NUMATC + 1
C      IF(NUMATC.GT.MX_ATC) THEN
C        PRINT *,'%Error BUILD .. Going to attach to many data set.'
C        GO TO 9000
C      ENDIF
C      ATCDSN(NUMATC) = DSNSTK(ISTK)
C      LATCDS(NUMATC) = LDSSTK(ISTK)
C      IF(NINDDN.EQ.0.OR.NUMATC.GT.1) THEN
C       PRINT *,' Going to attach new dsn =',
C    >           ATCDSN(NUMATC)(1:LATCDS(NUMATC)),'>>>'
C       PRINT *,' NUMATC, LATCDS =',NUMATC, LATCDS(NUMATC)
C        CALL ATTACH(ATCDSN(NUMATC),'        ',ATCDDN(NUMATC),ICON)
C        IF(ICON.NE.0) THEN
C          PRINT *,'%Error BUILD .. Failed to attach ',ATCDSN(NUMATC)
C          WRITE(6,'(''   return code is '',Z8)') ICON
C          GO TO 9000
C        ENDIF
C      ENDIF
C      CALL DCBS(ATCDCB(1,NUMATC), ATCDDN(NUMATC), 'RP')
C      CALL OPENS(ATCDCB(1,NUMATC), 'INPUT')
C      LATCPT(ISTK) = NUMATC
      NATCUN(ISTK) = 50 + ISTK

C  
C-------------------------------------------------------------------
C (3) Find specified member.
C-------------------------------------------------------------------
C  
300   CONTINUE
      IF(NINDDN.NE.0.AND.ISTK.EQ.1) GO TO 400
C      IF(LMESTK(ISTK).LE.0)         GO TO 400
C      CALL FIND(ATCDCB(1,LATCPT(ISTK)), MEMSTK(ISTK), ILL )
      TMPDSN=DSNSTK(ISTK)(:LDSSTK(ISTK))//MEMSTK(ISTK)
      OPEN(NATCUN(ISTK),FILE=TMPDSN,ACCESS='SEQUENTIAL',
     >     STATUS='OLD',IOSTAT=ILL)
      IF(ILL.EQ.0) THEN 
         GO TO 400
      ELSE
        
        PRINT *,'%Error BUILD .. Error to open file ',
     >            DSNSTK(ISTK)(:LDSSTK(ISTK)),MEMSTK(ISTK)
        PRINT *,' Return code is =',ILL
      ENDIF
      GO TO 9000
C  
C-------------------------------------------------------------------
C (4) Read input file.
C-------------------------------------------------------------------
C  
400   CONTINUE
      IF(LMESTK(ISTK).GT.0) THEN
         WRITE(6,401) DSNSTK(ISTK)(1:LDSSTK(ISTK)),
     >       MEMSTK(ISTK)(1:LMESTK(ISTK))
 401     FORMAT('*** Expanding     ',A,A)
C        PRINT *,'*** Expanding     ',DSNSTK(ISTK)(:LDSSTK(ISTK)),
C     >          MEMSTK(ISTK)(1:LMESTK(ISTK))
      ELSE
        PRINT *,'*** Expanding     ',DSNSTK(ISTK)(:LDSSTK(ISTK))
      ENDIF
      GO TO 420
410   CONTINUE
         WRITE(6,402) DSNSTK(ISTK)(1:LDSSTK(ISTK)),
     >       MEMSTK(ISTK)(1:LMESTK(ISTK))
 402     FORMAT('%%% Expanding Lib.',A,A)
C        PRINT *,'%%% Expanding Lib.',DSNSTK(ISTK)(:LDSSTK(ISTK)),
C     >         MEMSTK(ISTK)(1:LMESTK(ISTK))
C  
C (4.01) Read one-block.
C  
420   CONTINUE
460   CONTINUE
C      CALL READS(ATCDCB(1,LATCPT(ISTK)), INPUT(1,ISTK))
C      CALL CHECK(ATCDCB(1,LATCPT(ISTK)), ILL, LENGTH)
      READ(NATCUN(ISTK),'(A)',END=800) INPUT
C  
C (4.1) De-Block input data.
C  
450   CONTINUE
C     IF(LMESTK(ISTK).GT.0) THEN
C      PRINT *,'450 CONTINUE Start file ',DSNSTK(ISTK)(1:LDSSTK(ISTK)),
C    >        '(',MEMSTK(ISTK)(1:LMESTK(ISTK)),')'
C     ELSE
C       PRINT *,'450 CONTINUE Start file ',DSNSTK(ISTK)(1:LDSSTK(ISTK))
C     ENDIF
C      LINSTK(ISTK) = 0
C      LINMAX(ISTK) = LENGTH/NRECL
c460   CONTINUE
C     PRINT *,'Pass 460 CONTINUE, ISTK, NUMATC=',ISTK,NUMATC
C      LINSTK(ISTK) = LINSTK(ISTK) + 1
C      IF(LINSTK(ISTK).GT.LINMAX(ISTK)) GO TO 420
C  
C-------------------------------------------------------------------
C (5) Interprete input data.
C-------------------------------------------------------------------
500   CONTINUE
C  
C (5.1) Remove string if specified.
C  
      DO 510 I = 1, NORMOV
        IF(INPUT(1:LRMOVS(I)).EQ.
     >     RMVSTR(I)(1:LRMOVS(I)) ) THEN
             WORK = INPUT(LRMOVS(I)+1:)
             INPUT = WORK
             GO TO 520
        ENDIF
510   CONTINUE
520   CONTINUE
C  
C (5.1.1) For x'...' statement.
C  
      IPRMPT = INDEX(INPUT,PRPTST//'''')
      IF(IPRMPT.NE.0) GO TO 600
C  
      IF(INPUT(1:1).EQ.'=') THEN
C  
C (5.1.2) For =EXPAND statement.
C  
        IF(INPUT(1:LEXP).EQ.WEXP(1:LEXP)) THEN
          IBMODF = 0
          WORK   = INPUT(LEXP+1:72)
          GOTO 700
        ENDIF
C  
C (5.1.3) For ==EXPAND statement.
C  
        IF(INPUT(1:LEEXP).EQ.WEEXP(1:LEEXP)) THEN
          IBMODF = 1
          WORK   = INPUT(LEEXP+1:72)
CDEB         PRINT *,' WORK=',WORK
          GOTO 700
        ENDIF
C  
C (5.1.4) Includes data base information.
C  
CBAT       IF(ISTK.EQ.1.AND.INPUT(LINSTK(ISTK),ISTK)(1:1).EQ.'=') THEN
CBAT         CALL BLDDB(INPUT(LINSTK(ISTK),ISTK),OUTDCB,IRET)
CBAT         IF(IRET.EQ.0) GO TO 460
CBAT         IF(IRET.LT.0) GO TO 9000
CBAT       ENDIF
C  
C (5.1.5) For =LIBRARY command.
C  
        IF(INPUT(1:9).EQ.'=LIBRARY ') THEN
          WORK = INPUT(10:)
          GO TO 1000
        ENDIF
      ENDIF
C  
530   CONTINUE
C  
C-------------------------------------------------------------------
C (5.3)  Output data on a file.
C-------------------------------------------------------------------
C  
CBAT       IF(IJOBC.GE.1) THEN
CBAT          CALL BLDJOB(INPUT(LINSTK(ISTK),ISTK),
CBAT      >   OUTDCB, OUTDDN, IJOBC, JOBNAM)
CBAT          IF(IJOBC.LT.0)  GO TO  9000
CBAT          GO TO 460
CBAT       ENDIF
   
CDEB       PRINT *,'IBMODE(ISTK) =',IBMODE(ISTK)
      IF(IBMODE(ISTK).EQ.0) THEN
         OUTPUT = INPUT
      ELSE
CDEB       PRINT *,'WEEXP =',WEEXP(1:LEEXP),'>>'
         IF(INPUT(1:2).NE.'C'//WEEXP(1:1)) THEN
            OUTPUT = INPUT
         ELSE
            OUTPUT = '  '//INPUT(3:80)
         ENDIF
      ENDIF
      NW = lnblnk(output)
      if( nw.eq.0 ) nw = 80
      write(nounit,'(A)') output(:nw)
C      CALL PUT(OUTDCB,OUTPUT,ILL,80)
c      IF(ILL.NE.0) THEN
c        IF(ILL.EQ.3) THEN
c          PRINT *,'%Error BUILD .. Extent over while outputting ',
c     >            ' on ',OUTDDN
c          GO TO 9000
c        ELSE
c          PRINT *,'%Error BUILD .. output error occured .'
c          GO TO 9000
c        ENDIF
c      ENDIF
      GO TO 420
C  
C-------------------------------------------------------------------
C (6) Prompt Input data for x'...' statement.
C-------------------------------------------------------------------
C  
600   CONTINUE
      WORK   = INPUT(IPRMPT+2:72)
      IEAPS  = INDEX(WORK,'''')
      REST   = WORK(IEAPS+1:)
      LREST  = 72 - IEAPS - 1
      IF(IEAPS.EQ.0) GO TO 530
      DEFLT = WORK(:IEAPS-1)
      IDEF  = INDEX(DEFLT,'!')
      IF(IDEF.EQ.0) THEN
            DEFLT = ' '
      ELSE
            DEFLT = WORK(IDEF+1:IEAPS-1)
      ENDIF
      CALL PROMPT(2,WORK(:IEAPS-1),IRT)
      READ(5,'(A80)') WORK2
      IF(WORK2.EQ.BLANK.AND.IDEF.NE.0) WORK2 = DEFLT
      CALL UCSTRP(WORK2,' ',WORK,LWORK)
      IF(IPRMPT.EQ.1) THEN
        INPUT = WORK(:LWORK)//REST(:LREST)
      ELSE
        INPUT(IPRMPT:) =
     >       WORK(:LWORK)//REST(:LREST)
      ENDIF
      GOTO 500
C  
C-------------------------------------------------------------------
C (7) prepare file name for "=EXPAND " commaand.
C-------------------------------------------------------------------
C  
700   CONTINUE
C      IF(LMESTK(ISTK).GT.0) CALL NOTE(ATCDCB(1,LATCPT(ISTK)),LTTR(ISTK))
      ISTK = ISTK + 1
      IF(ISTK.GT.MX_STK) THEN
        PRINT *,'%Error BUILD  Too deap nest of expansion.'
        GO TO  9000
      ENDIF
      ICOMA = INDEX(WORK,'''')
      IF(ICOMA.EQ.0) THEN
        PRINT *,'%Error BUILD .. file name ',
     .          ' does not enclosed with ''.'
        PRINT *,'    INPUT=',INPUT
        GO TO  9000
      ENDIF
      WORK2 = WORK(ICOMA+1:)
      ICOMA = INDEX(WORK2,'''')
      IF(ICOMA.LE.1) THEN
        PRINT *,' %Error .. file name ',
     .          ' does not enclosed with ''.'
        PRINT *,'    INPUT=',INPUT
        GO TO  9000
      ENDIF
   
      CALL LBSDSN(WORK2(:ICOMA-1), TMPDSN, MEMSTK(ISTK),
     >                   LTMP, LMESTK(ISTK) )
      CALL CNVNAM(TMPDSN(:LTMP), DSNSTK(ISTK), LDSSTK(ISTK))
      IBMODE(ISTK) = IBMODF
      GO TO 200
C  
C-------------------------------------------------------------------
C (8) End of expand, get previous file name from stack.
C-------------------------------------------------------------------
C  
800   CONTINUE
      CLOSE(NATCUN(ISTK))
      ISTK = ISTK - 1
      IF(ISTK.GT.0) THEN
        GO TO 460
c        IF(LMESTK(ISTK).LE.0)  GO TO 460
c        CALL POINT(ATCDCB(1,LATCPT(ISTK)), LTTR(ISTK))
c        CALL READS(ATCDCB(1,LATCPT(ISTK)), INPUT(1,ISTK))
c        CALL CHECK(ATCDCB(1,LATCPT(ISTK)), ILL, LENGTH)
c        IF(ILL.EQ.0) THEN
c          IF(LENGTH/NRECL.NE.LINMAX(ISTK)) THEN
c            PRINT *,'%Error BUILD .. Block positioning error'
c           PRINT *,'  Error DSN=',DSNSTK(ISTK)(1:LDSSTK(ISTK)),
c     >              '(',MEMSTK(ISTK)(1:LMESTK(ISTK)),')'
c            GO TO 9000
c          ENDIF
c          GO TO 460
c        ELSEIF(ILL.EQ.1) THEN
c          PRINT *,'%Error BUILD .. Encounter end of file while ',
c     >            'positioning next read block.'
c          PRINT *,'  Error DSN=',DSNSTK(ISTK)(1:LDSSTK(ISTK)),
c     >            '(',MEMSTK(ISTK)(1:LMESTK(ISTK)),')'
c          GO TO 9000
c        ELSE
c          PRINT *,'%Error BUILD .. I/O error while ',
c     >            'positioning next read block.'
c          PRINT *,'  Error DSN=',DSNSTK(ISTK)(1:LDSSTK(ISTK)),
c     >            '(',MEMSTK(ISTK)(1:LMESTK(ISTK)),')'
c          GO TO 9000
c        ENDIF
      ENDIF
C  
      IF(LMESTK(1).GT.0) THEN
         WRITE(6,1001) DSNSTK(1)(:LDSSTK(1)),MEMSTK(1)(1:LMESTK(1))
 1001    FORMAT(' *** Expanded      ',A,A)
C        PRINT *,'*** Expanded      ',DSNSTK(1)(:LDSSTK(1)),
C     >         MEMSTK(1)(1:LMESTK(1))
C        PRINT *,'*** Expanded      ',DSNSTK(1)(:LDSSTK(1)),
C     >         MEMSTK(1)(1:LMESTK(1))
      ELSE
        PRINT *,'*** Expanded      ',DSNSTK(1)(:LDSSTK(1))
      ENDIF
      GO TO 10000
C  
C-------------------------------------------------------------------
C (10) Open LIBRARY data set.
C-------------------------------------------------------------------
C  
1000  CONTINUE
c      IF(LATCDS(0).GT.0) THEN
c        CALL CLOSES(ATCDCB(1,0))
c        CALL DETACH(ATCDDN(0),ICON)
c        IF(ICON.NE.0) THEN
c          PRINT *,'%Error BUILD .. Failed to free library data set ',
c     .            ATCDSN(0)(:LATCDS(0))
c          GO TO 9000
c        ENDIF
c        PRINT *,'%%% Free library  ',ATCDSN(0)(:LATCDS(0))
c      ENDIF
C  
      ICOMA = INDEX(WORK,'''')
      IF(ICOMA.EQ.0) THEN
        CALL UCSTRP(WORK,' ',WORK2,LWRK2)
        IF(WORK2.EQ.BLANK) THEN
          IF(NINDDN.NE.0.AND.ISTK.EQ.1) THEN
            PRINT *,'%Error BUILD .. Library data set name not ',
     .              ' available.  Library data set not defined.'
            GO TO 460
          ENDIF
          ATCDSN(0) = DSNSTK(ISTK)
          LATCDS(0) = LDSSTK(ISTK)
          GO TO 1010
        ELSEIF(WORK2(1:4).EQ.'OFF ') THEN
          LATCDS(0) = 0
C         PRINT *,'%%% Library  Turned Off.'
          GO TO 460
        ELSE
          PRINT *,'%Error BUILD .. file name ',
     .            ' does not enclosed with ''.'
          PRINT *,'    INPUT=',INPUT
          GO TO  9000
        ENDIF
      ENDIF
      WORK2 = WORK(ICOMA+1:)
      ICOMA = INDEX(WORK2,'''')
      IF(ICOMA.LE.1) THEN
        PRINT *,' %Error .. file name ',
     .          ' does not enclosed with ''.'
        PRINT *,'    INPUT=',INPUT
        GO TO  9000
      ENDIF
      CALL LBSDSN(WORK2(:ICOMA-1), TMPDSN, WORK, LTMP, LMEM)
      CALL CNVNAM(TMPDSN, ATCDSN(0), LATCDS(0))

1010  CONTINUE
      IF(LATCDS(0).LE.0) THEN
        PRINT *,' %Error BUILD  Invalid library data set specified.'
        PRINT *,'    INPUT=',INPUT
        GO TO  9000
      ENDIF
CNGY      IF(ATCDSN(0)(1:2).EQ.'T#') THEN
CNGY         IF(LATCDS(0).GT.36) THEN
CNGY           PRINT *,'%Error BUILD..Too long data set name to add ',
CNGY     >             ' prefix TRISTAN. '
CNGY           PRINT *,'              DSN =',ATCDSN(0)
CNGY           GO TO 9000
CNGY         ENDIF
CNGY         WORK   = 'TRISTAN.'//ATCDSN(0)(:LATCDS(0))
CNGY         LATCDS(0) = LATCDS(0) + 8
CNGY         ATCDSN(0) = WORK
CNGY      ENDIF
c      CALL ATTACH(ATCDSN(0),'        ',ATCDDN(0),ICON)
c      IF(ICON.NE.0) THEN
c          PRINT *,'%Error BUILD .. Failed to attach ',
c     .             ATCDSN(0)(:LATCDS(0))
c          WRITE(6,'(''   return code is '',Z8)') ICON
c          GO TO 9000
c      ENDIF
c      CALL DCBS(ATCDCB(1,0), ATCDDN(0), 'RP')
c      CALL OPENS(ATCDCB(1,0), 'INPUT')
c        PRINT *,'%%% Alloc Library ',ATCDSN(0)(:LATCDS(0))
      GO TO 460
C  
C-------------------------------------------------------------------
C (90) Normal return, Close open file.
C-------------------------------------------------------------------
C  
10000 CONTINUE
CBAT       PRINT *,' Job ',JOBNAM,' submitted.'
      IRET = 0
C  
9000  CONTINUE
c      CALL CLOSES(OUTDCB)
c      DO 9010 I = 1, NUMATC
c        CALL CLOSES(ATCDCB(1,I))
c        CALL DETACH(ATCDDN(I), ICON)
c        IF(ICON.NE.0) THEN
c          PRINT *,'%Warning BUILD .. Error to detach dsn ',
c     >             ATCDSN(I)
c        ENDIF
c9010  CONTINUE
C  
c      IF(LATCDS(0).GT.0) THEN
c        CALL CLOSES(ATCDCB(1,0))
c        CALL DETACH(ATCDDN(0), ICON)
c        IF(ICON.NE.0) THEN
c          PRINT *,'%Warning BUILD .. Error to detach dsn ',
c     >             ATCDSN(0)
c        ENDIF
c      ENDIF
C  
CBAT       CALL DBTERM(IRET)
C  
      RETURN
      END
