C***********************************************************************
C*
C*-------------------=======
C* Subroutine PRCDYN(CMDNAM)
C*-------------------=======
C*
C*(Purpose)
C*    Process dynamic allocation command.
C*
C*(Input)
C*    CMDNAM ; Command name.
C*
C*(Author)
C*    A. Miyamoto   29-Sep-1987   Modified PRCRED..
C*
C*
CC**********************************************************************
C
      SUBROUTINE PRCDYN(CMDNAM)
C
#include "prdyna.inc"
 
C
      CHARACTER*(*) CMDNAM
      CHARACTER     BUFNO*2, OPTCD*1, WAIT*3, BLANK*80
      DATA          BLANK/' '/
      COMMON /SSWORK/ NONUM, NUMLST(2, 500), IFBUF(2000)
      EQUIVALENCE  (OPTDYN(7:7), BUFNO(1:1))
      EQUIVALENCE  (OPTDYN(17:17), OPTCD(1:1))
      EQUIVALENCE  (OPTDYN(25:25), WAIT(1:1))
C
      CHARACTER   TUOFIL*200, TUODEV*32, TUOVOL*64
      CHARACTER   TUOMNT*8, TUODMN*8
 
      PARAMETER   (MXxTYP=20)
      CHARACTER*8  TYPNAM(MXxTYP)
#ifdef  MSP
      CHARACTER*80 INPSTR(MXxTYP)
#endif
#ifdef  UNIX
      CHARACTER*200 INPSTR(MXxTYP)
#endif
#ifdef  UNIX
      CHARACTER    TYPE*4, DEVICE*32, VOLUME*64, MOUNT*8, DMOUNT*8
#endif
      DATA      NUMTYP/4/
      DATA     (TYPNAM(K),K=1,4)/'FORMAT', 'DST001', 'DST002', 'FILE' /
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Define operand for the command 'CONSTANT at the initial call.
C ----------------------------------------------------------------------
C
      LCMD = LEN(CMDNAM)
#ifdef  UNIX
      IF(CMDNAM(:LCMD).EQ.'INPUT') GO TO 200
#endif
#ifdef  UNIX
      IF(CMDNAM(:LCMD).EQ.'OUTFILE') GO TO 500
#endif
#ifdef  MSP
      IF(CMDNAM(1:1).NE.']') GO TO 200
#endif
C
      IF(CMDNAM(:LCMD).EQ.']INPUT') THEN
C
C ...............................
C Load Type name list into buffer.
C ...............................
C
#ifdef TOPAZ
	CALL MAOPEN( IRET )
        CALL MAGETE('FILELIST', NONUM, 2000, NW, IFBUF )
        CALL MACLOS
#endif
#ifdef JLC
	IFBUF(3)=0
#endif

        DO 110 I = 1, IFBUF(3)
          IP     = 5 + IFBUF(4)*FLOAT(I-1)
          NUMTYP = NUMTYP + 1
          WRITE(TYPNAM(NUMTYP),'(2A4)') IFBUF(IP),IFBUF(IP+1)
110     CONTINUE
        DO 120 I = 1, NUMTYP
          DO 121 LC = 8, 1, -1
            IF( TYPNAM(I)(LC:LC) .NE. ' ' ) GO TO 122
121       CONTINUE
          GO TO 120
122       CONTINUE
#ifdef  MSP
       CALL DBCOPR('INPUT',TYPNAM(I)(:LC),'C*80',INPSTR(I),IRET)
#endif
#ifdef  UNIX
          CALL DBCOPR('INPUT',TYPNAM(I)(:LC),
#endif
#ifdef  UNIX
     >                'S*200', INPSTR(I), IRET )
#endif
          IF( IRET .LT. 0 ) THEN
             PRINT *,'%Error PRCDYN..Failed to define ',TYPNAM(I)
             STOP
          ENDIF
          INPSTR(I) = ' '
120     CONTINUE
 
        CALL DBCOPR('INPUT','BUFNO',   'C*2 ',BUFNO ,IRET)
        CALL DBCOPR('INPUT','OPTCD',   'C*1 ',OPTCD ,IRET)
        CALL DBCOPR('INPUT','WAIT' ,   'C*3 ',WAIT  ,IRET)
#ifdef  UNIX
        CALL DBCOPR('INPUT','TYPE',    'C*4 ',TYPE  ,IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('INPUT','DEVICE',  'C*32',DEVICE, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('INPUT','VOLUME',  'C*64',VOLUME, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('INPUT','POSITION',  'I*4',NFPOS, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('INPUT','MOUNT',   'C*8 ',MOUNT, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('INPUT','DISMOUNT','C*8 ',DMOUNT, IRET)
#endif
 
        NDYNAT = -1
        NUMDYN = 0
        NOWEXP = 0
        NOWRUN = 0
        NXTFIL = 0
        NOWPNT = 0
        OPTDYN = 'BUFNO(10) OPTCD(C) WAIT(30)'
#ifdef  UNIX
        TYPE   = ' '
#endif
#ifdef  UNIX
        DEVICE = ' '
#endif
#ifdef  UNIX
        VOLUME = ' '
#endif
#ifdef  UNIX
        MOUNT  = ' '
#endif
#ifdef  UNIX
        DMOUNT = ' '
#endif
#ifdef  UNIX
        NFPOS = 0
#endif
C
C For OUTFILE command.
C
#ifdef  UNIX
      ELSEIF(CMDNAM(:LCMD).EQ.']OUTFILE') THEN
#endif
#ifdef  UNIX
        NOFALL = 0
#endif
#ifdef  UNIX
        CALL DBCOPR('OUTFILE', 'FILE',  'S*200', TUOFIL, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('OUTFILE', 'DEVICE','C*32',  TUODEV, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('OUTFILE', 'VOLUME','C*64',  TUOVOL, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('OUTFILE', 'MOUNT', 'C*8',   TUOMNT, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('OUTFILE', 'DISMOUNT', 'C*8',TUODMN, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('OUTFILE', 'POSITION','I*4', NOPOS, IRET)
#endif
#ifdef  UNIX
        CALL DBCOPR('OUTFILE', 'SIZE','I*4',  NOSIZ, IRET)
#endif
C
#ifdef  UNIX
        TUOFIL = ' '
#endif
#ifdef  UNIX
        TUODEV = ' '
#endif
#ifdef  UNIX
        TUOVOL = ' '
#endif
#ifdef  UNIX
        TUOMNT = ' '
#endif
#ifdef  UNIX
        TUODMN = ' '
#endif
#ifdef  UNIX
        NOPOS = 0
#endif
#ifdef  UNIX
        NOSIZ = 0
#endif
 
C*    OUTFIL(i)      : Output file name.
C*    OUTDEV(i)      : Output device name for the tape file.
C*    OUTVOL(i)      : Output tape volume name.
C*    NOFINF(1,i)   : Output file type ( not in use.)
C*          (2,i)   : File position of the tape, when the tape is
C*                  selected.
C*          (3,i)   : =1 to mount tape prior to open file.
C*          (4,i)   : =1 to dismount tape when the end-of-file is
C*          (5,i)   : Tape size in a unit of kbytes.
C*    NOFALL        : Total number of output tape file.
C*
 
      ENDIF
      GO TO 900
C
C ----------------------------------------------------------------------
C (2) Search what operand was specified.
C ----------------------------------------------------------------------
C
200   CONTINUE
C
      NDYNAT =  1
      DO 210 IT = 1, NUMTYP
        IF( INPSTR(IT)(1:80) .NE. BLANK ) GO TO 300
210   CONTINUE
      RETURN
C
C ----------------------------------------------------------------------
C (3) Decode the arguments of the operand.
C ----------------------------------------------------------------------
C
300   CONTINUE
C     PRINT *,' '
C     PRINT *,' PRCDYN..IT =', IT,' TYPNAM=', TYPNAM(IT)
C     PRINT *,' INPSTR =', INPSTR(IT)
      IF( IT .EQ. 4 ) GO TO 400
      ICOM = INDEX(INPSTR(IT), ':')
      IF( ICOM .GT. 0 ) THEN
        CALL USTRNM( INPSTR(IT), ':', NONUM, NUMLST )
      ELSE
        CALL USTRNM( INPSTR(IT), '-', NONUM, NUMLST )
      ENDIF
C
C
      IEXP  = NUMLST(1,1)
      IRUNL = 0
      DO 310  I = 2, NONUM
C
C
C  If Last run# or file# not specified, get those info from database.
C
        IF( NUMLST(2,I) .EQ. 0 ) THEN
          IF( IRUNL .EQ. 0 ) THEN
C            CALL MAOPEN( IRET )
            IF( IT .LE. 3 ) THEN
C              CALL MAGETE( 'RUNLIST', IEXP, 2000, NW, IFBUF )
              IF( NW .LE. 0 ) THEN
                PRINT *,'%Warning..PRCDYN  Data ',
     >                  ' for Exp#', IEXP,' not exist.'
                GO TO 310
              ENDIF
              IRUNL = IFBUF(4)
            ELSE
C              CALL MAGETE( TYPNAM(IT), IEXP, 2000, NW, IFBUF )
              IF( NW .LE. 0 ) THEN
                PRINT *,'%Warning..PRCDYN ',TYPNAM(IT),
     >                  ' files for Exp#', IEXP,' not exist.'
                GO TO 310
              ENDIF
              IRUNL = IFBUF(3)
            ENDIF
C            CALL MACLOS
          ENDIF
          NUMLST(2,I) = IRUNL
        ENDIF
 
        NUMDYN           = NUMDYN + 1
        NRNDYN(1,NUMDYN) = IEXP
        NRNDYN(2,NUMDYN) = NUMLST(1,I)
        NRNDYN(3,NUMDYN) = NUMLST(2,I)
        TYPDYN(NUMDYN)   = TYPNAM(IT)
C       PRINT *,' NUMDYN =', NUMDYN,(NRNDYN(K,NUMDYN),K=1,3),
C    >            TYPDYN(NUMDYN)
310   CONTINUE
C
      INPSTR(IT) = ' '
      GO TO 900
C
C ------------------------------------------------------------------
C (4) When FILE command is specified.
C ------------------------------------------------------------------
C
400   CONTINUE
      NUMDYN = NUMDYN + 1
      NRNDYN(1,NUMDYN) = 0
      NRNDYN(2,NUMDYN) = 0
      NRNDYN(3,NUMDYN) = 0
      TYPDYN(NUMDYN)   = TYPNAM(IT)
CC      CALL UCUPER(80, INPSTR(IT))
CC      PRINT *,'PRCDYN .. INPSTR=',INPSTR(IT)
 
      CALL UCSTRP(INPSTR(IT),' ',FILDYN(NUMDYN),ILEN)
CC      PRINT *,' ILEN =',ILEN
CC      PRINT *,' FILDYN =',FILDYN(NUMDYN)
#ifdef  MSP
      IF( ILEN .GT. 44 ) THEN
#endif
#ifdef  UNIX
      IF( ILEN .GT. 200 ) THEN
#endif
        PRINT *,'%Error ..PRCDYN .. Too long file name ',
     >          'used in INPUT statement.'
        PRINT *,'File name was ',INPSTR(IT)
        STOP
      ENDIF
 
#ifdef  UNIX
      CALL UCUPER(4,TYPE)
#endif
#ifdef  UNIX
      CALL UCUPER(8,MOUNT)
#endif
#ifdef  UNIX
      CALL UCUPER(8,DMOUNT)
#endif
 
#ifdef  UNIX
      IF( TYPE(1:3).EQ.'VBS' ) THEN
#endif
#ifdef  UNIX
         NFINFO(1,NUMDYN) = 1
#endif
#ifdef  UNIX
      ELSEIF( TYPE(1:4).EQ.'RMTL') THEN
#endif
#ifdef  UNIX
         NFINFO(1,NUMDYN) = 2
#endif
#ifdef  UNIX
      ELSEIF( TYPE(1:4).EQ.'SONY') THEN
         NFINFO(1,NUMDYN) = 3

      ELSEIF( TYPE(1:4).EQ.'FORT') THEN
         NFINFO(1,NUMDYN) = 4
#endif
#ifdef  UNIX
      ELSE
#endif
#ifdef  UNIX
         NFINFO(1,NUMDYN) = 0
#endif
#ifdef  UNIX
      ENDIF
#endif
#ifdef  UNIX
      IF( MOUNT(1:5).EQ.'MOUNT') THEN
#endif
#ifdef  UNIX
         NFINFO(3,NUMDYN) = 1
#endif
#ifdef  UNIX
      ELSE
#endif
#ifdef  UNIX
         NFINFO(3,NUMDYN) = 0
#endif
#ifdef  UNIX
      ENDIF
#endif
#ifdef  UNIX
      IF( DMOUNT(1:8).EQ.'DISMOUNT') THEN
#endif
#ifdef  UNIX
         NFINFO(4,NUMDYN) = 1
#endif
#ifdef  UNIX
      ELSE
#endif
#ifdef  UNIX
         NFINFO(4,NUMDYN) = 0
#endif
#ifdef  UNIX
      ENDIF
#endif
#ifdef  UNIX
      NFINFO(2,NUMDYN) = NFPOS
#endif
#ifdef  UNIX
      FILDEV(NUMDYN)   = DEVICE
#endif
#ifdef  UNIX
      FILVOL(NUMDYN)   = VOLUME
#endif
 
      INPSTR(IT) = ' '
#ifdef  UNIX
        TYPE   = ' '
#endif
#ifdef  UNIX
        DEVICE = ' '
#endif
#ifdef  UNIX
        VOLUME = ' '
#endif
#ifdef  UNIX
        MOUNT  = ' '
#endif
#ifdef  UNIX
        DMOUNT = ' '
#endif
#ifdef  UNIX
        NFPOS = 0
#endif
#ifdef  UNIX
      GO TO 900
#endif
C
C ********************************************
C   To handle OUTFILE command.
C ********************************************
 500  CONTINUE
#ifdef  UNIX
      NOFALL = NOFALL + 1
#endif
#ifdef  UNIX
      OUTFIL(NOFALL) = TUOFIL
#endif
#ifdef  UNIX
      OUTDEV(NOFALL) = TUODEV
#endif
#ifdef  UNIX
      OUTVOL(NOFALL) = TUOVOL
#endif
#ifdef  UNIX
      NOFINF(1,NOFALL) = 0
#endif
#ifdef  UNIX
      NOFINF(2,NOFALL) = NOPOS
#endif
#ifdef  UNIX
      IF( TUOMNT(1:5).EQ.'MOUNT') THEN
#endif
#ifdef  UNIX
         NOFINF(3,NOFALL) = 1
#endif
#ifdef  UNIX
      ELSE
#endif
#ifdef  UNIX
         NOFINF(3,NOFALL) = 0
#endif
#ifdef  UNIX
      ENDIF
#endif
#ifdef  UNIX
      IF( TUODMN(1:5).EQ.'DISMO') THEN
#endif
#ifdef  UNIX
         NOFINF(4,NOFALL) = 1
#endif
#ifdef  UNIX
      ELSE
#endif
#ifdef  UNIX
         NOFINF(4,NOFALL) = 0
#endif
#ifdef  UNIX
      ENDIF
#endif
#ifdef  UNIX
      NOFINF(5,NOFALL) = NOSIZ
#endif

C
#ifdef  UNIX
      TUOFIL = ' '
#endif
#ifdef  UNIX
      TUODEV = ' '
#endif
#ifdef  UNIX
      TUOVOL = ' '
#endif
#ifdef  UNIX
      TUOMNT = ' '
#endif
#ifdef  UNIX
      TUODMN = ' '
#endif
#ifdef  UNIX
      NOPOS = 0
#endif
#ifdef  UNIX
      NOSIZ = 0
#endif
 
C
C (9) Normal return
C
900   CONTINUE
      RETURN
      END
