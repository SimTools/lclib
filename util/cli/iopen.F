C   03/07/85 802022207  MEMBER NAME  IOPEN    (FORT)     M  FORTRAN
C
       FUNCTION IOPEN(LU,RFILE,EXT,KIND)
C
C      Modified by FOREST ROUSE for PEP4 8/84.  It can now open files
C      by LOGICAL NAMES (It opens the file under the FORTRAN open with
C      a default file name supplied which has the EXT specified by the
C      User).
C
CCC    INCLUDE '($SSDEF)'
C
       INTEGER*4 TABLE, J5, NARGET, NARGS, SEARCH, LZ, LENRD
       CHARACTER*(*) RFILE
       CHARACTER*(*) EXT
       CHARACTER*(*) KIND
CCC    CHARACTER ACCESS*10, TYPE*7, DEF_FILE*10
       CHARACTER ACCESS*10, TYPE*7
CCC    CHARACTER CARRIAGECONTROL*7, FORM*11
       CHARACTER FORM*11
CCC    CHARACTER*3 CVT_UNIT
C      EXTERNAL TEST_IOPEN
C
C..... open file
C
       IF (INDEX(KIND,'R').NE.0) THEN
         TYPE = 'OLD'
       ELSE
         TYPE = 'NEW'
       END IF
       IF (INDEX(KIND,'A').NE.0) THEN
         ACCESS = 'APPEND'
         TYPE = 'UNKNOWN'
       ELSE
         ACCESS = 'SEQUENTIAL'
       END IF
       IF (INDEX(KIND,'L').NE.0) THEN
CCC      CARRIAGECONTROL = 'LIST'
       ELSE
CCC      CARRIAGECONTROL = 'FORTRAN'
       END IF
       IF (INDEX(KIND,'U').NE.0) THEN
         FORM = 'UNFORMATTED'
       ELSE
         FORM = 'FORMATTED'
       END IF
2      CLOSE (LU)
C
C      ADD . IF NECESSARY TO EXTENSION : IF FILE IS A DEVICE - NO DEFAU
C
5      CONTINUE
       LZ = LENRD(RFILE)
       IF (RFILE(LZ:LZ).EQ.':') THEN
         IF (INDEX(KIND,'R').EQ.0) THEN
           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
     %      ACCESS=ACCESS)
         ELSE
CMSP           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
CMSP     %           ACCESS=ACCESS,ACTION='READ')
CVMS
#if defined(HIUXF77) || defined(MACHTEN_PPC) || defined(AIX)
           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
     >           ACCESS=ACCESS)
#else
           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
     %           ACCESS=ACCESS,READONLY)
#endif
CVMS
         END IF
       ELSE
C
C        ENCODE UNIT NUMBER TO MAKE A VIABLE DEFAULT FILE
C
CCC      IRET = OTS$CVT_L_TI(LU,CVT_UNIT,%VAL(3))
CCC      IF (IRET .NE. SS$_NORMAL) CVT_UNIT = '001'
C
CCC      IF (INDEX(EXT,'.') .NE. 0) THEN
CCC        DEF_FILE = 'FOR'//CVT_UNIT//EXT
CCC      ELSE
CCC        DEF_FILE = 'FOR'//CVT_UNIT//'.'//EXT
CCC      ENDIF
C
         IF (INDEX(KIND,'R').EQ.0) THEN
           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
     %           ACCESS=ACCESS)
         ELSE
CDD    WRITE(6,*) 'LZ=',LZ
CDD    WRITE(6,*) 'FILE=',RFILE(1:LZ)
CDD    WRITE(6,*) 'STATUS,FORM,ACCESS',TYPE,FORM,ACCESS
CMSP           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
CMSP     %           ACCESS=ACCESS,ACTION='READ')
CVMS
#if defined(HIUXF77) || defined(MACHTEN_PPC) || defined(AIX)
           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
     %           ACCESS=ACCESS)
#else
           OPEN (LU,FILE=RFILE(1:LZ),STATUS=TYPE,ERR=3,FORM=FORM,
     %           ACCESS=ACCESS,READONLY)
#endif
CVMS
         END IF
       ENDIF
       IOPEN = 0
       RETURN
3      CONTINUE
CCC    CALL MSGER(ICOND,RFILE(1:LZ))
CCC    IOPEN = ICOND
       IOPEN = -1
       RETURN
       END
