C******************************************************************
C*
C* -------------------------============
C*  Subroutine CNVNAM(INAME, ONAME, LENO)
C* -------------------------============
C*(Function)
C*  Convert FACOM's character name to the directory name
C*  suitable on UNIX.
C*  If NO file name in the table matches, INAME is returned to FNAME
C*(Input)
C*  INNAME ; FACOM's character name.
C*         = "INITIALIZE" to load conversion table into memory.
C*(Output)
C*  ONAME  ; Proper directory name on UNIX(Directory name end with /).
C*  LENO   ; Valid character length
C*(Author)
C*  A.Miyamoto  3-May-1994
C*
C*******************************************************************
C
      SUBROUTINE CNVNAM(INAME, ONAME, LENO)
C
      CHARACTER*(*) INAME, ONAME
      INTEGER*4     LENO
C
      PARAMETER (MAXBUF=200)
      CHARACTER*44   FNAME(MAXBUF)
      CHARACTER*100 UNAME(MAXBUF)
      INTEGER*4     LENF(MAXBUF), LENU(MAXBUF)
      SAVE  NONAME, FNAME, UNAME
      DATA  NONAME/0/
      CHARACTER*100 DICTF
      CHARACTER*150 INPUT
CSUN       INTEGER*4     ACCESS
C
C =======< Entry Point >==========================================
C
      IF( INAME(1:10).EQ.'INITIALIZE') THEN
        NONAME=0
        DICTF=' ' 
#if HP
	CALL GETENV('HOME'//CHAR(0),DICTF)  ! for HP Unix.
#else
	CALL GETENV('HOME',DICTF)
#endif
        ILB = LNBLNK(DICTF)
        DICTF(ILB+1:)='/.buildrc'

c             print *, 'Reading conv table from ', DICTF(:lnblnk(DICTF))

CSUN         IR  = ACCESS(DICTF,' ')
CSUN         IF( IR.NE.0 ) RETURN

        OPEN(9,FILE=DICTF(:LNBLNK(DICTF)),STATUS='OLD',IOSTAT=IR)
        IF( IR.NE.0 ) THEN
           PRINT *, 'Failed to open ', DICTF(:LNBLNK(DICTF))
           PRINT *, 'IOSTAT = ', IOSTAT
           RETURN
        ENDIF
        
        DO 100 I = 1, MAXBUF
           INPUT =' '
           READ(9,'(A)',END=190) INPUT
           LIN = LNBLNK(INPUT)
           DO 110 K = 1, LIN
              IF( INPUT(K:K).NE.' ') THEN
                 I1=K
                 I2=K
                 GO TO 120
              ENDIF
 110       CONTINUE
           GO TO 100
 120       CONTINUE
           DO 130 K = I1, LIN
              IF( INPUT(K:K).EQ.' ') THEN
                 I2=K-1
                 GO TO 140
              ENDIF
 130       CONTINUE
           GO TO 100
 140       CONTINUE
           DO 150 K = I2+1, LIN
              IF( INPUT(K:K).NE.' ') THEN
                 I3=K
                 GO TO 160
              ENDIF
 150       CONTINUE
           GO TO 100
 160       CONTINUE

           NONAME = NONAME + 1
           IF( NONAME.GT.MAXBUF) THEN
              PRINT *,'%Error..BUILD...Too many rules in the file ',
     >        DICTF
              GO TO 190
           ENDIF
           FNAME(NONAME)=INPUT(I1:I2)
           LENF(NONAME)=I2-I1+1
           UNAME(NONAME)=INPUT(I3:LIN)
           LENU(NONAME)=LIN-I3+1
 100     CONTINUE
 190     CONTINUE
         CLOSE(9)
#ifdef BUILD_DEBUG
         PRINT *,' NONAME =',NONAME
         DO 195 I = 1, NONAME
            PRINT *,' FNAME =',FNAME(I)(:LENF(I)),'==='
            PRINT *,' UNAME =',UNAME(I)(:LENU(I)),'==='
195      CONTINUE
#endif
         RETURN
      ENDIF
C
C .. Search tables in the file.
C        
      LENI = LEN(INAME)
      ONAME= INAME
      LENO = LENI
      DO 300 I = 1, NONAME
        IF( INAME(:LENI).NE.FNAME(I)(1:LENF(I)) ) GO TO 300
        ONAME = UNAME(I)
        LENO  = LENU(I)
        GO TO 400
 300  CONTINUE
 400  CONTINUE
#ifdef BUILD_DEBUG
      print *,' LENI=',LENI,' LENO=',LENO,' NONAM =',NONAME
      PRINT *,' INAME =',INAME(:LENI),'==='
      PRINT *,' ONAME =',ONAME(:LENO),'==='
#endif
      RETURN
      END
