C   08/01/87 711041729  MEMBER NAME  CHVAL    (FORT)     M  FORTRAN
C+
C (Filename) CHVAL
C (Purpose ) COM Utility Routine (extended version of MODVAL)
C (Author  ) A.Shirahashi
C (Date    ) 8-Jan-1987
C-
       SUBROUTINE CHVAL( QUEST,NVAL,NAMES,HELP,VARTYP,ARRAY )
C
C ARG
       INTEGER   *  4  NVAL
       CHARACTER * (*) NAMES(NVAL), HELP(NVAL), QUEST
       CHARACTER * (*) VARTYP
       INTEGER   *  4  ARRAY(NVAL)
C
C COMMON
       include 'comode.inc'
C
C VAR
       PARAMETER( MAXENT = 32 )
C
       INTEGER * 4  IBUF(MAXENT)
       REAL    * 4  RBUF(MAXENT)
       LOGICAL * 4  LBUF(MAXENT)
       EQUIVALENCE( IBUF,RBUF,LBUF )
C
       LOGICAL   * 4  MFLAG(MAXENT), MODIFY
       CHARACTER * 1  VTYPE(MAXENT)
C
       INTEGER   *  4  MAXNAM
       CHARACTER * 64  RESP, TEXT, STRG
C
       INTEGER * 4  LABEL
C
C FUNC
       INTEGER * 4  LENRD
C
C BEGIN
       IF( ICOMER .LT. 0 ) GOTO LABEL
       IF( LUNCOM .EQ. 0 ) CALL INICOM( 'BUG' )
C
       IF( NVAL .GT. MAXENT ) THEN
         WRITE( LUNCOM,1000 ) NVAL
1000     FORMAT( ' COM: Too Many Entries: ',I3,'in CHVAL' )
         RETURN
       END IF
C
       IF( LENRD( VARTYP ) .EQ. 1 ) THEN
         DO 10 J = 1, NVAL
           VTYPE( J ) = VARTYP( 1:1 )
10       CONTINUE
       ELSE
         DO 20 J = 1, NVAL
           VTYPE( J ) = VARTYP( J:J )
20       CONTINUE
       END IF
C
       MAXNAM = 0
       DO 100 J = 1, NVAL
         IF( VTYPE(J) .NE. 'I' .AND.
     &       VTYPE(J) .NE. 'R' .AND.
     &       VTYPE(J) .NE. 'L'
     &   ) THEN
           WRITE( LUNCOM,1010 ) VTYPE(J)
1010       FORMAT( ' COM: Unknown Variable Type: ',A1,' in CHVAL' )
           RETURN
         END IF
         IBUF( J ) = ARRAY( J )
         MFLAG( J ) = .FALSE.
         MAXNAM = MAX( MAXNAM, LENRD( NAMES( J ) ) )
100    CONTINUE
C
200    CONTINUE
       RESP = ' '
       MODIFY = .FALSE.
C
300    CONTINUE
C       IF( .NOT. BATCH ) CALL CM$CLR
       WRITE( LUNCOM,1020 ) COMPRM,QUEST
1020   FORMAT( ' ',/,' ',A,':  ',A,/,' ' )
C
       IF( RESP .EQ. '?' ) THEN
         CALL COMHLP( NVAL,NAMES,HELP )
       END IF
C
       DO 400 J = 1, NVAL
         CALL SHVAL( NAMES(J)(1:MAXNAM),VTYPE(J),
     &               ARRAY(J),MFLAG(J),IBUF(J) )
400    CONTINUE
C
       CALL LINRD( 1,1 )
500    CONTINUE
       CALL FLARD( IFLQ,IFCR )
       IF( .NOT.MODIFY ) THEN
         IF( IFLQ .NE. 0 ) THEN
           WRITE( LUNCOM,1030 ) COMPRM
1030       FORMAT( ' ',/,' ',A,': Press <ENTER> to Accept' )
         END IF
800      ASSIGN 800 TO LABEL
         CALL COMKEY( '?'//COMPRM//'> ',RESP )
         IF( ICOMER.NE.1 ) RETURN
         IF( RESP.EQ.' ' .OR. RESP.EQ.'OK' .OR. RESP.EQ.'ok' ) THEN
           CALL LINRD(1,1)
           RETURN
         END IF
       ELSE
         IF( IFLQ .NE. 0 ) THEN
           WRITE( LUNCOM,1040 ) COMPRM
1040       FORMAT( ' ',/,' ',A,': Type OK to Accept' )
         END IF
810      ASSIGN 810 TO LABEL
         CALL COMKEY( '?'//COMPRM//'> ',RESP )
         IF( ICOMER.GT.1 ) GOTO 200
         IF( ICOMER.NE.1 ) RETURN
         IF( RESP.EQ.'OK' .OR. RESP.EQ.'ok' ) THEN
           DO 600 J = 1, NVAL
             ARRAY( J ) = IBUF( J )
600        CONTINUE
           CALL LINRD(1,1)
           RETURN
         END IF
       END IF
       IF( RESP.EQ.' ' .OR. RESP.EQ.'?' ) GOTO 300
       CALL FLGRD( 0,0 )
       CALL COMTCH( NAMES,NVAL,RESP(1:MAXNAM),J )
       IF( J .LE. 0 ) THEN
         CALL LINRD( 1,1 )
         IF( ICOMER .EQ. 3 ) RETURN
         GOTO 500
       END IF
C
820    ASSIGN 820 TO LABEL
       CALL GETTXT( '?'//VTYPE(J)//'*4',TEXT )
       IF( ICOMER .NE. 1 ) RETURN
       IF( TEXT .EQ. ' ' ) GOTO 700
C
       IF( VTYPE(J) .EQ. 'I' ) THEN
         READ( TEXT,*,ERR=700 ) IBUF(J)
       ELSE IF( VTYPE(J) .EQ. 'R' ) THEN
         READ( TEXT,*,ERR=700 ) RBUF(J)
       ELSE IF( VTYPE(J) .EQ. 'L' ) THEN
         READ( TEXT,*,ERR=700 ) LBUF(J)
       END IF
C
       MODIFY = .TRUE.
       MFLAG( J ) = .TRUE.
       GOTO 500
C
700    CONTINUE
       CALL LINRD(1,1)
       WRITE( LUNCOM,* ) 'COM: Eroor in Decoding ',RESP(1:MAXNAM)
       GOTO 500
C
       END
C
C (* SUBROUTINE SHVAL *)
       SUBROUTINE SHVAL( NAME,VTYPE,VAL,MFLAG,NEW )
C
C ARG
       CHARACTER * (*)  NAME
       CHARACTER *  1   VTYPE
       INTEGER   *  4   VAL
       LOGICAL   *  4   MFLAG
       INTEGER   *  4   NEW
C
C COMMON
       include 'comode.inc'
C
C VAR
       INTEGER * 4  VALI, NEWI
       REAL    * 4  VALR, NEWR
       LOGICAL * 4  VALL, NEWL
       EQUIVALENCE( VALI,VALR,VALL )
       EQUIVALENCE( NEWI,NEWR,NEWL )
C
       CHARACTER * 16  STRVAL, STRNEW, NAME1
       INTEGER   *  4  LENVAL, LENNEW
C
C BEGIN
       VALI = VAL
       NEWI = NEW
C
       IF( VTYPE .EQ. 'I' ) THEN
         WRITE( STRVAL,* ) VALI
         WRITE( STRNEW,* ) NEWI
       ELSE IF( VTYPE .EQ. 'R' ) THEN
         CALL CNVFLT( VALR,STRVAL )
         CALL CNVFLT( NEWR,STRNEW )
       ELSE IF( VTYPE .EQ. 'L' ) THEN
         WRITE( STRVAL,* ) VALL
         WRITE( STRNEW,* ) NEWL
       END IF
C
       LENVAL = LENRD( STRVAL )
       LENNEW = LENRD( STRNEW )
C
       IF( MFLAG ) THEN
         WRITE( LUNCOM,* ) NAME,' = ',STRVAL(1:LENVAL),
     &                                      ' ',STRNEW(1:LENNEW)
       ELSE
         WRITE( LUNCOM,* ) NAME,' = ',STRVAL(1:LENVAL)
       END IF
C
       RETURN
       END
C
C (* SUBROUTINE CNVFLT *)
       SUBROUTINE CNVFLT( VAL,STR )
C
C ARG
       REAL      *  4   VAL
       CHARACTER * (*)  STR
C
C VAR
       CHARACTER * 16  FORM
C
C FUNC
       INTEGER * 4  IVFORM
C
C BEGIN
       LS = IVFORM( 'G',LEN(STR),6,VAL,STR )
       IE = INDEX( STR,'E' )
       IF( IE .EQ. 0 ) IE = LS + 1
       I = IE - 1
100    CONTINUE
         IF( STR(I:I) .NE. '0' ) GOTO 110
         I = I - 1
       GOTO 100
110    CONTINUE
       IF( STR(I:I+1) .EQ. '.0' ) I = I + 1
       IF( IE .GT. LS ) THEN
         STR = STR(1:I)
         IF( STR(I:I) .EQ. '.' ) THEN
           LS = IVFORM( 'F',LEN(STR),1,VAL,STR )
         END IF
         RETURN
       END IF
       READ( STR(IE+1:LS),* ) NP
       IF( NP .EQ. 0 ) THEN
         STR = STR(1:I)
         RETURN
       END IF
       NE = I - INDEX( STR,'.' )
       IF( NP .LT. 0 .AND. 6 - NE .GE. -NP ) THEN
         LS = IVFORM( 'F',LEN(STR),NE-NP,VAL,STR )
         RETURN
       END IF
       STR = STR(1:I)//STR(IE:)
       RETURN
       END
C
C (* FUNCTION IVFORM *)
       INTEGER FUNCTION IVFORM( EDIT,NW,ND,VAL,STR )
C
C ARG
       CHARACTER * (*)  EDIT
       INTEGER   *  4   NW,ND
       REAL      *  4   VAL
       CHARACTER * (*)  STR
C
C VAR
       CHARACTER * 16  FORM
C
C FUNC
       INTEGER * 4  LKBRD
C
C BEGIN
       WRITE( FORM,1000 ) EDIT,NW,ND
1000   FORMAT( '(',A,I2,'.',I2,')' )
       WRITE( STR,FORM ) VAL
       IVFORM = LKBRD( STR,0 )
       RETURN
       END
