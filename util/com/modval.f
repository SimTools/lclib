C   11/11/86 701081945  MEMBER NAME  MODVAL   (FORT)     M  FORTRAN
C+++++ THIS IS MODVAL.FOR
C
       SUBROUTINE MODVAL(QUEST,NVAL,NAMES,HELP,VARTYP,ARRAY)
C
C@@    IMPLICIT NONE
c__    include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 NVAL
       CHARACTER*(*) NAMES(NVAL), HELP(NVAL), QUEST
       CHARACTER*1   VARTYP
       INTEGER * 4   ARRAY ( * )
       INTEGER * 4   SIZE
C@@@   INTEGER*4 DESCR(2), BESCR(2)
       INTEGER*4 ITYPE, LABEL, ISTAT, I, J, II
       INTEGER*4 LENX, NLEN, IFLQ, IFCR
       LOGICAL*4 LCMVAL
       INTEGER*4 ICMVAL
       REAL*4    RCMVAL
       CHARACTER*64 CCMVAL, RESP, BESP
       INTEGER * 4   BSIZE
       PARAMETER ( BSIZE = 640 )
       LOGICAL*1 MODIF, MFLAG(32)
       LOGICAL * 1      BUFFER (BSIZE)
       LOGICAL * 4      LBUF (BSIZE/4)
       INTEGER * 4      IBUF (BSIZE/4)
       REAL    * 4      RBUF (BSIZE/4)
       CHARACTER * 32   CBUF ( 20 )
       EQUIVALENCE ( BUFFER, IBUF, LBUF, RBUF, CBUF )
C
       INTEGER * 4     CHRLEN
C
C..... MODIFY A TABLE OF VALUES (L*4, I*4, R*4 OR CHAR)
C      Support for CHAR type was temporary removed in FACOM vers. (RSI)
C
       IF (ICOMER.LT.0) GOTO LABEL
       IF (LUNCOM.EQ.0) CALL INICOM('BUG')
C@@@   ITYPE = IAND ( ISHFT(DESCR(1),-16), 255 )
       IF (VARTYP .NE. 'L' .AND. VARTYP .NE. 'I' .AND.
     &     VARTYP .NE. 'R' ) THEN
         WRITE (LUNCOM,104) VARTYP
 104     FORMAT (' COM: ILLEGAL VARIABLE TYPE:',A1,' IN MODVAL')
         RETURN
       END IF
       IF (VARTYP.EQ. 'C') THEN
         LENX = CHRLEN ( ARRAY )
       ELSE
         LENX = 4
       END IF
       IF (LENX*NVAL.GT.BSIZE) THEN
         WRITE (LUNCOM,105) LENX*NVAL,BSIZE
 105     FORMAT (' COM: TOO MANY CHARACTERS:',I5,'.GT.',I4,' IN MODVAL')
         RETURN
       END IF
C
C..... SET THE INITIAL VALUES
C
 3     RESP = ' '
       MODIF = .FALSE.
       DO 10 I = 1, NVAL
         MFLAG(I) = .FALSE.
         IF ( VARTYP .EQ. 'C' ) THEN
           CALL CHRCPY ( ARRAY, CBUF, I )
         ELSE
           IBUF ( I ) = ARRAY ( I )
         END IF
10     CONTINUE
C@@@   BESCR(1) = DESCR(1)
C@@@   BESCR(2) = EXTSUB(BUFFER)
C@@@   CALL COMCPY(NVAL,DESCR,BESCR)
C
C..... TYPE THE OPTIONS
C
 1     CONTINUE
C      IF ( .NOT. BATCH ) CALL CM$CLR
       WRITE (LUNCOM,110) COMPRM,QUEST
 110   FORMAT (' '/' ',A,':  ',A/' ')
       IF (RESP.EQ.'?') THEN
         CALL COMHLP(NVAL,NAMES,HELP)
       END IF
       IF (VARTYP.EQ.'L') THEN
         DO 20 J = 1, NVAL
           IF (MFLAG(J)) THEN
             WRITE (LUNCOM,*) NAMES(J),' ',
     &       LCMVAL(ARRAY(J)),LBUF(J)
           ELSE
             WRITE (LUNCOM,*) NAMES(J), ' ',
     &       LCMVAL(ARRAY(J))
           END IF
20       CONTINUE
       ELSE IF (VARTYP.EQ.'I') THEN
         DO 30 J = 1, NVAL
           IF (MFLAG(J)) THEN
             WRITE (LUNCOM,*) NAMES(J), ' ',
     &       ICMVAL(ARRAY(J)),IBUF(J)
           ELSE
             WRITE (LUNCOM,*) NAMES(J),' ',
     &       ICMVAL(ARRAY(J))
           END IF
30       CONTINUE
       ELSE IF (VARTYP.EQ.'R') THEN
         DO 40 J = 1, NVAL
           IF (MFLAG(J)) THEN
             WRITE (LUNCOM,*) NAMES(J),' ',
     &       RCMVAL(ARRAY(J)),RBUF(J)
           ELSE
             WRITE (LUNCOM,*) NAMES(J),' ',
     &       RCMVAL(ARRAY(J))
           END IF
40       CONTINUE
       ELSE IF (VARTYP.EQ.'C') THEN
         DO 50 J = 1, NVAL
           RESP = CCMVAL(ARRAY(J))
           BESP = CCMVAL(CBUF(J))
           IF (MFLAG(J)) THEN
             WRITE (LUNCOM,113) NAMES(J),RESP(1:LENX),BESP(1:LENX)
 113         FORMAT (1X,A,2X,A,2X,A)
           ELSE
             WRITE (LUNCOM,113) NAMES(J),RESP(1:LENX)
           END IF
50       CONTINUE
       END IF
C
C..... GET A KEYWORD
C
       CALL LINRD(1,1)
 2     CALL FLARD(IFLQ,IFCR)
       IF (.NOT.MODIF) THEN
         IF (IFLQ.NE.0) WRITE (LUNCOM,122) COMPRM
 122     FORMAT (' '/' ',A,': Press <ENTER> to Accept')
 80      ASSIGN 80 TO LABEL
         CALL COMKEY('?'//COMPRM//'> ',RESP)
         IF (ICOMER.NE.1) RETURN
         IF (RESP.EQ.' ' .OR. RESP.EQ.'OK' .OR. RESP.EQ.'ok') THEN
           CALL LINRD(1,1)
           RETURN
         END IF
       ELSE
         IF (IFLQ.NE.0) WRITE (LUNCOM,123) COMPRM
 123     FORMAT (' '/' ',A,': Type OK to Accpet')
 81      ASSIGN 81 TO LABEL
         CALL COMKEY('?'//COMPRM//'> ',RESP)
         IF (ICOMER.GT.1) GOTO 3
         IF (ICOMER.NE.1) RETURN
         IF (RESP.EQ.'OK' .OR. RESP.EQ.'ok') THEN
C@@@       CALL COMCPY(NVAL,BESCR,DESCR)
           DO 601 I = 1, NVAL
             IF ( VARTYP .EQ. 'C' ) THEN
               CALL CHRCPY ( CBUF, ARRAY, I )
             ELSE
               ARRAY ( I ) = IBUF ( I )
             END IF
601        CONTINUE
           CALL LINRD(1,1)
           RETURN
         END IF
       END IF
       IF (RESP.EQ.' ' .OR. RESP.EQ.'?') GOTO 1
       CALL FLGRD(0,0)
       NLEN = LEN(NAMES(1))
       CALL COMTCH(NAMES,NVAL,RESP(1:NLEN),J)
       IF (J.LE.0) THEN
         CALL LINRD(1,1)
         IF (ICOMER.EQ.3)  RETURN
         GOTO 2
       END IF
C
C..... GET THE VALUE
C
       IF (VARTYP.EQ.'L') THEN
 82      ASSIGN 82 TO LABEL
         CALL LCMMOD(BUFFER,J,ISTAT)
         IF (ICOMER.NE.1) RETURN
       ELSE IF (VARTYP.EQ.'I') THEN
 83      ASSIGN 83 TO LABEL
         CALL ICMMOD(BUFFER,J,ISTAT)
         IF (ICOMER.NE.1) RETURN
       ELSE IF (VARTYP.EQ.'R') THEN
 84      ASSIGN 84 TO LABEL
         CALL RCMMOD(BUFFER,J,ISTAT)
         IF (ICOMER.NE.1) RETURN
       ELSE IF (VARTYP.EQ.'C') THEN
 85      ASSIGN 85 TO LABEL
         CALL CCMMOD(CBUF,J,ISTAT)
         IF (ICOMER.NE.1) RETURN
       END IF
       IF (ISTAT.NE.1) THEN
         CALL LINRD(1,1)
         WRITE (LUNCOM,*) 'COM: ERROR DECODING ',RESP(1:NLEN)
         GOTO 2
       END IF
       MODIF = .TRUE.
       MFLAG(J) = .TRUE.
       GOTO 2
       END
C
       FUNCTION LCMVAL( VALUE )
C@@    IMPLICIT  NONE
       LOGICAL*4 VALUE, LCMVAL
C
C..... RETURN A LOGICAL FROM DESCRIPTOR
C
       LCMVAL = VALUE
       END
C
C====================
C
       FUNCTION ICMVAL(VALUE)
C@@    IMPLICIT  NONE
       INTEGER*4 VALUE, ICMVAL
C
C..... RETURN AN INTEGER FROM DESCRIPTOR
C
       ICMVAL = VALUE
       END
C
C====================
C
       FUNCTION RCMVAL(VALUE)
C@@    IMPLICIT  NONE
       REAL*4 VALUE, RCMVAL
C
C..... RETURN A REAL FROM DESCRIPTOR
C
       RCMVAL = VALUE
       END
C
C====================
C
       CHARACTER * (*) FUNCTION CCMVAL(VALUE)
C@@    IMPLICIT  NONE
       CHARACTER*(*) VALUE
C
C..... RETURN A CHARACTER STRING FROM DESCRIPTOR
C
       CCMVAL = VALUE
       END
C
       SUBROUTINE LCMMOD(L,J,ISTAT)
C@@    IMPLICIT  NONE
c__    include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 J, ISTAT
       LOGICAL*4 L(J), M
       CHARACTER*8 TEXT
C@@@   INTEGER*4 OTS$CVT_TL_L
C
C..... MODIFY A LOGICAL FROM DESCRIPTOR
C
       CALL GETTXT('?L*4',TEXT)
       IF (ICOMER.NE.1) RETURN
       ISTAT = 2
       IF (TEXT.EQ.' ') RETURN
C@@@   ISTAT = OTS$CVT_TL_L(TEXT,M,%VAL(4))
C@@@   IF (ISTAT.NE.1) RETURN
       READ ( TEXT, '(L1)' ) M
       L(J) = M
       ISTAT = 1
       END
C
C====================
C
       SUBROUTINE ICMMOD(I,J,ISTAT)
C@@    IMPLICIT  NONE
c__    include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 J, ISTAT
       INTEGER*4 I(J), K
       CHARACTER*16 TEXT
       CHARACTER*5  RUNFRM
       INTEGER*4    LTEXT
C@@@   INTEGER*4 OTS$CVT_TI_L
       INTEGER*4    LKBRD
C
C..... MODIFY AN INTEGER FROM DESCRIPTOR
C
       CALL GETTXT('?I*4',TEXT)
       IF (ICOMER.NE.1) RETURN
       ISTAT = 2
       IF (TEXT.EQ.' ') RETURN
C@@@   ISTAT = OTS$CVT_TI_L(TEXT,K,%VAL(4),%VAL(1))
C@@@   IF (ISTAT.NE.1) RETURN
       LTEXT = LKBRD ( TEXT, 0 )
       WRITE ( RUNFRM, '(A,I2,A)' ) '(I',LTEXT,')'
       LTEXT = LKBRD ( RUNFRM, 0 )
C@@@   WRITE ( 6, * ) 'LTEXT = ', LTEXT, ' RUNFORMAT = ', RUNFRM
       READ ( TEXT, RUNFRM(1:LTEXT) ) K
       I(J) = K
       ISTAT = 1
       END
C
C====================
C
       SUBROUTINE RCMMOD(R,J,ISTAT)
C@@    IMPLICIT  NONE
c__    include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 J, ISTAT
       REAL*4 R(J), S
       CHARACTER*24 TEXT
C@@@   INTEGER*4 OTS$CVT_T_F
C
C..... MODIFY A REAL FROM DESCRIPTOR
C
       CALL GETTXT('?R*4',TEXT)
       IF (ICOMER.NE.1) RETURN
       ISTAT = 2
       IF (TEXT.EQ.' ') RETURN
C@@@   ISTAT = OTS$CVT_T_F(TEXT,S,,,%VAL(1))
C@@@   IF (ISTAT.NE.1) RETURN
       READ ( TEXT, '(F12.5)' ) S
       R(J) = S
       ISTAT = 1
       END
C
C====================
C
       SUBROUTINE CCMMOD(C,J,ISTAT)
C@@    IMPLICIT  NONE
c__    include 'comode.inc'
       include 'comode.inc'
       INTEGER*4 J, ISTAT
       CHARACTER C(J)*(*), TEXT*64
C
C..... MODIFY A CHARACTER STRING FROM DESCRIPTOR
C
       CALL GETTXT('?C* ',TEXT)
       IF (ICOMER.NE.1) RETURN
       ISTAT = 2
       IF (TEXT.EQ.' ') RETURN
       C(J) = TEXT
       ISTAT = 1
       END
C
C----------
C CHRLEN
C----------
C
      INTEGER FUNCTION CHRLEN ( ARRAY )
C
C ARGUMENTS
C
      CHARACTER * (*) ARRAY (*)
C
C MAIN
C
      CHRLEN = LEN ( ARRAY(1) )
      RETURN
      END
C
C----------
C CHRCPY
C----------
C
      SUBROUTINE CHRCPY ( INARY, OUTARY, IND )
C
C ARGUMENTS
C
      CHARACTER * (*)  INARY(*), OUTARY(*)
      INTEGER * 4      IND
C
C MAIN
C
      OUTARY ( IND ) = INARY ( IND )
      RETURN
      END
