CC*********************************************************************C
C*
C*---------------------------------=====
C* Subroutine DBCANL(NCMD, OPERND, NRET)
C*---------------------------------=====
C*
C*(Purpose)
C*    Analize operand, and take appropreate action.
C*
C*(Input)
C*    NCMD   ; Command code.
C*    OPERND ; Input command operand.
C*
C*(Output)
C*    NRET   ; Return code.
C*         = 0 ; for normal return.
C*         < 0 ; in case of error.
C*
C*(Author)
C*    A. Miyamoto   13-Jun-1986   Original version.
C*
CC**********************************************************************
C
      SUBROUTINE DBCANL(NCMD, OPERND, NRET)
C
      CHARACTER*(*) OPERND
#ifdef  MSP
      CHARACTER     OPRNAM*16, ARGMNT*80, FRMT*10, BLNK*80
#endif
#ifdef  UNIX
       CHARACTER     OPRNAM*16, ARGMNT*200, FRMT*10, BLNK*200
#endif
#ifdef DEC
	integer*8 IADR
#endif
      DATA   BLNK/' '/
C
#include "dbcmdb.inc"
#include "prgenf.inc"
C
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Initialization.
C ----------------------------------------------------------------------
C
      NRET   = 0
      LENOPR = LEN(OPERND)

	print *,' In dbcanl ... opr=',OPERND(:LENOPR)
C
C (1.1) Clear value for Type -4 variable.
C
      IPNT   = NDBCTL(2,NCMD)
100   CONTINUE
      IF(IPNT.LE.0) GO TO 110
      IF(NDBCTL(3,IPNT).EQ.-4) THEN
        CALL UVSET(NDBCTL(4,IPNT),0)
      ENDIF
      IPNT = NDBCTL(2,IPNT)
      GO TO 100
110   CONTINUE
C
C ----------------------------------------------------------------------
C (2) Obtain operand name and argument.
C ----------------------------------------------------------------------
C
C  IBLNK = 1, while looking for non-blank character, otherwise 0
C
      ILST = 1
      I1ST = 1
      IBLNK= 1
C
C (2.1) Looking for Operand name.
C
200   CONTINUE
      IF(ILST.GT.LENOPR) GO TO 900
      IF(OPERND(ILST:ILST).EQ.'(') THEN
        IF(I1ST.EQ.ILST) THEN
          PRINT *,'%Error DBCANL .. No operand name specified.'
          GO TO 990
        ENDIF
        OPRNAM = OPERND(I1ST:ILST-1)
        LOPR   = MIN(16, ILST-I1ST)
        GO TO 210
      ELSEIF(OPERND(ILST:ILST).EQ.' '.AND.IBLNK.EQ.0) THEN
        IF(I1ST.EQ.ILST) THEN
          PRINT *,'%Error DBCANL .. No operand name specified.'
          GO TO 990
        ENDIF
        OPRNAM = OPERND(I1ST:ILST-1)
        LOPR   = MIN(16, ILST-I1ST)
        LARG   = 0
        GO TO 300
      ELSEIF(OPERND(ILST:ILST).NE.' '.AND.IBLNK.EQ.1) THEN
        I1ST   = ILST
        IBLNK  = 0
      ENDIF
      ILST     = ILST + 1
      GO TO 200
C
C (2.2) Looking for the closed blaket.
C
210   CONTINUE
 
      NKET  = 1
      ILST  = ILST + 1
      IKET  = ILST
220   CONTINUE
      IF(ILST.GT.LENOPR) THEN
        PRINT *,'%Error DBCANL .. Blaket un-balanced.'
        GO TO 990
      ENDIF
      IF(OPERND(ILST:ILST).EQ.'(') THEN
        NKET = NKET + 1
      ELSEIF(OPERND(ILST:ILST).EQ.')') THEN
        NKET = NKET - 1
        IF(NKET.LE.0) THEN
          IF(ILST.LE.IKET) THEN
            LARG = 0
          ELSE
            ARGMNT = OPERND(IKET:ILST-1)
            LARG   = ILST - IKET
          ENDIF
          GO TO 300
        ENDIF
      ENDIF
      ILST = ILST + 1
      GO TO 220
C
C ----------------------------------------------------------------------
C (3) Search corresponding operand, and fill value.
C ----------------------------------------------------------------------
C
300   CONTINUE
      CALL UCUPER(16, OPRNAM)
      CALL DBxFDO(NCMD, OPRNAM, IOPR)
      IF(IOPR.LE.0) THEN
        PRINT *,'%Error DBCANL .. Operand ',OPRNAM(:LOPR),' is not ',
     .          ' defined for command ',DBCMDN(NCMD)
        GO TO 990
      ENDIF
C
C (3.1) For no-argument operand.
C

	

      IOPTYP = NDBCTL(3,IOPR)

	print *,' OPRNAM =',OPRNAM,' IOPTYP=',IOPTYP



      IF( IOPTYP.GT.-5 ) CALL UCUPER(LARG, ARGMNT)
      IF(IOPTYP.EQ.-4) THEN
        CALL UVSET(NDBCTL(4,IOPR),1)
        GO TO 400
      ENDIF
C
      IF(LARG.LE.0) THEN
        PRINT *,'%Error DBCANL .. no value is specified for operand ',
     .          OPRNAM(:LOPR),' of command ',DBCMDN(NCMD)
        GO TO 990
C
C (3.2) For Yes/No or On/Off operand.
C
      ELSEIF(IOPTYP.EQ.-3) THEN
        IYES = INDEX(ARGMNT(:LARG),'YES')
        ION  = INDEX(ARGMNT(:LARG),'ON')
        IF(IYES.EQ.1.OR.ION.EQ.1) THEN
          CALL UVSET(NDBCTL(4,IOPR),1)
        ELSE
          CALL UVSET(NDBCTL(4,IOPR),0)
        ENDIF
C
C (3.3) For R*4 operand.
C
      ELSEIF(IOPTYP.EQ.-2) THEN
        READ(ARGMNT(:LARG),*) VAL
        CALL UVSET(NDBCTL(4,IOPR),VAL)
C
C (3.4) For I*4 operand.
C
      ELSEIF(IOPTYP.EQ.-1) THEN
        IHEX = INDEX(ARGMNT(:LARG),'Z')
        IBIN = INDEX(ARGMNT(:LARG),'B')
        IF(IHEX.EQ.0.AND.IBIN.EQ.0) THEN
          READ(ARGMNT(:LARG),*) IVAL
        ELSEIF(IHEX.NE.0.AND.IBIN.EQ.0) THEN
          DO 330 I  = LARG, IHEX, -1
            IF(ARGMNT(I:I).NE.' ') GO TO 335
330       CONTINUE
335       CONTINUE
          LENG = I - IHEX
          IF(LENG.GT.8) LENG = 8
          WRITE(FRMT,'(''(Z'',I1,'')'')') LENG
          READ(ARGMNT(IHEX+1:LARG),FRMT) IVAL
        ELSEIF(IBIN.NE.0.AND.IHEX.EQ.0) THEN
          IVAL = 0
          ICNT = -1
          DO 340 IP = LARG, IBIN, -1
            IF(ICNT.EQ.-1.AND.
     >         ARGMNT(IP:IP).NE.'1'.AND.ARGMNT(IP:IP).NE.'0') GO TO 340
            IF(ICNT.EQ.-1) THEN
              ICNT = 1
            ELSE
              ICNT = 2*ICNT
            ENDIF
            IF(ARGMNT(IP:IP).EQ.'1') IVAL = IVAL + ICNT
340       CONTINUE
        ENDIF
        CALL UVSET(NDBCTL(4,IOPR),IVAL)	
C
C (3.5) For C*n operand.
C
      ELSEIF(IOPTYP.GT.0) THEN
        DO 350 I1 = 1, LARG
          IF(ARGMNT(I1:I1).NE.' ') GO TO 352
350     CONTINUE
352     DO 354 I2 = LARG,I1,-1
          IF(ARGMNT(I2:I2).NE.' ') GO TO 356
354     CONTINUE
356     CONTINUE
        LFIL = I2 - I1 + 1
        IF(LFIL.GT.0) THEN
          CALL UVSETC(NDBCTL(4,IOPR),ARGMNT(I1:),LFIL)
          LREST = IOPTYP - LFIL
        ELSE
          LREST = IOPTYP
        ENDIF
        IF(LREST.GT.0) THEN
          IADR  = NDBCTL(4,IOPR) + LFIL
          CALL UVSETC(IADR,BLNK,LREST)
        ENDIF
C
C (3.6) For S*n operand.
C
      ELSEIF(IOPTYP.LT.-5) THEN
	JOPTYP = IABS(IOPTYP)
        DO 360 I1 = 1, LARG
          IF(ARGMNT(I1:I1).NE.' ') GO TO 362
360     CONTINUE
362     DO 364 I2 = LARG,I1,-1
          IF(ARGMNT(I2:I2).NE.' ') GO TO 366
364     CONTINUE
366     CONTINUE
        LFIL = I2 - I1 + 1
        IF(LFIL.GT.0) THEN
          CALL UVSETC(NDBCTL(4,IOPR),ARGMNT(I1:),LFIL)
          LREST = JOPTYP - LFIL
        ELSE
          LREST = JOPTYP
        ENDIF
        IF(LREST.GT.0) THEN
          IADR  = NDBCTL(4,IOPR) + LFIL
          CALL UVSETC(IADR,BLNK,LREST)
        ENDIF
      ENDIF
C
C ----------------------------------------------------------------------
C (4) Prepare search for next operand.
C ----------------------------------------------------------------------
C
400   CONTINUE
      ILST  = ILST + 1
      I1ST  = ILST
      ILST  = I1ST
      IBLNK = 1
      GO TO 200
C
C ----------------------------------------------------------------------
C (9) For return .
C ----------------------------------------------------------------------
C
900   CONTINUE
      NRET = 0


	print *,' NUMGEN, IUMGEN =',NUMGEN,IUMGEN



      RETURN
C
C (9.2) Error return
C
990   CONTINUE
      NRET = -1
      RETURN
      END
