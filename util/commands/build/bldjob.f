C***********************************************************************
C* 
C*  -----------------------------------------===============
C*  Subroutine Bldjob( INPUT, OUTDCB, OUTDDN, IRET , JOBNAM)
C*  -----------------------------------------===============
C* 
C*(Function)
C*   Complete the Job card supplied from the input data.
C*   Main tasks are to construct job name, and fill the password
C*   and the message class.
C* 
C*(Input)
C*   INPUT  : Input JCL card.
C*   OUTDCB : DCB data for output file.
C*   OUTDDN : Output dd name.
C*   IRET   : Indicatting how many times this routine is called.
C*            At first, this routine should be called with IRET = 1
C* 
C*(Output)
C*   IRET   : Return code.
C*          <  0, if error hapens.
C*             0, when completed job card.
C*          >  0, expecting continuation of job card.
C* 
C*(Author)
C*   A. Miyamoto   9-Oct-1985
C*                25-Oct-1985  To match BUILD version 3.
C* 
C***********************************************************************
C  
      SUBROUTINE BLDJOB( INPUT, OUTDCB, OUTDDN, IRET, JOBNAM)
C  
      CHARACTER*80   INPUT, OUTPUT, WORK1
      CHARACTER      USERID*4, JOBNAM*8, PSWD*8, MSGC*1, OUTDDN*8
      CHARACTER*20   OUTFMT
      INTEGER*4      OUTDCB(50)
      DATA           IUSER/0/,I1ST/3/, ILST/10/
      DATA           IDEST/0/,LMSGC/0/,LNOTFY/0/
      DATA           OUTFMT/'(A80)'/
      DATA           LOCHR/80/
C  
C  (1) First call to this routine. Check job name.
C  
      IF(IRET.GT.1.AND.INPUT(1:3).EQ.'//*') THEN
        CALL PUT(OUTDCB,INPUT,ILL,80)
        IF(ILL.NE.0) GO TO 2000
        RETURN
      ENDIF
C  
      DO 50 I = 72, 3, -1
         IF(INPUT(I:I).NE.' ') GO TO 90
50    CONTINUE
        PRINT *,'%Error BAT ... No valid Job statement.'
        PRINT *,' Error line is,'
        PRINT *,INPUT
        IRET = -999
        RETURN
90    CONTINUE
      INLAST = I
      ICONT  = 0
      IF(INPUT(INLAST:INLAST).EQ.',') THEN
        ICONT = 1
        INLAST = INLAST - 1
      ENDIF
      IF(IRET.NE.1) GO TO 500
C  
C  (1.1) Is it JCL card ? Isn't it comment card ?
C  
      IF(INPUT(1:2).NE.'//'.OR.INPUT(1:3).EQ.'//*') THEN
        PRINT *,'%Error BAT .. First card is not job card. Input is'
        PRINT *,INPUT
        IRET = -1
        RETURN
      ENDIF
C  
C (1.2) Is it valid jobname ?
C  
      CALL GETUID(USERID)
      IF(INPUT(3:6).EQ.'XXXX') THEN
         INPUT(3:6) = USERID
C        IUSER = 1
         I1ST  = 7
      ELSEIF(INPUT(3:3).NE.USERID(1:1)) THEN
         PRINT *,'%Error BAT..First character of the job name ',
     >           'must be ',USERID(1:1),' for your group.'
         PRINT *,'Error line is,'
         PRINT *,INPUT
         IRET = -2
         RETURN
      ELSEIF(INPUT(3:6).NE.USERID) THEN
         IUSER = 1
      ENDIF
C  
C Validate JOBNAME.
C  
      DO 120 IPNT = I1ST, ILST
         IF(INPUT(IPNT:IPNT).EQ.' ') GO TO 140
         ICODE = ICHAR(INPUT(IPNT:IPNT))
         IF((ICODE.GE.193.AND.ICODE.LE.201).OR.
     >      (ICODE.GE.209.AND.ICODE.LE.217).OR.
     >      (ICODE.GE.226.AND.ICODE.LE.233).OR.
     >      (ICODE.GE.240.AND.ICODE.LE.249).OR.
     >       ICODE.EQ.91.OR.
     >       ICODE.EQ.123.OR.ICODE.EQ.124) GO TO 120
   
         PRINT *,'%Error BAT ..Invalid character in JOB name field.'
         PRINT *,'Error line is,'
         PRINT *,INPUT
         IRET = -3
         RETURN
120   CONTINUE
      I1ST = ILST + 1
      GO TO 150
140   CONTINUE
      I1ST = IPNT
150   CONTINUE
      JOBNAM = INPUT(3:I1ST-1)
C  
C (1.3) Is there JOB word ?
C  
      IJOB = INDEX(INPUT(I1ST:INLAST+2),' JOB ')
      IF(IJOB.EQ.0) THEN
        PRINT *,'%Error BAT .. No Job statement in first JCL Card.'
        PRINT *,' Error line is,'
        PRINT *,INPUT
        IRET = -4
        RETURN
      ENDIF
   
      I1ST   = I1ST + IJOB + 4
C  
C (2) Search password.
C  
      CALL PASWRD(PSWD)
      CALL BGMCLS(MSGC)
C     PRINT *,' USERID=',USERID,' PASWRD=',PSWD,' CLASS=',MSGC
      DO 200 IPNT = I1ST, INLAST
        IF(INPUT(IPNT:IPNT).NE.' ') GO TO 210
200   CONTINUE
C  
C ... Case 1.  Only Job statement. Fill password and job class.
C  
      IF(IUSER.EQ.1) THEN
        CALL PROMPT(2,'Enter password for JOB '//JOBNAM//' >>',
     >                IRTN)
        READ(5,'(A8)') PSWD
        CALL CUPPER(PSWD)
      ENDIF
      OUTPUT = '//'//JOBNAM//' JOB '//PSWD
      LOUT   = 24
C  
205   CONTINUE
      IF(ICONT.EQ.0) THEN
        OUTPUT(LOUT:) = ',MSGCLASS='//MSGC//',NOTIFY='//USERID
        CALL PUT(OUTDCB,OUTPUT,ILL,80)
        IF(ILL.NE.0) GO TO 2000
        IRET = 0
        RETURN
      ELSE
        OUTPUT(LOUT:LOUT)  = ','
        CALL PUT(OUTDCB,OUTPUT,ILL,80)
        IF(ILL.NE.0) GO TO 2000
        LMSGC  = 0
        LNOTFY = 0
        IRET = IRET + 1
        RETURN
      ENDIF
C  
C Some job parameters are specified in the first job card.
C  
210   CONTINUE
      OUTPUT = '//'//JOBNAM//' JOB '
      LOUT   = 16
C  
C ..  JOB (,,,).... type job card.
C  
      IF(INPUT(IPNT:IPNT).EQ.'(') THEN
        IKET = INDEX(INPUT(IPNT:INLAST),')')
        ILJOBP = IPNT + IKET - 1
        IF(IKET.EQ.0) THEN
          PRINT *,'%Error BAT ... Not closed braket appears.'
          PRINT *,'Error line is,'
          PRINT *,INPUT
          IRET = -5
          RETURN
        ELSEIF(IPNT+1.EQ.IKET-1) THEN
          IF(WORK1(I:I).EQ.',') THEN
            IF(IUSER.EQ.1) THEN
              CALL PROMPT(2,'Enter password for JOB '//JOBNAM//' >>',
     >                      IRTN)
              READ(5,'(A8)') PSWD
              CALL CUPPER(PSWD)
            ENDIF
            OUTPUT(LOUT:) = '('//PSWD//')'
            LOUT   = LOUT + 10
          ENDIF
        ENDIF
        WORK1 = INPUT(IPNT+1:IPNT+IKET-2)
        LWORK1 = IKET - 2
        DO 230 I = 1, LWORK1 - 1
          IF(WORK1(I:I).NE.' ') GO TO 240
230     CONTINUE
240     CONTINUE
        IF(WORK1(I:I).EQ.',') THEN
          IF(IUSER.EQ.1) THEN
            CALL PROMPT(2,'Enter password for JOB '//JOBNAM//' >>',
     >                  IRTN)
            READ(5,'(A8)') PSWD
            CALL CUPPER(PSWD)
          ENDIF
          OUTPUT(LOUT:) = '('//PSWD//WORK1(I:LWORK1)//')'
          LOUT   = LOUT + LWORK1 - I + 11
        ELSE
          OUTPUT(LOUT:) = '('//WORK1(1:LWORK1)//')'
          LOUT   = LOUT + LWORK1 + 2
        ENDIF
C  
C  If no more JOB statement, output them.
C  
        IF(ILJOBP.EQ.INLAST) GO TO 205
        IPNT = ILJOBP + 2
C  
C If "," appears before "=" or neither "," nor "=" exists,
C password is not specified, i.e.
C                                                    (ICOMA,IEQUAL)
C    JOB  ,CLASS=S               no password            1  lt  7
C         CLASS=S,REGION=4096K   no password            8  gt  6
C         CLASS=S                no password            0  lt  6
C         password,CLASS=S          password exist.     9  lt 15
C         password                  password exist.     0  eq  0
C  
      ELSE
        ICOMA = INDEX(INPUT(IPNT:INLAST),',')
        IEQUAL= INDEX(INPUT(IPNT:INLAST),'=')
        IF(ICOMA.EQ.1.OR.
     >    (ICOMA.NE.0.AND.ICOMA.GT.IEQUAL).OR.
     >    (ICOMA.EQ.0.AND.IEQUAL.NE.0)) THEN
          IF(IUSER.EQ.1) THEN
            CALL PROMPT(2,'Enter password for JOB '//JOBNAM//' >>',
     >                    IRTN)
            READ(5,'(A8)') PSWD
            CALL CUPPER(PSWD)
          ENDIF
C         OUTPUT(LOUT:) = PSWD//','
C         LOUT   = LOUT + 9
          OUTPUT(LOUT:) = PSWD
          LOUT   = LOUT + 8
          IF(ICOMA.LT.IEQUAL) IPNT   = IPNT + ICOMA
        ENDIF
      ENDIF
C  
C Fill DEST parameter, if specified.
C  
      IDEST  = INDEX(INPUT(IPNT:INLAST),'DEST=')
      IF(IDEST.NE.0) THEN
        IP = IPNT + IDEST + 4
        IF(INPUT(IP:IP+5).EQ.'NOHOLD') THEN
          WORK1(IPNT+IDEST+10:INLAST)=INPUT(IPNT+IDEST+10:INLAST)
          INPUT(IPNT+IDEST-2:) = WORK1(IPNT+IDEST+10:INLAST)
          OUTPUT(LOUT:) = ',DEST=NOHOLD,'
          INLAST = INLAST - 12
          IDEST = 1
        ELSEIF(INPUT(IP:IP+3).EQ.'HOLD') THEN
          WORK1(IPNT+IDEST+8:INLAST)=INPUT(IPNT+IDEST+8:INLAST)
          INPUT(IPNT+IDEST-2:) = WORK1(IPNT+IDEST+8:INLAST)
          OUTPUT(LOUT:) = ',DEST=HOLD,'
          INLAST = INLAST - 10
          IDEST = 1
        ENDIF
      ELSE
          OUTPUT(LOUT:LOUT) = ','
      ENDIF
C     OUTPUT(63:) = '*BAT modified  3'
      CALL PUT(OUTDCB,OUTPUT,ILL,80)
      IF(ILL.NE.0) GO TO 2000
C  
C Treats another parameter.
C  
      LMSGC  = INDEX(INPUT(IPNT:INLAST),'MSGCLASS=')
      LNOTFY = INDEX(INPUT(IPNT:INLAST),'NOTIFY=')
      IF(LNOTFY.NE.0) THEN
         IF(INPUT(IPNT+LNOTFY+6:IPNT+LNOTFY+9).EQ.'XXXX') THEN
            INPUT(IPNT+LNOTFY+6:IPNT+LNOTFY+9) = USERID
         ENDIF
      ENDIF
      IF(ICONT.EQ.0) THEN
        IF(LMSGC.EQ.0) THEN
          OUTPUT = '//         MSGCLASS='//MSGC//','
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(ILL.NE.0) GO TO 2000
        ENDIF
        IF(LNOTFY.EQ.0) THEN
          OUTPUT = '//         NOTIFY='//USERID//','
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(ILL.NE.0) GO TO 2000
        ENDIF
        IF(INLAST.GT.IPNT) THEN
          OUTPUT = '//         '//INPUT(IPNT:INLAST)
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(ILL.NE.0) GO TO 2000
        ENDIF
        IRET = 0
        RETURN
      ELSE
        IF(INLAST.GT.IPNT) THEN
          OUTPUT = '//         '//INPUT(IPNT:INLAST)//','
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(ILL.NE.0) GO TO 2000
        ENDIF
        IRET = IRET + 1
        RETURN
      ENDIF
C  
C (3) Continuation Job card.
C  
500   CONTINUE
      IMSGC = INDEX(INPUT(3:INLAST),'MSGCLASS=')
      INOTFY= INDEX(INPUT(3:INLAST),'NOTIFY=')
      IF(INOTFY.NE.0) THEN
         IF(INPUT(IPNT+INOTFY+6:IPNT+INOTFY+9).EQ.'XXXX') THEN
            INPUT(IPNT+INOTFY+6:IPNT+INOTFY+9) = USERID
         ENDIF
      ENDIF
      IF(ICONT.EQ.0) THEN
        IF(LMSGC.EQ.0.AND.IMSGC.EQ.0) THEN
          OUTPUT = '//         '//'MSGCLASS='//MSGC//','
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(ILL.NE.0) GO TO 2000
        ENDIF
        IF(LNOTFY.EQ.0.AND.INOTFY.EQ.0) THEN
          OUTPUT = '//         '//'NOTIFY='//USERID//','
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(ILL.NE.0) GO TO 2000
        ENDIF
          CALL PUT(OUTDCB,OUTPUT,ILL,80)
          IF(ILL.NE.0) GO TO 2000
        IRET = 0
      ELSE
        IF(IMSGC .NE.0) LMSGC  = 1
        IF(INOTFY.NE.0) LNOTFY = 1
        CALL PUT(OUTDCB,OUTPUT,ILL,80)
        IF(ILL.NE.0) GO TO 2000
        IRET = IRET + 1
      ENDIF
      RETURN
C  
2000  CONTINUE
      PRINT *,'%Error BAT .. while output JCL card.'
      PRINT *,' return code =',ILL
      IRET = -9999
      RETURN
      END
