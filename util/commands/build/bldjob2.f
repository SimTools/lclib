C***********************************************************************
C* 
C*  ----------------------------------=======
C*  Subroutine Bldjob( INPUT, NOUNIT, IRET , JOBNAM)
C*  ----------------------------------=======
C* 
C*(Function)
C*   Complete the Job card supplied from the input data.
C*   Main tasks are to construct job name, and fill the password
C*   and the message class.
C* 
C*(Input)
C*   INPUT  : Input JCL card.
C*   NOUNIT : Output unit number.
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
C* 
C*(Update Record)
C*   This version for NAGOYA FACOM M200 system.
C* 
C***********************************************************************
C  
      SUBROUTINE BLDJOB( INPUT, NOUNIT, IRET, JOBNAM)
C  
      CHARACTER*80   INPUT, OUTPUT, WORK, WORK2
      CHARACTER      USERID*7, JOBNAM*8
      CHARACTER*20   OUTFMT
      DATA           OUTFMT/'(A80)'/
      DATA           LOCHR/80/
C  
C  (1) First call to this routine. Check job name.
C  
C     PRINT *,' INPUT IS='
C     PRINT *,INPUT
      IF(NOUNIT.EQ.6) THEN
        OUTFMT = '(1X,A79)'
        LOCHR  = 79
      ENDIF
      IRET = -1
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
C (1.3) Is there JOB word ?
C  
      IJOB = INDEX(INPUT(1:72),' JOB ')
      IF(IJOB.EQ.0) THEN
        PRINT *,'%Error BAT .. No Job statement in first JCL Card.'
        PRINT *,' Error line is,'
        PRINT *,INPUT
        IRET = -4
        RETURN
      ENDIF
      CALL GETUID(USERID)
      JOBNAM = INPUT(3:IJOB-1)
      OUTPUT = '//'//INPUT(3:IJOB-1)//' JOB '//USERID//',########,'
      LOUT   = 22 + IJOB
C     PRINT *,'OUTPUT =',OUTPUT(1:LOUT),'>>>'
C  
C (2) Search ,, in job parameter.
C  
      ICOMA2 = INDEX(INPUT(IJOB+5:72),',,')
      IF(ICOMA2.EQ.0) THEN
        PRINT *,'%Error BAT .. no ",," string in first job card.'
        PRINT *,'Input is'
        PRINT *,INPUT
        IRET = -3
        RETURN
      ENDIF
      ICOMA2 = ICOMA2 + IJOB + 6
      WORK   = INPUT(ICOMA2:72)
      DO 120 LWORK = 73 - ICOMA2, 1, -1
        IF(WORK(LWORK:LWORK).NE.' ') GO TO 130
120   CONTINUE
        PRINT *,'%Error BAT .. no CLASS specified in job card.'
        PRINT *,'Input is'
        PRINT *,INPUT
        IRET = -4
        RETURN
130   CONTINUE
      ICONT = 0
      IF(WORK(LWORK:LWORK).EQ.',') THEN
        ICONT = 1
        LWORK = LWORK - 1
      ENDIF
C     PRINT *,' WORK =',WORK(:LWORK),'>>>'
C  
      ICLASS = INDEX(WORK(:LWORK),'CLASS=')
      IF(ICLASS.EQ.0) THEN
        PRINT *,'%Error BAT .. no CLASS specified in job card.'
        PRINT *,'Input is'
        PRINT *,INPUT
        IRET = -4
        RETURN
      ENDIF
      OUTPUT(LOUT:) = WORK(ICLASS:ICLASS+6)
      LOUT   = LOUT + 7
      LWORK2 = LWORK - 7
C     PRINT *,' LOUT=',LOUT
C     PRINT *,' OUTPUT=',OUTPUT(:LOUT),'>>>'
C     PRINT *,' LWORK2 = ',LWORK2
      IF(LWORK2.GT.1) THEN
        IF(ICLASS.EQ.1) THEN
          WORK2 = WORK(9:LWORK)
          LWORK2 = LWORK - 8
        ELSEIF(ICLASS+8.GT.LWORK) THEN
          WORK2 = WORK(:ICLASS-2)
          LWORK2 = ICLASS - 2
        ELSE
          WORK2  =  WORK(:ICLASS-2)//','//WORK(ICLASS+8:LWORK)
          LWORK2 =  LWORK - 8
        ENDIF
        WORK = WORK2(:LWORK2)
      ENDIF
C     PRINT *,' LWORK2 = ',LWORK2
C     IF(LWORK2.GT.0) PRINT *,' WORK =',WORK(:LWORK2),'>>>'
C  
C ... Search additional parameter after CLASS parameter.
C  
      DO 300 I = 1, LWORK2
        IF(WORK(I:I).NE.' ') THEN
          OUTPUT(LOUT:LOUT) = ','
          WRITE(NOUNIT,OUTFMT) OUTPUT(:LOCHR)
          OUTPUT = '// '//WORK(I:LWORK2)
          IF(ICONT.EQ.1) OUTPUT = '// '//WORK(I:LWORK2)//','
          WRITE(NOUNIT,OUTFMT) OUTPUT(:LOCHR)
          GO TO 310
        ENDIF
300   CONTINUE
      IF(LWORK2.LT.1) THEN
        IF(ICONT.EQ.1) OUTPUT(LOUT:LOUT) = ','
      ENDIF
      WRITE(NOUNIT,OUTFMT) OUTPUT(:LOCHR)
310   IRET = 0
      RETURN
      END
