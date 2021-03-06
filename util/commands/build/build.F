C***********************************************************************
C* 
C*   BUILD program, version 3.
C* 
C*   A. Miyamoto   17-Oct-1985
C* 
C***********************************************************************
C  
      PARAMETER     (L_DSN=56, L_IN=137)
      CHARACTER*137  ARGMNT,WARG
      CHARACTER*100   WINDS,INDSN
CMSP       CHARACTER*100    NOUTDD
C  
      PARAMETER     (MXRMOV=40)
      CHARACTER*20   RMVSTR(MXRMOV),WRSTR(MXRMOV)
      INTEGER*4      LRMOVS(MXRMOV)
CKEK        CHARACTER*4    USERID
CNGY        CHARACTER*7    USERID
CMSP       character*8    USERID

      DATA           NOUNIT/6/
      DATA           NORMOV/0/
C  
C----------------------------------------------------------------------
C  Data for option.
C----------------------------------------------------------------------
C  
      PARAMETER     (MX_OPT=5)
      CHARACTER*8    OPTNAM(MX_OPT)
      INTEGER*4      LOPTNM(MX_OPT)
      DATA          (OPTNAM(I),LOPTNM(I),I=1,MX_OPT)
     > /' R(',      3,
     >  ' OUTDD(',   7,
     >  ' EXP(',    5,
     >  ' EEXP(',   6,
     >  ' PROMPT(',   8 /
      CHARACTER*16   EXPSTR, EEXPST
      CHARACTER*8    OUTDDN
      CHARACTER*1    PRPTST
      CHARACTER*4    WPRMP
      DATA   EXPSTR, LEXP  /'=EXPAND ',  8/
      DATA   EEXPST, LEEXP /'==EXPAND ', 9/
      DATA   OUTDDN/'SYSPRINT'/
      DATA   WPRMP/'%   '/
C  
      character*200 arg2nd
      integer*4     narg
C  
C-----< Entry Point >--------------------------------------------------
C  
      PRPTST = WPRMP(1:1)
C (1) Parse input argument.
C  
C 1.1  Get parameter
C  
CKEK        CALL GPARM(ARGMNT,LARG)
CNGY        CALL RDPRM(ARGMNT,LARG)
C
C ... For UNIX Sun OS
C
      narg = iargc()
      IARG1 = 1
      LARG  = 1

      if( narg.ge.1 ) then
        do 10 ia=1,narg
#if ! defined(HP)
           call getarg(ia,arg2nd)  ! for SUN unix
#else
           iflen = igetarg(ia, arg2nd, 200)  ! for HP unix
#endif
           lar2nd = lnblnk(arg2nd)
           ARGMNT(LARG:)=arg2nd(:lar2nd)//' '
           LARG = LARG+lar2nd+1
 10     continue
      else
        LARG=1
      endif
C   
      IARG1 = 1
      IARGL = LARG
      IF(LARG.GT.7) THEN
         IF(ARGMNT(3:7).EQ.'BUILD') THEN
           IARG1 = 8
           IARGL = LARG - 2
         ENDIF
      ENDIF
CBLD        IF(LARG.GT.7) THEN
CBLD          IF(ARGMNT(3:7).EQ.'BUILD') THEN
CBLD            IARG1 = 8
CBLD            IARGL = LARG - 2
CBLD          ENDIF
CBLD       ENDIF
CBAT       IF(LARG.GT.8) THEN
CBAT          IF(ARGMNT(3:8).EQ.'BLDBAT') THEN
CBAT            IARG1 = 9
CBAT            IARGL = LARG - 2
CBAT          ENDIF
CBAT       ENDIF
      IF(IARG1.GT.IARGL) GO TO 205
      NARG1 = IARG1
      CALL CUPPER( ARGMNT(IARG1:IARGL) )
C     PRINT *,' ARG=',ARGMNT(IARG1:IARGL),'==='
C-----------------------------------------------------------------------
C (2)   Get input data set name
C-----------------------------------------------------------------------
      DO 200 IABS = IARG1, IARGL
        IF(ARGMNT(IABS:IABS).NE.' ') GO TO 230
200   CONTINUE
205   CONTINUE
      CALL PROMPT(2,'Enter input dataset name >>',IRET)
      READ(5,'(A)') WINDS
      DO 210 LINDSN = L_DSN, 1, -1
        IF(WINDS(LINDSN:LINDSN).NE.' ') GO TO 500
210   CONTINUE
      PRINT *,'%Error BUILD.. Invalid input data set name'
      PRINT *,' input dsn =',WINDS
      STOP 210
C  
230   CONTINUE
      DO 240 I = IABS, IARGL
         IF(ARGMNT(I:I).EQ.' ') THEN
           WINDS  = ARGMNT(IABS:I-1)
           LINDSN = I - IABS
           IARG1  = I
           GO TO 300
         ENDIF
240   CONTINUE
      WINDS  = ARGMNT(IABS:IARGL)
      LINDSN = IARGL - IABS + 1
      GO TO 500
C-----------------------------------------------------------------------
C (3)   Search option string.
C-----------------------------------------------------------------------
C  
300   CONTINUE
C     PRINT *,'Going to parse option.'
C     PRINT *,' IARG1, IARGL=',IARG1, IARGL
C     PRINT *,' option =',ARGMNT(IARG1:IARGL),'>>>'
      IF(IARG1.GT.IARGL) GO TO 500
      DO 310 IOPT = 1, MX_OPT
        ICHK = INDEX(ARGMNT(IARG1:IARGL),OPTNAM(IOPT)(1:LOPTNM(IOPT)))
        IF(ICHK.EQ.0) GO TO 310
        IS   = IARG1 + ICHK + LOPTNM(IOPT) - 1
        IR   = INDEX(ARGMNT(IS:IARGL),')')
C       PRINT *,' IS,IR=',IS,IR
        IF(IR.EQ.0) THEN
          PRINT *,'%Error in BUILD, option does not enclosed ")"'
          PRINT *,' input is ',ARGMNT(IARG1:IARGL)
          STOP 300
        ENDIF
        LWARG = IR - 1
        IF(LWARG.LT.1) GO TO 310
        WARG  = ARGMNT(IS:IS+IR-2)
C       PRINT *,' LWARG=',LWARG
C       PRINT *,' WARG=',WARG(1:LWARG),'>>>'
C  
        GO TO (320, 340, 360, 380, 400), IOPT
C  
C (3.1) Remove option case.
C  
320     CONTINUE
        CALL CTOKEN(WARG(1:LWARG),NORMOV,WRSTR,LRMOVS)
        DO 330 I = 1, NORMOV
           RMVSTR(I) = 'C'//WRSTR(I)(1:LRMOVS(I))
           LRMOVS(I) = LRMOVS(I) + 2
330     CONTINUE
        GO TO 310
C  
C (3.2) UNIT option case.
C  
340     CONTINUE
        OUTDDN = WARG(1:LWARG)
        GO TO 310
C  
C (3.4) EXPAND string option.
C  
360     CONTINUE
        EXPSTR = WARG(1:LWARG)
        LEXP   = LWARG
        GO TO 310
C  
C (3.4) EEXPAND string option.
C  
380     CONTINUE
        EEXPST = WARG(1:LWARG)
        LEEXP   = LWARG
        GO TO 310
C  
C (3.5) EEXPAND string option.
C  
400     CONTINUE
        PRPTST = WARG(1:1)
   
310   CONTINUE
C  
C-----------------------------------------------------------------------
C (5)   Check existance of input data set
C-----------------------------------------------------------------------
C  
500   CONTINUE
C  
C 5.1  Constract fully qualified data set name.
C  
c      PRINT *,' LINDSN =',LINDSN
c      PRINT *,' WINDS =',WINDS(1:LINDSN),'>>>'
      IF(WINDS(1:1).EQ.'*') THEN
         INDSN  = WINDS(1:LINDSN)
c      ELSEIF(WINDS(1:1).NE.'''') THEN
c         CALL GETUID(USERID)
c         INDSN  = USERID//'.'//WINDS(1:LINDSN)
c         LINDSN = LINDSN + 8
      ELSE
c         INDSN  = WINDS(2:LINDSN-1)
c         LINDSN = LINDSN - 2
         INDSN  = WINDS(1:LINDSN)
         LINDSN = LINDSN 
      ENDIF
      PRINT *,' INDSN =',INDSN(1:LINDSN),'>>>'
C  
C-----------------------------------------------------------------------
C (7)   Call main BUILDer.
C-----------------------------------------------------------------------
C  
      CALL BUILDM(INDSN(1:LINDSN) , OUTDDN, NORMOV, RMVSTR, LRMOVS,
     >            EXPSTR(1:LEXP), EEXPST(1:LEEXP), PRPTST, IRET )
      IF(IRET.LT.0) STOP 902
      STOP
   
      END
