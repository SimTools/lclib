C*****************************************************
C*
C*  Subroutine TBFOPN( DISKFL )
C*
C*(Function)
C*  Open FACOM's disk file for subsequent read/write.
C*(Input)
C*  DISKFL .. FACOM's DISK file name.
C*
C*(Author)
C*  A. Miyamoto  31-May-1994  Original version.
C*
C*****************************************************
 
      SUBROUTINE TBFOPN( RHOST, DISKFL, NRET )
 
 
      CHARACTER*(*) RHOST, DISKFL
      CHARACTER*8       JOBNAME
      CHARACTER*32      HOST
      CHARACTER*160     OPER
      CHARACTER*1       ACT(9),NULL
      CHARACTER*9       ACTION
      EQUIVALENCE(ACTION,ACT(1))
      DATA NULL /X'00'/
 
c234567890
C      INTEGER*4         UNIT,RC,RC1,RC2,TIME
C      CHARACTER*32760   BUFF
C      PARAMETER        (BUFSIZ=100000)
C      INTEGER*4         IBUF(BUFSIZ)
 
C
C      EQUIVALENCE(BUFF, BUFX(1))
C
 
C ***************************************************************** *
C     CALL MTLINT
C ***************************************************************** *
      JOBNAME='        '
      HOST = RHOST
      ITIME=0
      CALL MTLINT( HOST//CHAR(0), ITIME, JOBNAME//CHAR(0), IRC )
C
      IF( IRC .NE. 0 ) THEN
        PRINT *,'In TBFOPN..Error in MTLINT ... NRET =',IRC
        STOP
      ENDIF
C
      PRINT *,'TBFOPN.. Pass MTLINT... JOBNAME =',JOBNAME,
     >   ' Host=',HOST
C
C ************************************************************** *
C     CALL MTLATR
C ************************************************************** *
C
      IRC =0
      IRC1=0
      IRC2=0
 
      OPER='IN INPUT LRECL(23472) BLKSIZE(23476) RECFM(V,B,S) '//
     >       'BUFNO(20) OPTCD(C)'
CC    print *,' oper=',oper
      CALL MTLATR( OPER//NULL ,IRC,IRC1,IRC2 )
C
CC    WRITE(6,*) 'In TBFOPN..MTLATR return code =',IRC
      IF (IRC.NE.0) THEN
          WRITE(6,*) 'Attribute.'
          WRITE(6,*) 'IPFCMD RC1=',RC1
          WRITE(6,*) 'IPFCMD RC2=',RC2
          GO TO 9000
      END IF
C ************************************************************** *
C     CALL MTLALC
C ************************************************************** *
C
      IUNIT = 10
 
      WRITE(OPER,210) DISKFL(:LNBLNK(DISKFL))
 210  FORMAT('DS(',A,') SHR REUSE USING(IN)')
CC    PRINT *,'ALLOC ..',OPER
      CALL MTLALC( IUNIT,OPER//NULL,IRC,IRC1,IRC2 )
C
      WRITE(6,*) 'MTLALC return code =',IRC
      IF (IRC.NE.0) THEN
          WRITE(6,*) 'Alloc error.'
          WRITE(6,*) 'IPFCMD IRC1=',IRC1
          WRITE(6,*) 'IPFCMD IRC2=',IRC2
          GO TO 9000
      END IF
C ************************************************************** *
C     CALL MTLOPN
C ************************************************************** *
      IUNIT=10
      ACTION = 'READ'//CHAR(0)
C      ACT(5) = NULL
C
      CALL MTLOPN( IUNIT, ACTION, IRC )
C
      WRITE(6,*) 'In TBFOPN .. MTLOPN return code =',IRC
 
      NRET = 0
 
      RETURN
 
 
C ************************************************************** *
C     CALL MTLEND
C ************************************************************** *
9000  CONTINUE
      CALL MTLEND( IRC )
      WRITE(6,*) 'In TBFOPN.. MTLEND return code =',IRC
      NRET = -1
C
      RETURN
      END
