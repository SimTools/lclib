C**********************************************************************
C* 
C*  --------------------------------===============================
C*  Subroutine UMTLST( USER, MXxUMT, UMTVOL, UMTUNI, NPHLOG, NUMUSE )
C*  --------------------------------===============================
C* 
C*(Function)
C*  Get UMTLIST information into buffer.
C* 
C*(Input)
C*  USER   ; User ID (Char*4)
C*  MXxUMT ; Output buffer size.
C* 
C*(Output)
C*  UMTVOL(i) : Char*6, UMT Vol=Ser
C*  UMTUNI(i) : Char*4, UMT Unit(CTL3 or CTL6)
C*  NPHLOG(2,i)
C*        (1,i) : # of physical data sets.
C*        (2,i) : # of logical data sets.
C*  NUMUSE    : # of used UMTs.
C* 
C*(Note)
C*  This program uses, the file uid.@.UMT.UMTLIST for UMTLIST command
C*  output and Fortran unit # 29 to read this file.
C* 
C*(Author)
C*  A. Miyamoto     26-Sep-1988  Original version.
C* 
C**********************************************************************
C* 
      SUBROUTINE UMTLST( USER, MXxUMT, UMTVOL, UMTUNI, NPHLOG, NUMUSE)
C  
      CHARACTER     USER*4
      CHARACTER     UMTVOL(MXxUMT)*6, UMTUNI(MXxUMT)*4
      INTEGER*4     NPHLOG(2,MXxUMT)
C  
      CHARACTER     UMTLDS*80, WORK*160
      CHARACTER*8   IOSB(2)/'FT06F001','FT05F001'/
      DATA  IRU / 29/
C  
C =====< Entry Point >==================================================
C  
C ----------------------------------------------------------------------
C (1) Allocate temporaly file to output UMTLIST command output.
C ----------------------------------------------------------------------
C  
      NUMALC     = 0
C  
      CALL GETUID(UMTLDS)
      UMTLDS(5:) = '.@.UMT.UMTLIST'
      LDSN       = INDEX(UMTLDS,' ') - 1
      WORK       = ''''//UMTLDS(:LDSN)//''''
      CALL DSNCHK(WORK, IRET)
      IF(IRET.LT.8) THEN
        WORK  = 'DELETE '''//UMTLDS(:LDSN)//''''
        CALL IPFCMD(IRT1, IRT2, WORK, 160, IOSB )
        IF(IRT1.NE.0.OR.IRT2.NE.0) RETURN
      ENDIF
      WORK = 'ALLOC DD(UMTLIST) DS('''//UMTLDS(:LDSN)//
     >       ''') NEW CA SP(10 10) T REUSE USING(FB) '
      CALL IPFCMD(IRT1, IRT2, WORK, 160, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) RETURN
C  
      WORK = 'UMTLIST VOL U('//USER//')'
      CALL IPFCMD(IRT1, IRT2, WORK, 160, IOSB)
      IF(IRT1.NE.0.OR.IRT2.NE.0) RETURN
C  
      WORK = 'FREE DD(UMTLIST)'
      CALL IPFCMD(IRT1, IRT2, WORK, 160, IOSB)
C  
C ----------------------------------------------------------------------
C (2) Read ate temporaly file to output LISTC, and issue LISTC
C ----------------------------------------------------------------------
C  
      CALL UALCPS(IRU, UMTLDS, 'SHRR', 'FB', 10, IRET)
C  
      READ( IRU, '(A)' ) WORK(:80)
      READ( IRU, '(25X,I5)')  INUSE
      NUMUSE = 0
      DO 200 I = 1, MXxUMT
CC      READ( IRU, '(7X,A6,5X,A4,17X,I6,13X,I6)',END=300)
        READ( IRU, '(8X,A6,5X,A4,17X,I6,13X,I6)',END=300)
     >        UMTVOL(I), UMTUNI(I), NPHLOG(1,I), NPHLOG(2,I)
        NUMUSE = NUMUSE + 1
        IF( NUMUSE .GE. MXxUMT ) THEN
           NUMUSE = -INUSE
           GO TO 300
        ENDIF
200   CONTINUE
300   CONTINUE
      CLOSE( IRU )
      RETURN
      END
