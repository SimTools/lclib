C***********************************************************************
C*
C*===================----===
C* Subroutine CMBTRK(IRET)
C*===================----===
C*
C* (Function)
C*     Create combined track banks.
C* (Input)
C*     IDREC   :  Record ID.
C*     ILEVEL  :  Level of analysis.
C*     IDEBUG  :  Debug flag.
C* (Output)
C*     IRET    :  Return flag.
C* (Update Record)
C*   93/05/10  K.Fujii            Original version.
C*
C***********************************************************************
 
      SUBROUTINE CMBTRK( IRET )
 
      DATA NCALL /0/
C
C==================<<  Entry Point  >>================================
C
#ifdef HIUXF77
      CALL CMBBLK
      CALL UCONST
#endif
C--
C  Reset return code.
C--
      IRET = 1
C--
C  Initialization.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         CALL INICLS(IRT)
      ENDIF
C--
C  TBGET CDC tracks.
C--
      CALL GTCDTK(IRT)
      IF ( IRT.LT.0 )                            GO TO 9999
C--
C  Flag out lepton tracks.
C--
      CALL RMVLPT(IRT)
      IF ( IRT.LT.0 )                            GO TO 9999
C--
C  Output gamma tracks.
C--
      CALL GAMTRK(IRT)
      IF ( IRT.LT.0 )                            GO TO 9999
C--
C  Make 2-dim. histogram of hadron clusters.
C--
      CALL HSTCEL(IRT)
      IF ( IRT.LT.0 )                            GO TO 9999
C--
C  Print out generated clusters.
C--
C>>>
#ifdef P
      CALL PRTCEL
#endif 
C>>>
C--
C  Start cluster track matching.
C--
      NTRAK = 0
C>>>
#ifdef P
      CALL PRTCLS(1)
#endif 
C>>>
1     CALL FGLCLS(IRT)
C>>>
#ifdef P
      IF( IRT.EQ.0 ) CALL PRTCLS(2)
#endif 
C>>>
      IF ( IRT.EQ.0 ) THEN
         CALL HADTRK(NTRAK)
                                                 GO TO 1
      ENDIF
C--
C  Output lepton or unmatched tracks.
C--
      CALL LPTTRK(NTRAK,IRT)
      IF ( IRT.LT.0 )                            GO TO 9999
C--
C  Print out reconstructed clusters.
C--
C>>>
#ifdef P
      CALL PRTCLS(3)
#endif 
#ifdef P
      GO TO 9999
#endif 
C>>>
C--
C  That's it.
C--
      IRET = 0
9999  RETURN
      END
