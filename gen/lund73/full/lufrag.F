CC**********************************************************************
C*
C*==============================================================
C* Subroutine LUFRAG(NIN,INLIST,MXxOUT,NSPAR,ISPAR,KSTAT,JSTAT,
C*                   NOUT,OUTLST, NRET )
C*===================------------------=========================
C*
C* (Function)
C*    Parton-shower, decay, and hadronize partons with 
C*    LUND-PS and string fragmentation.
C* (Inputs)
C*    NIN          :(I*4): # of input partons.
C*    INLIST( 1,i) :(R*4): Particle ID (PDG format).
C*          ( 2,i) :(R*4): Px (GeV).
C*          ( 3,i) :(R*4): Py (GeV).
C*          ( 4,i) :(R*4): Pz (GeV).
C*          ( 5,i) :(R*4): E  (GeV).
C*          ( 6,i) :(R*4): m  (GeV).
C*          ( 7,i) :(R*4): helicity.
C*    MXxOUT       :(I*4): Size of OUTLST.
C*    NSPAR        :(I*4): # showering pairs.
C*    ISPAR ( 1,j) :(I*4): pointer to 1-st parton in j-th pair.
C*          ( 2,j) :(I*4): pointer to 2-nd parton in j-th pair.
C*    KSTAT (   i) :(I*4): color singlet group flag.
C*    JSTAT (   i) :(I*4): # daughters.
C* (Outputs)
C*    NOUT         :(I*4): # output particles.
C*    OUTLST( 1,i) :(R*4): 0.
C*          ( 2,i) :(R*4): Particle ID ( PDG format)
C*          ( 3,i) :(R*4): mass (GeV)
C*          ( 4,i) :(R*4): charge
C*          ( 5,i) :(R*4): Px
C*          ( 6,i) :(R*4): Py
C*          ( 7,i) :(R*4): Pz
C*          ( 8,i) :(R*4): E
C*          ( 9,i) :(R*4): 0.
C*          (10,i) :(R*4): 0.
C*          (11,i) :(R*4): 0.
C*          (12,i) :(R*4): # of daughter
C*          (13,i) :(R*4): Pointer to the daughter (relative)
C*          (14,i) :(R*4): Pointer to the mother (relative)
C*          (15,i) :(R*4): 0.
C*          (16,i) :(R*4): life time  (c*t (cm))
C*          (17,i) :(R*4): 0.
C*          (18,i) :(R*4): 0.
C*          (19,i) :(R*4): 0.
C*          (20,i) :(R*4): 0.
C*    NRET         :(I*4): return flag.
C* (Update Records)
C*    95/02/10  K.Fujii		New version for JETSET7.3.
C*
CC**********************************************************************
 
      SUBROUTINE LUFRAG( NIN,INLIST,MXxOUT,NSPAR,ISPAR,KSTAT,JSTAT,
     .                   NOUT, OUTLST, NRET )
 
      INTEGER * 4  NIN, MXxOUT, NSPAR, ISPAR(2,*), KSTAT(*), JSTAT(*),
     .             NOUT, NRET
      REAL    * 4  INLIST( 10, NIN ), OUTLST( 20, MXxOUT )
C--
C  LUND commons.
C--
      COMMON /LUJETS/  N, K ( 4000, 5 ), P ( 4000, 5 ), V( 4000, 5 )
      INTEGER*4        N, K
      REAL   *4        P, V
      COMMON /LUDAT1/  MSTU( 200 ), PARU( 200 ), 
     .                 MSTJ( 200 ), PARJ( 200 )
      INTEGER*4        MSTU, MSTJ
      REAL   *4        PARU, PARJ
C--
C  Constants
C--
      EXTERNAL         LUDATA
C--
C  Pointer from /LUJETS/ to OUTLST.
C     IDHIST ( i ) : Address in OUTLST for particles in /LUJETS/
C--
      INTEGER*4    IDHIST( 4000 )
      INTEGER*4    NJOIN, IJOIN( 4000 )
C
C ======< Entry Point >=============================================
C
C--
C  Reset #particles in /LUJETS/.
C--
      N     = 0
C--
C  Check NSPAR to decide whether parton shower is required or not.
C--
      IF ( NSPAR.LE.0 ) THEN
	 KF  = INT( INLIST(1,1) )
	 KFA = IABS(KF) 
	 IF ( NIN.EQ.1 .AND. KFA.EQ.15 ) THEN
C--
C  Decay taus using TAUOLA.
C  Notice that TAUOLA initializatin is carried out in FLNPAR
C  and TAUOLA summary is printed out in FLUERU.
C  TAUOLA parameters should be read in by FLNPAR from FT05.
C--
	    IF ( KF.LT.0 ) THEN
	       KTO = 1
	    ELSE
	       KTO = 2
	    ENDIF
	    CALL LUTUHL(2,IHLON)
            POL = INLIST(7,1)*IHLON
            CALL TAUDEC(KTO,1,POL,INLIST(2,1))
            CALL LUHEPC(2)
       	 ELSE	 
C--
C  If input particles do not require parton shower generations,
C  fragment them according to the standard procedure.
C--
            DO 100 I  = 1, NIN
               N   = N + 1
	       KF  = INT( INLIST(1,I) )
	       KFA = IABS(KF)
	       IF ( KFA.EQ.220000 ) KF = ISIGN(33,KF)
C--
               K(N,1) = KSTAT(I) + 1
               K(N,2) = KF
               K(N,3) = 0
               K(N,4) = 0
               K(N,5) = 0
               P(N,1) = INLIST( 2, I )
               P(N,2) = INLIST( 3, I )
               P(N,3) = INLIST( 4, I )
               P(N,4) = INLIST( 5, I )
               P(N,5) = INLIST( 6, I )
               V(N,1) = 0
               V(N,2) = 0
               V(N,3) = 0
               V(N,4) = 0
               V(N,5) = 0
100         CONTINUE
	 ENDIF
      ELSE
C--
C  If input particles require parton shower generations,
C  Do PARTON SHOWER first.
C--
         NJOIN = 0
         DO 200 I = 1, NIN
            N      = N + 1
	    KF     = INT( INLIST(1,I) )
	    KFA    = IABS(KF)
	    IF ( KFA.EQ.220000 ) KF = ISIGN(33,KF)
C--
            IF ( KSTAT(I).NE.0 ) THEN
               NJOIN = NJOIN + 1
               IJOIN(NJOIN) = I
            ELSE IF ( NJOIN.GT.0 ) THEN
               IF ( KSTAT(I-1).NE.0 ) THEN
                  NJOIN = NJOIN + 1
                  IJOIN(NJOIN) = I
               ENDIF
            ENDIF
C--
            K(N,1) = KSTAT(I) + 1
            K(N,2) = KF
            K(N,3) = 0
            K(N,4) = 0
            K(N,5) = 0
            P(N,1) = INLIST( 2, I )
            P(N,2) = INLIST( 3, I )
            P(N,3) = INLIST( 4, I )
            P(N,4) = INLIST( 5, I )
            P(N,5) = INLIST( 6, I )
            V(N,1) = 0
            V(N,2) = 0
            V(N,3) = 0
            V(N,4) = 0
            V(N,5) = 0
200      CONTINUE
C--
C  Do LUJOIN.
C--
         CALL LUJOIN(NJOIN,IJOIN)
C>>>
#ifdef DBGLU
       PRINT *,' Before LUSHOW '
#endif 
#ifdef DBGLU
       CALL LULIST(2)
#endif 
C>>>
C--
C  Do LUSHOW for each showering pair.
C--
         DO 300 IS = 1, NSPAR
            IP1 = ISPAR(1,IS)
            IP2 = ISPAR(2,IS)            
            QMX = (P(IP1,4)+P(IP2,4))**2 - (P(IP1,1)+P(IP2,1))**2
     .          - (P(IP1,2)+P(IP2,2))**2 - (P(IP1,3)+P(IP2,3))**2
            QMX = SQRT(QMX) - P(IP1,5) - P(IP2,5)
            QMX = MAX(QMX,1.)
C>>>
#ifdef DBGLU
       PRINT *,' IS IP1 IP2 QMX = ', IS, IP1, IP2, QMX
#endif 
C>>>
            CALL LUSHOW(IP1,IP2, QMX )
300      CONTINUE
C>>>
#ifdef DBGLU
       PRINT *,' After LUSHOW '
#endif 
#ifdef DBGLU
       CALL LULIST(2)
#endif 
C>>>
C--
C  Store modified momenta for unstable particles.
C  When one such particle has a daughter of the same flavor,
C  take it as the one after gluon emission and test if further
C  gluon emission occured or not. If not, consider the last one
C  as the parent parton after gluon radiations.
C-- 
C    MDF = the line# of the 1-st daughter
C    MDL = the line# of the last daughter
C--
         DO 400 I = 1, NIN
            IF ( JSTAT(I).GT.0 ) THEN
	       KF = K(I,2) 
	       MP = I
35	       MDF = K(MP,4)
	       MDL = K(MP,5)
	       IF ( MDF*MDL.NE.0 ) THEN
	          DO 40 M = MDF, MDL
	             IF ( K(M,2).EQ.KF ) THEN
	                MP = M
	               	IF( K(M,1).GT.3 )	GO TO 35
	             ENDIF
40	          CONTINUE
	       ENDIF
               INLIST( 2, I ) = P(MP,1)
               INLIST( 3, I ) = P(MP,2)
               INLIST( 4, I ) = P(MP,3)
               INLIST( 5, I ) = P(MP,4)
            ENDIF
400      CONTINUE
C>>>
#ifdef DBGLU
       PRINT *,' Before LUPREP '
#endif 
C>>>
         CALL LUPREP(1)
C>>>
#ifdef DBGLU
       PRINT *,' After LUPREP '
#endif 
#ifdef DBGLU
       CALL LULIST(2)
#endif 
C>>>
      ENDIF
C--
C  Do LUEXEC for fragmentation.
C--
      CALL LUEXEC
C>>>
#ifdef DBGLU
       PRINT *,' After LUEXEC '
#endif 
#ifdef DBGLU
       CALL LULIST(2)
#endif 
C>>>
      IF ( MSTU(23).NE.0 .OR. MSTU(24).NE.0 .OR. MSTU(28).NE.0 ) THEN
         PRINT *,'%Warning..LUFRAG  Possible error in LUEXEC detecded.'
         PRINT *,'          This event will be skipped.'
         MSTU(23) = 0
         MSTU(24) = 0
         MSTU(28) = 0
         NRET = -1
         RETURN
      ENDIF
C--
C  Output particles after fragmentations and decays.
C--
      NOUT = 0
      DO 7000 I = 1, N
         KS  = K ( I, 1 )
         KF  = K ( I, 2 )
         KH  = K ( I, 3 )
         KFA = IABS( KF )
         IDHIST(I) = 0
C--
C  Skip quarks and gluons.
C--
         IF ( ( KFA.GE.1 .AND. KFA.LE.10 ) .OR.
     .          KFA.EQ.21 )			GO TO 7000
C--
C  Skip fundamental bosons except for photons.
C--
         IF ( KFA.GE.23 .AND. KFA.LE.100 )    	GO TO 7000
C--
C  Now store this particle in OUTLST.
C--
         NOUT = NOUT + 1
         IF ( NOUT .GT. MXxOUT ) THEN
            PRINT *, '%LUFRAG-F, Output buffer overflowed. Truncated.'
            PRINT *, '           NOUT = ', NOUT
            CALL LULIST(2)
            NOUT = MXxOUT
                                                 GO TO 900
         ENDIF
         CALL UVZERO( 20, OUTLST(1,NOUT))
         IDHIST(I)      = NOUT
C--
	 IF ( KFA.EQ.33 ) THEN
	    CHRG  = 0.
	    KF    = ISIGN(220000,KF)
	    XCTAU = 0.
	 ELSE
	    CHRG  = LUCHGE( KF ) / 3.0
	    XCTAU = ULCTAU( KF )
	 ENDIF
C--         
         OUTLST(2,NOUT) = KF
         OUTLST(3,NOUT) = P( I, 5 )
         OUTLST(4,NOUT) = CHRG
         OUTLST(5,NOUT) = P( I, 1 )
         OUTLST(6,NOUT) = P( I, 2 )
         OUTLST(7,NOUT) = P( I, 3 )
         OUTLST(8,NOUT) = P( I, 4 )
         OUTLST(12,NOUT) = 0.
C--
         IF ( KH.EQ.0 ) THEN
            OUTLST(14,NOUT) = 0
         ELSE
            IPAR = IDHIST(KH)
            IF ( IPAR.GT.0 ) THEN
               OUTLST(12,IPAR) = OUTLST(12,IPAR) + 1.
               NDAU            = OUTLST(12,IPAR) + 0.1
               IF ( NDAU.EQ.1 )  OUTLST(13,IPAR) = NOUT
               OUTLST(14,NOUT) = IPAR
            ENDIF
         ENDIF
         OUTLST(16,NOUT) = XCTAU
7000  CONTINUE
C--
C  That's it.
C--
900   RETURN
      END
