CC**********************************************************************
C*
C*===========================================----===
C*  Subroutine LUHADR( IDREC, LEVEL, IDEBUG, NRET )
C*===========================================----===
C*
C* (Function)
C*    LUND hadronizer.
C*    Hadronize particle in 'Spring:Particle_List' bank.
C* (Inputs)
C*    IDREC  ; record ID, use only ID=1
C*    LEVEL  ; not used.
C*    IDEBUG ; not used.
C* (Output)
C*    NRET  ; Return code.
C* (Update Record)
C*   91/08/27  K.Fujii		Derived from LUHADR by TAYM.
C*				Heavily modified.
C*   95/01/11  K.Fujii		Another big changes for implementation
C*				of tau polarization.
C*				ID=220000 is the LSP.
C*
CC**********************************************************************
 
      SUBROUTINE LUHADR( IDREC, LEVEL, IDEBUG, NRET )
 
      INTEGER*4       IDREC, LEVEL, IDEBUG, NRET
      COMMON /SSWORK/ IHEAD(20), NOELM(30), RBUF(20,30), RINLST(10,30),
     .                ROTLST(20, 400), KSTAT(30), RTMP(20),
     .                ISHUFL(30), ISHPR(2,30), ITMP(20),
     .                NSGRP(10), ISGRP(2,30,10), JSTAT(30),
     .                NPTLV(10), IPTLV(30,10), IDIDIT(30),
     .                NDKPTN(10), IDKPTN(30,10), INOSHW(30)
      COMMON /LUSEED/ NLSEED
      DATA  ILEVT/0/
C
C========< Entry Point >================================================
C
C--
C  Skip if non-event record.
C--
      NRET = 0
      IF ( IDREC.NE.1 ) RETURN
      ILSAVE = NLSEED
C--
C  Get 'Spring:Parton_List' into local common.
C--
C  The parton showering is handled level by level, since showers
C  from parent parton pairs might modify the momenta of their
C  daughter partons.
C  The level is defined by
C     RBUF(19,*) = ISLEV*100 + ISPAIR
C  where partons with the same ISLEV are passed to LUFRAG at one
C  time (that means they comprise a set of color singlet states and
C  that there is no parent-daughter relation among the set) and
C  partons with the same ISPAIR are paired in LUSHOW calls in
C  LUFRAG. 
C  Lepton pairs are required to have ISLEV = 0 and NDAU = 0 for
C  proper polarization handling when the pair contains a tau.
C  If you are satisfied with the default tau decay treatment, you
C  do not have to distinguish leptons from quarks in the decay
C  daughters from W or Z, etc.
C  Color singlet groups are distinguished by
C     RBUF(18,*) = ICF
C  where ICF must have different values for different color singlet
C  groups.
C  Helicities of the primary partons should be stored in
C     RBUF(17,*) = IHEL
C  when necessary as with tau leptons.
C--
      CALL TBNOEL(1, 'Spring:Parton_List', NPART, NOELM )
      IF ( NPART.GT.30 ) THEN
         PRINT *,'%Fatal Error in LUHADR...'
         PRINT *,'# of partons in ''SPring:Parton_list'' ',
     .           ' bank exceeds buffer size.'
         STOP
      ENDIF
      DO 100 IP = 1, NPART
         IE = NOELM(IP)
         CALL TBGET(1,'Spring:Parton_List',IE, NW, RBUF(1,IP), IRET )
100   CONTINUE
C--
C  Scan through Spring:Parton_List and classify them according to
C  required showing scheme. 
C--
C     NSLVL               = # levels of showering
C     NPTLV(ISLV)         = # partons in the level ISLV(=1,..,NSLVL)
C     IPTLV(IQ,ISLV)      = IP pointer to RBUF of IQ-th parton in
C			    the ISLV-th level (IQ=1,...,NPTLV(ISLV))
C     NSGRP(ISLV)         = # showering pairs in the ISLV-th level
C     ISGRP(1,IPSHW,ISLV) = IQ pointer of the 1st parton 
C          (2,IPSHW,ISLV) = IQ pointer of the 2nd parton 
C                           in IPSHW-th pair (IPSHW=1,..,NSGRP(ISLV))
C     NDKPTN(ISLV)	  = # unstable partons (NDAU>0) in the
C			    ISLV-th level.
C     IDKPTN(IMTH,ISLV)   = IQ pointer of IMTH-th unstable parton
C                           in the ISLV-th level
C--
C     NNOSHW       	  = # non-showering partons
C     INOSHW(IQ)	  = IP pointer of IQ-th non-showering 
C			    partons (IQ=1,..,NNOSHW)
C  Notice that non-showering partons might be affected by parton
C  showering, if they are daughters of some parton in a showering
C  pair: their momenta have to be readjusted when the parent
C  momentum is modified by the parton showering. 
C--
      NSLVL  = -999
      NNOSHW = 0
      CALL UVZERO( 10,NPTLV)
      CALL UVZERO(300,IPTLV)
      CALL UVZERO( 10,NSGRP)
      CALL UVZERO(600,ISGRP)
      CALL UVZERO( 30,NDKPTN)
C--
      DO 200 IP = 1, NPART
         NDAU  = RBUF(12,IP)
         IFSHW = RBUF(19,IP)
         ISLV  = IFSHW/100
         IPSHW = MOD(IFSHW,100)
         NSLVL = MAX(NSLVL,ISLV)
         IF ( ISLV.LE.0 ) THEN
            IF ( NDAU.LE.0 ) THEN
               NNOSHW = NNOSHW + 1
               INOSHW(NNOSHW) = IP
            ENDIF
         ELSE
            NPTLV(ISLV) = NPTLV(ISLV) + 1
            IPTLV(NPTLV(ISLV),ISLV) = IP
            IF ( IPSHW.GT.0 ) THEN
               IF ( NSGRP(ISLV).GT.0 .AND.
     .            ISGRP(1,IPSHW,ISLV).GT.0 ) THEN
                  ISGRP(2,IPSHW,ISLV) = NPTLV(ISLV)
               ELSE
                  NSGRP(ISLV) = NSGRP(ISLV) + 1
                  ISGRP(1,IPSHW,ISLV) = NPTLV(ISLV)
               ENDIF
               IF ( NDAU.GT.0 ) THEN
                  NDKPTN(ISLV) = NDKPTN(ISLV) + 1
                  IDKPTN(NDKPTN(ISLV),ISLV) = NPTLV(ISLV)
               ENDIF
            ENDIF
         ENDIF
200   CONTINUE
C--
C  Now start creating Generator:Particle_List bank.
C--
C  Data format of Generator:Particle_List bank.
C    Generator:Particle_List
C      Elm# n  RBUF( 1) = particle serial #
C                  ( 2) = Particle ID (PDG).
C                  ( 3) = mass (GeV)
C                  ( 4) = charge
C                  ( 5) = Px (GeV)
C                  ( 6) = Py (GeV)
C                  ( 7) = Pz (GeV)
C                  ( 8) = E  (GeV)
C                  ( 9) = X  (cm) of vertex point.
C                  (10) = Y  (cm)
C                  (11) = Z  (cm)
C                  (12) = # of daughter particle (=0 in this hadronizer)
C                  (13) = Pointer to the daughter (=0)
C                  (14) = Pointer to the mother (-n n=element# of
C                                               SPRING:Parton_List )
C                  (15) = trigger timing.
C                  (16) = particle life time (c*nsec)
C                  (18) = not used
C                  (19) = not used
C                  (20) = not used.
C--
C  Loop over levels of parton showers.
C--
C>>>
#ifdef DBGLU
        PRINT *, ' LUHADR starts Hadronization.'
#endif 
C>>>
      INELM = 0
      DO 7000 ISLV = 1, NSLVL
         IF ( NPTLV(ISLV).LE.0 )		GO TO 7000
         NIN   = 0
         CALL UVZERO(30,IDIDIT)
C--
C  Loop over partons in this level and store their information
C  in RINLST and invoke LUFRAG to carry out parton showering 
C  and fragmentation and decay.
C--
C     NIN           = # partons in RINLST
C     KSTAT(IIN)    = color flow flag
C     NSG           = NSGRP(ISLV) = # showering pairs in the
C		      current showering level
C     ISHPR(1,IS)   = IIN pointer of the 1st parton
C          (2,IS)   = IIN pointer of the 2nd parton
C		      in IS-th showering pair (IS=1,..,NSG)
C     RINLST(1,IIN) = ID
C           (2,IIN) = Px
C	    (3,IIN) = Py
C	    (4,IIN) = Pz
C	    (5,IIN) = E
C	    (6,IIN) = mass
C	    (7,IIN) = helicity
C--
C  Partons in RINLST must be orderd to make a planar tree in
C  terms of color flow. (e.g.) When this level consists of b, 
C  W+, bbar, and W- from a t-tbar pair, they must be ordered,
C  for instance, as b, bbar, W+, and W- and
C     KSTAT(1-4) = 1 0 0 0
C     JSTAT(1-4) = 0 0 2 2
C--
C     ISHUFL(IQ)    = Pointer to RINLST of IQ-th parton in the
C                     current showering level (IIN=1,..,NIN)
C--
         DO 300 IQ = 1, NPTLV(ISLV)
            IF ( IDIDIT(IQ).GT.0 )               GO TO 300
            IP   = IPTLV(IQ,ISLV)
            ID   = RBUF(2,IP)
            IDA  = IABS(ID)
            NDAU = RBUF(12,IP)
            NIN  = NIN + 1
            ICF  = RBUF(18, IP)
            CALL UVZERO( 10, RINLST(1,NIN) )
            RINLST(1,NIN)  = RBUF(2,IP)
            RINLST(2,NIN)  = RBUF(5,IP)
            RINLST(3,NIN)  = RBUF(6,IP)
            RINLST(4,NIN)  = RBUF(7,IP)
            RINLST(5,NIN)  = RBUF(8,IP)
            RINLST(6,NIN)  = RBUF(3,IP)
            RINLST(7,NIN)  = RBUF(17,IP)
            ISHUFL(IQ)     = NIN
            IDIDIT(IQ)     = 1
            KSTAT(NIN)     = 0
            JSTAT(NIN)     = NDAU
            IF ( IDA.GT.9 .AND. IDA.NE.21 )      GO TO 300
            KSTAT(NIN)     = 1
C--
C  Look for color singlet partner.
C--
            DO 30 JQ = IQ+1, NPTLV(ISLV)
               IF ( IDIDIT(JQ).GT.0 )            GO TO 30
               JP   = IPTLV(JQ,ISLV)
               JD   = RBUF(2,JP)
               JDA  = IABS(JD)
               NDAU = RBUF(12,JP)
               IF ( JDA.GT.9 .AND. JDA.NE.21 )   GO TO 30
               JCF  = RBUF(18,JP)
               IF ( JCF.NE.ICF )                 GO TO 30
               NIN  = NIN + 1
               CALL UVZERO( 10, RINLST(1,NIN))
               RINLST(1,NIN)  = RBUF(2,JP)
               RINLST(2,NIN)  = RBUF(5,JP)
               RINLST(3,NIN)  = RBUF(6,JP)
               RINLST(4,NIN)  = RBUF(7,JP)
               RINLST(5,NIN)  = RBUF(8,JP)
               RINLST(6,NIN)  = RBUF(3,JP)
               RINLST(7,NIN)  = RBUF(17,IP)
               ISHUFL(JQ)     = NIN
               IDIDIT(JQ)     = 1
               KSTAT(NIN)     = 1
               JSTAT(NIN)     = NDAU
30          CONTINUE
            KSTAT(NIN) = 0
300      CONTINUE
C--
C  Do parton shower and fragmentation.
C--
         NSG = NSGRP(ISLV)
         DO 400 IS = 1, NSG
            ISHPR(1,IS) = ISHUFL(ISGRP(1,IS,ISLV))
            ISHPR(2,IS) = ISHUFL(ISGRP(2,IS,ISLV))
400      CONTINUE
C>>>
#ifdef DBGLU
        PRINT *, ' LUHADR calls LUFRAG.'
#endif 
#ifdef DBGLU
        PRINT *, '   ISLV   = ', ISLV
#endif 
#ifdef DBGLU
        PRINT *, '   NIN    = ', NIN
#endif 
#ifdef DBGLU
        PRINT *, '   NSGRP  = ', NSGRP(ISLV)
#endif 
#ifdef DBGLU
        PRINT *, '   ISHPR  = ', ((ISHPR(L,M),L=1,2),M=1,NSG)
#endif 
#ifdef DBGLU
        PRINT *, '   KSTAT  = ', (KSTAT(M),M=1,NIN)
#endif 
#ifdef DBGLU
        PRINT *, '   JSTAT  = ', (JSTAT(M),M=1,NIN)
#endif 
#ifdef DBGLU
        DO 666 M = 1, NIN
#endif 
#ifdef DBGLU
           PRINT *, '   RINLST(*,',M,') = ', (RINLST(L,M),L=1,6)
#endif 
#ifdef DBGLU
666     CONTINUE
#endif 
C>>>
         CALL LUFRAG(NIN, RINLST, 400, NSG, ISHPR,
     .               KSTAT, JSTAT, NOUT, ROTLST, IRET )
C>>>
#ifdef DBGLU
        PRINT *, ' LUFRAG returns IRET = ', IRET, ' NOUT = ',NOUT
#endif 
#ifdef DBGLU
        PRINT *, '        modified RINLST as follows:'
#endif 
#ifdef DBGLU
        DO 667 M = 1, NIN
#endif 
#ifdef DBGLU
           PRINT *, '   RINLST(*,',M,') = ', (RINLST(L,M),L=1,6)
#endif 
#ifdef DBGLU
667     CONTINUE
#endif 
C>>>
         IF ( IRET .LT. 0 ) THEN
            NRET = -1
            RETURN
         ENDIF
C--
C  Put particles in this level to bank.
C--
         DO 600 I = 1, NOUT
            ROTLST( 1,I) = I + INELM
            ROTLST(13,I) = IFIX(ROTLST(13,I)+0.1) + INELM
            IF ( ROTLST(14,I).GT.0 ) THEN
               ROTLST(14,I) = IFIX(ROTLST(14,I)+0.1) + INELM
            ELSE
               DO 50 J = 1, NPTLV(ISLV)
                  IF ( JSTAT(J).EQ.0 )           GO TO 60
50             CONTINUE
60             ROTLST(14,I) = -IPTLV(J,ISLV)
            ENDIF
            IE = I + INELM
            CALL TBPUT ( 1, 'Generator:Particle_List',IE,
     .                       20, ROTLST(1,I), IRET )
600      CONTINUE
         INELM = INELM + NOUT
C--
C  Modify daughter momenta if parent momentum got modified.
C--
         DO 700 IMTH = 1, NDKPTN(ISLV)
            IQ = IDKPTN(IMTH,ISLV)
            IP = IPTLV(IQ,ISLV)
            IN = ISHUFL(IQ)
            NDAU = RBUF(12,IP)
            IDAU = RBUF(13,IP) - 1
            DO 70 JDAU = 1, NDAU
               IDAU = IDAU + 1
               CALL UBSTFD(RBUF(5,IDAU),RBUF(5,IP),RBUF(5,IDAU))
               CALL UBSTBK(RBUF(5,IDAU),RINLST(2,IN),RBUF(5,IDAU))
70          CONTINUE
700      CONTINUE
7000  CONTINUE
C--
C  Then output non-showering particles.
C  Notice that IDA = 220000 is the LSP(chi^0_1) which is
C  assumed to be stable.
C--
C>>>
#ifdef DBGLU
        PRINT *, ' LUHADR now treats non-showering partons.'
#endif 
C>>>
      DO 800 JP = 1, NNOSHW
         IP    = INOSHW(JP)
         ID    = RBUF(2,IP)
         IDA   = IABS(ID)
         IF ( IDA.GT.24 .AND. IDA.NE.220000 .AND.
     .        IDA.NE.1000022 ) THEN
            PRINT *,'%Warning... LUHADR:Particle_ID=',ID,
     .            ' not recognized.'
            GO TO 800
         ENDIF
C--
C  Neutrinos, e, mu, gamma, and LSP.
C--
         IF ( IDA.EQ.12 .OR. IDA.EQ.14 .OR. IDA.EQ.16 .OR.
     .        IDA.EQ.11 .OR. IDA.EQ.13 .OR. IDA.EQ.22 .OR.
     .        IDA.EQ.220000 .OR. IDA.EQ.1000022 ) THEN
            INELM = INELM + 1
            CALL UVZERO(20, RTMP )
            RTMP( 1) =  INELM
            RTMP( 2) =  ID
            RTMP( 3) =  RBUF(3, IP)
            RTMP( 4) = -MOD(ID,2)
            RTMP( 5) =  RBUF(5, IP)
            RTMP( 6) =  RBUF(6, IP)
            RTMP( 7) =  RBUF(7, IP)
            RTMP( 8) =  RBUF(8, IP)
            RTMP(14) = -IP
            CALL TBPUT( 1,'Generator:Particle_List',INELM, 20,RTMP,IRET)
C--
C  Tau, Z, and W.
C--
         ELSE IF ( IDA .EQ. 15 .OR. IDA.EQ.23 .OR. IDA.EQ.24) THEN
            CALL UVZERO( 10, RINLST )
            RINLST(1,1) = RBUF(2,IP)
            RINLST(2,1) = RBUF(5,IP)
            RINLST(3,1) = RBUF(6,IP)
            RINLST(4,1) = RBUF(7,IP)
            RINLST(5,1) = RBUF(8,IP)
            RINLST(6,1) = RBUF(3,IP)
            RINLST(7,1) = RBUF(17,IP)
            KSTAT(1)    = 0
            JSTAT(1)    = 0
            NIN = 1
            CALL LUFRAG(NIN, RINLST, 400, 0, ISGRP(1,1,1),
     .                  KSTAT, JSTAT, NOUT, ROTLST, IRET )
            IF ( IRET .LT. 0 ) THEN
               NRET = -1
               RETURN
            ENDIF
            DO 80  I = 1, NOUT
               ROTLST( 1,I) = I + INELM
               ROTLST(13,I) = IFIX(ROTLST(13,I)+0.1) + INELM
               IF ( ROTLST(14,I) .GT. 0. ) THEN
                  ROTLST(14,I) = IFIX(ROTLST(14,I)+0.1) + INELM
               ELSE
                  ROTLST(14,I) = -IP
               ENDIF
               IE = I + INELM
               CALL TBPUT ( 1, 'Generator:Particle_List',IE,
     .                          20, ROTLST(1,I), IRET )
80          CONTINUE
            INELM = INELM + NOUT
         ENDIF
800   CONTINUE
C--
C  Output Generator Header bank.
C--
      CALL TBGET( 1,'Spring:Header', 1, NW, ITMP, IRET )
      IEBEAM = ITMP(7)
      ILEVT = ILEVT + 1
      CALL UVZERO(10, ITMP )
      ITMP( 1) = ILEVT
      CALL UIDATE( ITMP( 2) )
      CALL UITIME( ITMP(3) )
      ITMP( 3) = 1
      ITMP( 5) = ILSAVE
      ITMP( 7) = IEBEAM
      CALL TBPUT(1,'Generator:Header', 1,10, ITMP, IRET )
C--
C  That's it.
C--
      RETURN
      END
