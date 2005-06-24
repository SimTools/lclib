C***********************************************************************
C*
C*================================----===
C* Subroutine SMREMC(LEVEL,IDEBUG,IRET)
C*================================----===
C*
C* (Purpose)
C*     Create Production:EMC;Hit_Cell by smearing 4-momenta in
C*     Generator:Particle_List.
C* (Inputs)
C*        LEVEL    : (I*4) :  level flag.
C*        IDEBUG   : (I*4) :  debug flag.
C* (Output)
C*        IRET     : (I*4) :  return flag.
C* (Relation)
C*     Called by SMREVT.
C*     Calls the following routines;
C* (Updaterecord)
C*     4/18/90  K.Fujii           Original version.
C*     5/22/90  K.Fujii           Modified to take shower size
C*                                into account.
C*     5/23/90  K.Fujii           Ignore EM-showering particles
C*                                if their creation points are
C*                                in side the calorimeter volume.
C*     6/07/90  K.Fujii           Bank name changed.
C*     6/20/90  K.Fujii           Fixed a bug in shower making for
C*                                endcap hits.
C*    10/11/90  K.Fujii           Changed output bank name and its
C*                                format.
C*     5/22/92  K.Fujii           Changed calorimetor segmentiation
C*                                scheme to keep the counter size
C*                                constant.
C*                                Added Si pad information to
C*                                hit cell bank.
C*     6/11/92  J.Kanzaki         Fixed several bugs in endcap
C*                                Si hit making.
C*     6/24/92  K.Fujii           Another bug fixed.
C*     3/27/93  K.Fujii           Another bug fixed.
C*                                   RT-->RF
C*                                   RT-->ZMAX
C*                                in block boundary calculations.
C*     3/29/93  K.Fujii           Modified shower shape routine.
C*
C***********************************************************************
 
      SUBROUTINE SMREMC(LEVEL,IDEBUG,IRET)
 
#include "smrgdt.inc"
#include "smrego.inc"
#include "smrrnd.inc"
#include "smrres.inc"
#include "unumcn.inc"
C--
C  MXxSEG is the number of phi/theta segments to which a shower
C  contributes.
C--
      PARAMETER      ( MXxNW = 400, MXxHSG = 2, MXxSEG = 2*MXxHSG+1,
     .                 MXxPD = 80,
     .                 MXxCEL = MXxSEG*MXxSEG, MXxCL = 10000,
     .                 MXxPHI = 3000, MXxTHT = 1000, MXxRAD = 500 )
      COMMON /SSWORK/ XP(3), RDATA(MXxNW,MXxCL), DXY(2), XY(2),
     .                EXY(-MXxHSG:MXxHSG,-MXxHSG:MXxHSG),
     .                IPDATA(3,MXxPD,MXxCL), NPDATA(MXxCL),
     .                IBPINT(0:MXxPHI,0:MXxTHT),
     .                IEPINT(0:MXxPHI,0:MXxRAD,2),
     .                NWRD(MXxCL), NELM(MXxCL)
      INTEGER*4       IDATA(MXxNW,MXxCL)
      EQUIVALENCE    (IDATA,RDATA)
      CHARACTER*64    BKNAM1 / 'Production:EMC;Hit_Cell' /
      CHARACTER*64    BKNAM2 / 'Production:EMC;Hit_Pad'  /
      PARAMETER       ( LNxEMC = 3 )
      DATA  EPSL      / 0.1 /
C
C .. to output particle info. at EMC entrance.
      REAL*4  EMPARTHIT(12) , kinv
      INTEGER*4  NEMPARTHIT     

C>>>
      DATA  ETHREM    / 5.E-2 /
C>>>
C>>>>
#ifdef TIME
      PARAMETER ( MXxTIM = 20 )
#endif 
#ifdef TIME
      COMMON /TIMER/ TIMER(MXxTIM)
#endif 
C>>>>
      DATA NCALL  /0/
      EXTERNAL        UCONST
C
C========< Entry Point >================================================
C
C--
C  Array check.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         IF ( MXxPHI.LT.NPHIEM .OR. MXxTHT.LT.NTHTEM .OR.
     .        MXxRAD.LT.NRADEM ) THEN
            PRINT *, '>>>>>>> Fatal Error in SMREMC >>>>>>>>'
            PRINT *, '   Pointer array too small. Stop.'
            STOP
         ENDIF
      ENDIF
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Zero cluster data.
C--
      NEMPARTHIT=0
      MEMCLS = 0
      NEMCLS = 0
      CALL UVZERO(MXxNW*MXxCL,RDATA)
      CALL UVZERO(MXxCL,NPDATA)
      CALL UVZERO((MXxPHI+1)*(MXxTHT+1),IBPINT)
      CALL UVZERO((MXxPHI+1)*(MXxRAD+1)*2,IEPINT)
C--
C  Loop over final state particles.
C--
      DO 40000 KPT = 1, NPRTCL
         IPT = IEPLST(KPT)
C--
C  Skip neutral particles other than gamma.
C--
         IPID = ABS(RPLIST( 2,IPT))
         ICHG = RPLIST( 4,IPT)
         IF ( ICHG.EQ.0 .AND. IPID.NE.22 )       GO TO 40000
C--
C  Check if decay point outside the calorimeter inner wall.
C  If not, skip this unstable particle.
C--
         NDOT = RPLIST(12,IPT)
         IF ( NDOT.GT.0 ) THEN
            IDT = RPLIST(13,IPT)
            XDK = RPLIST( 9,IDT)
            YDK = RPLIST(10,IDT)
            ZDK = RPLIST(11,IDT)
            RDK = SQRT(XDK*XDK+YDK*YDK)
            IF ( RDK.LE.RCYEM(2) .AND. ZDK.GE.ZCYEM(1) .AND.
     .           ZDK.LE.ZCYEM(2) )               GO TO 40000
         ENDIF
C--
C  Check if starting point inside the calorimeter inner wall.
C  If not, skip this particle.
C--
         X0  = RPLIST( 9,IPT)
         Y0  = RPLIST(10,IPT)
         Z0  = RPLIST(11,IPT)
         R0  = SQRT(X0*X0+Y0*Y0)
         IF ( R0.GT.RCYEM(2) .OR. Z0.LT.ZCYEM(1) .OR.
     .        Z0.GT.ZCYEM(2) )                   GO TO 40000
C--
C  Get intersection with EM calorimeter.
C--
         IF ( ICHG.NE.0 ) THEN
            CALL UHLXCY(RCYEM,ZCYEM,EPSL,RHELIX(1,IPT),PHI,XP,IRT)
         ELSE
            CALL ULINCY(RCYEM,ZCYEM,EPSL,RHELIX(1,IPT),PHI,XP,IRT)
         ENDIF
         IF ( IRT.LE.0 )                         GO TO 40000
C
C ... Generator Particle number
         NEMPARTHIT=NEMPARTHIT+1
         EMPARTHIT(1) = NEMPARTHIT
         EMPARTHIT(2) = IPT
         EMPARTHIT(3) = RPLIST( 2,IPT)   
         EMPARTHIT(4) = RPLIST( 4,IPT) 
C ... 5-8 = Px, Py,Pz, E
         if ( ichg.ne.0 ) then
            kinv =  1.0/RHELIX(3,IPT) 
            EMPARTHIT(5) = -kinv*sin(rhelix(2,ipt)+phi)
            EMPARTHIT(6) =  kinv*cos(rhelix(2,ipt)+phi)
            EMPARTHIT(7) =  kinv*rhelix(5,ipt)
         else 
            EMPARTHIT(5) = RPLIST(5,IPT)
            EMPARTHIT(6) = RPLIST(6,IPT)
            EMPARTHIT(7) = RPLIST(7,IPT)
         endif
         EMPARTHIT(8) = RPLIST( 8,IPT)
C ... 9-11 = X, Y, Z
         EMPARTHIT(9)  = XP(1)
         EMPARTHIT(10) = XP(2)
         EMPARTHIT(11) = XP(3)
         call tbput(1,'Production:EMC;Hit_Track',
     >                 NEMPARTHIT,11,EMPARTHIT,IRT)
C--
C  Update cluster data.
C        RDATA(1,    ICL ) = E
C        IDATA(2,    ICL ) = ICEL
C             (3,    ICL ) = # contributing particles
C             (4-->, ICL ) = gnerator particle #'s
C--
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
C--
C  Create cluster data for barrel part.
C--
         IF ( IRT.EQ.1 ) THEN
C--
C  Create Si pad hits.
C--
            PHIC = MOD(ATAN2(XP(2),XP(1))+x2PID,x2PID)
            IFIC = PHIC/DPHIEM
            RF   = RCYEM(2)
            RT   = SQRT(RF*RF+XP(3)*XP(3))
            CSTC = XP(3)/RT
            THTC = LOG( (1-CSTC)/(1+CSTC) )/2 - THMNEM
            ITHC = THTC/DTHTEM
C--
            DFPD = DPHIEM/NPADEM
            JFIC = (PHIC-DPHIEM*IFIC)/DFPD
            DTPD = DTHTEM/NPADEM
            JTHC = (THTC-DTHTEM*ITHC)/DTPD
C--
            KCEL = 1000*ITHC + IFIC
            IPAD = 1000*JTHC + JFIC
C--
C  Skip particles other than e or gamma.
C--
            IF ( IPID.NE.11 .AND. IPID.NE.22 )   GO TO 3500
C--
C  Calculate energy deposits.
C--
            ESUM = RPLIST(8,IPT)
            XY(1)  = RF*(PHIC-IFIC*DPHIEM)
            XY(2)  = RF*(THTC-ITHC*DTHTEM)
            DXY(1) = RF*DPHIEM
            DXY(2) = RF*DTHTEM
            CALL SMRSHA(1,MXxHSG,DXY,XY,EXY)
            DO 1000 KTH = -MXxHSG, +MXxHSG
               ITHT = KTH + ITHC
               IF ( ITHT.LT.0 .OR. ITHT.GE.NTHTEM ) GO TO 1000
               DO 100 KFI = -MXxHSG, +MXxHSG
                  IFI  = KFI + IFIC
                  IPHI = MOD(IFI+NPHIEM,NPHIEM)
                  E    = ESUM*EXY(KFI,KTH)
                  ICL  = IBPINT(IPHI,ITHT)
                  IF ( ICL.EQ.0 ) THEN
                     NEMCLS   = NEMCLS + 1
                     IF ( NEMCLS.GT.MXxCL ) THEN
                        PRINT *, ' >>>> SMREMC >>>> '
                        PRINT *, '    Too many cells in an event.'
                        PRINT *, '    Subsequent hits ignored. '
                        NEMCLS = NEMCLS - 1
                        IRET   = 1
                                                 GO TO 4500
                     ENDIF
                     IBPINT(IPHI,ITHT) = NEMCLS
                     RDATA( 1,NEMCLS)  = E
                     IDATA( 2,NEMCLS)  = 1000*ITHT + IPHI
                     IDATA( 3,NEMCLS)  = 1
                     IDATA(LNxEMC+1,NEMCLS) = IPT
                  ELSE
                     NPT = IDATA(3,ICL)
                     NPT = NPT + 1
                     IF ( NPT.GT.MXxNW-LNxEMC ) THEN
                        PRINT *, ' >>>> SMREMC >>>> '
                        PRINT *, '    Too many particles in a cell.'
                        PRINT *, '    STOP. '
                        STOP
                     ENDIF
                     RDATA( 1,ICL) = RDATA( 1,ICL) + E
                     IDATA( 3,ICL) = NPT
                     IDATA(LNxEMC+NPT,ICL) = IPT
                  ENDIF
100            CONTINUE
1000        CONTINUE
C--
C  Create cluster data for end cap part.
C--
         ELSE
C--
C  Create Si pad hits.
C--
            IZE  = 2*IRT - 5
            IF ( IZE.EQ.-1 ) THEN
               RDMNEM = RMMNEM
               DRADEM = DRDMEM
            ELSE
               RDMNEM = RPMNEM
               DRADEM = DRDPEM
            ENDIF
C--
            PHIC = MOD(ATAN2(XP(2),XP(1))+x2PID,x2PID)
            IFIC = PHIC/DPHIEM
            RF   = SQRT(XP(1)*XP(1)+XP(2)*XP(2))
            RT   = SQRT(RF*RF+XP(3)*XP(3))
            ZMAX = SQRT(XP(3)*XP(3))
            SNRC = RF/RT
            RADC = LOG( (1+SNRC)/(1-SNRC) )/2 - RDMNEM
            IRDC = RADC/DRADEM
C--
            DFPD = DPHIEM/NPADEM
            JFIC = (PHIC-DPHIEM*IFIC)/DFPD
C--
            DRPD = DRADEM/NPADEM
            JRDC = (RADC-DRADEM*IRDC)/DRPD
C--
            KCEL = IZE*( 1000000 + 1000*IRDC + IFIC )
            IPAD = 1000*JRDC + JFIC
C--
C  Skip particles other than e or gamma.
C--
            IF ( IPID.NE.11 .AND. IPID.NE.22 )   GO TO 3500
C--
C  Calculate energy deposits.
C--
            ESUM   = RPLIST(8,IPT)
            XY(1)  = RF*(PHIC-IFIC*DPHIEM)
            XY(2)  = ZMAX*(RADC-IRDC*DRADEM)
            DXY(1) = RF*DPHIEM
            DXY(2) = ZMAX*DRADEM
            CALL SMRSHA(1,MXxHSG,DXY,XY,EXY)
            DO 2000 KRD = -MXxHSG, +MXxHSG
               IRAD = KRD + IRDC
               IF ( IRAD.LT.0 .OR. IRAD.GE.NRADEM ) GO TO 2000
               DO 200 KFI = -MXxHSG, +MXxHSG
                  IFI  = KFI + IFIC
                  IPHI = MOD(IFI+NPHIEM,NPHIEM)
                  E    = ESUM*EXY(KFI,KRD)
                  ICL  = IEPINT(IPHI,IRAD,IRT-1)
                  IF ( ICL.EQ.0 ) THEN
                     NEMCLS   = NEMCLS + 1
                     IF ( NEMCLS.GT.MXxCL ) THEN
                        PRINT *, ' >>>> SMREMC >>>> '
                        PRINT *, '    Too many cells in an event.'
                        PRINT *, '    Subsequent hits ignored. '
                        NEMCLS = NEMCLS - 1
                        IRET   = 1
                                                 GO TO 4500
                     ENDIF
                     IEPINT(IPHI,IRAD,IRT-1) = NEMCLS
                     RDATA( 1,NEMCLS)  = E
                     IDATA( 2,NEMCLS)
     .                        = IZE*( 1000000 + 1000*IRAD + IPHI )
                     IDATA( 3,NEMCLS)  = 1
                     IDATA(LNxEMC+1,NEMCLS) = IPT
                  ELSE
                     NPT = IDATA( 3,ICL)
                     NPT = NPT + 1
                     IF ( NPT.GT.MXxNW-LNxEMC ) THEN
                        PRINT *, ' >>>> SMREMC >>>> '
                        PRINT *, '    Too many particles in a cell.'
                        PRINT *, '    STOP. '
                        STOP
                     ENDIF
                     RDATA( 1,ICL) = RDATA( 1,ICL) + E
                     IDATA( 3,ICL) = NPT
                     IDATA(LNxEMC+NPT,ICL) = IPT
                  ENDIF
200            CONTINUE
2000        CONTINUE
         ENDIF
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(11) = TIMER(11) + T2 - T1
#endif 
C>>>
C--
C  Update Si pad data.
C        IPDATA(1,i,ICL) = ICEL
C              (2,i,ICL) = 1000*ITH + IFI
C              (3,i,ICL) = particle #
C--
3500     CONTINUE
         IF ( MEMCLS.GT.0 ) THEN
            DO 4000 ICL = 1, MEMCLS
               NPT   = NPDATA(    ICL)
               ICELL = IPDATA(1,1,ICL)
               IF ( NPT.GT.0 .AND. KCEL.EQ.ICELL ) THEN
                  NPT = NPT + 1
                  IF ( NPT.GT.MXxPD ) THEN
                     PRINT *, ' >>>> SMREMC >>>> '
                     PRINT *, '    Too many particle in a single pad.'
                     PRINT *, '    Ignore subsequent particles.'
                                                 GO TO 40000
                  ENDIF
                  NPDATA(      ICL) = NPT
                  IPDATA(1,NPT,ICL) = KCEL
                  IPDATA(2,NPT,ICL) = IPAD
                  IPDATA(3,NPT,ICL) = IPT
                  NWRD  (      ICL) = 3*NPT
                                                 GO TO 40000
               ENDIF
4000        CONTINUE
         ENDIF
C--
         MEMCLS   = MEMCLS + 1
         IF ( MEMCLS.GT.MXxCL ) THEN
            PRINT *, ' >>>> SMREMC >>>> '
            PRINT *, '    Too many cells hit in a single event. '
            PRINT *, '    Subsequent hits ignored. '
            MEMCLS = MEMCLS - 1
            IRET   = 1
                                                 GO TO 4500
         ENDIF
         NPT                  = 1
         NPDATA(      MEMCLS) = NPT
         IPDATA(1,NPT,MEMCLS) = KCEL
         IPDATA(2,NPT,MEMCLS) = IPAD
         IPDATA(3,NPT,MEMCLS) = IPT
         NELM  (      MEMCLS) = MEMCLS
         NWRD  (      MEMCLS) = 3
40000 CONTINUE
C--
C  End of particle loop.
C--
4500  CONTINUE
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
C--
C  TBMPT 'Production:EMC;Hit_Pad'.
C        IBUFF(1,IP) = ICEL
C             (2,IP) = IPAD
C             (3,IP) = generator particle #
C--
      IF ( MEMCLS.GT.0 ) THEN
         CALL TBMPT(1,BKNAM2,MEMCLS,3*MXxPD,NELM,NWRD,IPDATA,IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, 'SMREMC failed to TBMPT ', BKNAM1
            PRINT *, '    IRT = ', IRT
            IRET = IRT
            RETURN
         ENDIF
      ENDIF
C--
C  Smear energy deposits.
C--
      NCL = 0
      DO 5000 ICL = 1, NEMCLS
C--
         E    = RDATA( 1,ICL)
         IF ( E.LT.0.1*ETHREM )                  GO TO 5000
C--
         ICEL = IDATA( 2,ICL)
         NPT  = IDATA( 3,ICL)
         IZE  = ICEL/1000000
C--
         IF ( IZE.EQ.0 ) THEN
            SIGE = SQRT(SGEMB0**2+(SGEMB/SQRT(E))**2)
            E    = E*( 1 + SIGE*RANN(ISEED) )
         ELSE
            SIGE = SQRT(SGEME0**2+(SGEME/SQRT(E))**2)
            E    = E*( 1 + SIGE*RANN(ISEED) )
         ENDIF
C--
C  Energy threshold.
C--
         IF ( E.LT.ETHREM )                      GO TO 5000
         NCL  = NCL + 1
         IDATA( 1,NCL) = E*1000
         IDATA( 2,NCL) = ICEL
         IDATA( 3,NCL) = NPT
         DO 500 JPT = 1, NPT
            IDATA(LNxEMC+JPT,NCL) = IDATA(LNxEMC+JPT,ICL)
500      CONTINUE
         NWRD (   NCL) = LNxEMC + NPT
         NELM (   NCL) = NCL
5000  CONTINUE
C--
C  TBPUT 'Production:EMC;Hit_Cell'.
C        IBUFF(   1) = E( MeV)
C             (   2) = ICEL
C             (   3) = # contributing particles
C             (4-->) = 1-st gnerator particle #
C--
      IF ( NCL.GT.0 ) THEN
         CALL TBMPT(1,BKNAM1,NCL,MXxNW,NELM,NWRD,IDATA,IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, 'SMREMC failed to TBMPT ', BKNAM1
            PRINT *, '    IRT = ', IRT
            IRET = IRT
            RETURN
         ENDIF
      ENDIF
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(12) = TIMER(12) + T2 - T1
#endif 
C>>>
C--
C  That's it.
C--
      RETURN
      END
