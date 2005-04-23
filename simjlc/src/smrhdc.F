C***********************************************************************
C*
C*================================----===
C* Subroutine SMRHDC(LEVEL,IDEBUG,IRET)
C*================================----===
C*
C* (Purpose)
C*     Create Production:HDC;Hit_Cell by smearing 4-momenta in
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
C*     5/23/90  K.Fujii           Do not ignore unstable particles
C*                                if they decay after entering the
C*                                calorimeter. Ignore daughter
C*                                particles, even if stable, when
C*                                they decay after their parents
C*                                entering the calorimeter.
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
C*     6/24/92  K.Fujii           Another bug fixed.
C*     7/08/92  K.Fujii           The SMRSHW ityp has been one!!!
C*                                Now fixed to 2.
C*     3/27/93  K.Fujii           Another bug fixed.
C*                                   RT-->RF
C*                                   RT-->ZMAX
C*                                in block boundary calculations.
C*     3/29/93  K.Fujii           Modified shower shape routine.
C*
C***********************************************************************
 
      SUBROUTINE SMRHDC(LEVEL,IDEBUG,IRET)
 
#include "smrgdt.inc"
#include "smrhgo.inc"
#include "smrrnd.inc"
#include "smrres.inc"
#include "unumcn.inc"
C--
C  MXxSEG is the number of phi/theta segments to which a shower
C  contributes.
C--
      PARAMETER      ( MXxNW = 100, MXxHSG = 3, MXxSEG = 2*MXxHSG+1,
     .                 MXxCEL = MXxSEG*MXxSEG, MXxCL = 5000,
     .                 MXxPHI = 200, MXxTHT = 100, MXxRAD = 50 )
      COMMON /SSWORK/ XP(3), RDATA(MXxNW,MXxCL), DXY(2), XY(2),
     .                EXY(-MXxHSG:MXxHSG,-MXxHSG:MXxHSG),
     .                IBPINT(0:MXxPHI,0:MXxTHT),
     .                IEPINT(0:MXxPHI,0:MXxRAD,2),
     .                NWRD(MXxCL), NELM(MXxCL)
      INTEGER*4       IDATA(MXxNW,MXxCL)
      EQUIVALENCE    (IDATA,RDATA)
      CHARACTER*64    BKNAME / 'Production:HDC;Hit_Cell' /
      PARAMETER       ( LNxHDC = 3 )
      DATA  EPSL      / 0.1 /
C>>>
      DATA  ETHRHD    / 5.E-2 /
C>>>
C>>>>
C .. to output particle info. at HDC entrance.
      REAL*4  HDPARTHIT(12), kinv
      INTEGER*4  NHDPARTHIT     
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
         IF ( MXxPHI.LT.NPHIHD .OR. MXxTHT.LT.NTHTHD .OR.
     .        MXxRAD.LT.NRADHD ) THEN
            PRINT *, '>>>>>>> Fatal Error in SMRHDC >>>>>>>>'
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
      NHDPARTHIT=0
      NHDCLS = 0
      CALL UVZERO(MXxNW*MXxCL,RDATA)
      CALL UVZERO((MXxPHI+1)*(MXxTHT+1),IBPINT)
      CALL UVZERO((MXxPHI+1)*(MXxRAD+1)*2,IEPINT)
C--
C  Loop over final state particles.
C--
      DO 30000 KPT = 1, NPRTCL
         IPT = IEPLST(KPT)
C--
C  Skip e, mu, neutrinos, gamma, and other exotic stable particles.
C--
         IPID = ABS(RPLIST( 2,IPT))
         IF ( IPID.EQ.11 .OR. IPID.EQ.12 .OR. IPID.EQ.13 .OR.
     .        IPID.EQ.14 .OR. IPID.EQ.16 .OR. IPID.EQ.22 .OR.
     .        IPID.GT.100000 )                   GO TO 30000
C--
C  Check if decay point inside the calorimeter inner wall.
C  If yes, skip this unstable particle.
C--
         NDOT = RPLIST(12,IPT)
         IF ( NDOT.GT.0 ) THEN
            IDT = RPLIST(13,IPT)
            XDK = RPLIST( 9,IDT)
            YDK = RPLIST(10,IDT)
            ZDK = RPLIST(11,IDT)
            RDK = SQRT(XDK*XDK+YDK*YDK)
            IF ( RDK.LE.RCYHD(2) .AND. ZDK.GE.ZCYHD(1) .AND.
     .           ZDK.LE.ZCYHD(2) )               GO TO 30000
         ENDIF
C--
C  Check if starting point inside the calorimeter inner wall.
C  If not, skip this particle.
C--
         X0  = RPLIST( 9,IPT)
         Y0  = RPLIST(10,IPT)
         Z0  = RPLIST(11,IPT)
         R0  = SQRT(X0*X0+Y0*Y0)
         IF ( R0.GT.RCYHD(2) .OR. Z0.LT.ZCYHD(1) .OR.
     .        Z0.GT.ZCYHD(2) )                   GO TO 30000
C--
C  Get intersection with HD calorimeter.
C--
         ICHG = RPLIST( 4,IPT)
         IF ( ICHG.NE.0 ) THEN
            CALL UHLXCY(RCYHD,ZCYHD,EPSL,RHELIX(1,IPT),PHI,XP,IRT)
         ELSE
            CALL ULINCY(RCYHD,ZCYHD,EPSL,RHELIX(1,IPT),PHI,XP,IRT)
         ENDIF
         IF ( IRT.LE.0 )                         GO TO 30000
C
C ... Generator Particle number
         NHDPARTHIT=NHDPARTHIT+1
         HDPARTHIT(1) = NHDPARTHIT
         HDPARTHIT(2) = IPT
         HDPARTHIT(3) = RPLIST( 2,IPT)   
         HDPARTHIT(4) = RPLIST( 4,IPT) 
C ... 5-8 = Px, Py,Pz, E
         if ( ichg.ne.0 ) then
            kinv =  1.0/RHELIX(3,IPT) 
            HDPARTHIT(5) = -kinv*sin(rhelix(2,ipt)+phi)
            HDPARTHIT(6) =  kinv*cos(rhelix(2,ipt)+phi)
            HDPARTHIT(7) =  kinv*rhelix(5,ipt)
         else 
            HDPARTHIT(5) = RPLIST(5,IPT)
            HDPARTHIT(6) = RPLIST(6,IPT)
            HDPARTHIT(7) = RPLIST(7,IPT)
         endif
         HDPARTHIT(8) = RPLIST( 8,IPT)
C ... 9-11 = X, Y, Z
         HDPARTHIT(9)  = XP(1)
         HDPARTHIT(10) = XP(2)
         HDPARTHIT(11) = XP(3)

         call tbput(1,'Production:HDC;Hit_Track',
     >                 NHDPARTHIT,11,HDPARTHIT,IRT)
C--

C--
C  Create cluster data.
C        RDATA(    1, ICL ) = E
C        IDATA(    2, ICL ) = ICEL
C             (    3, ICL ) = # contributing particles
C             ( 4-->, ICL ) = gnerator particle #'s
C--
C--Barrel.
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
         IF ( IRT.EQ.1 ) THEN
            PHIC = MOD(ATAN2(XP(2),XP(1))+x2PID,x2PID)
            IFIC = PHIC/DPHIHD
            RF   = RCYHD(2)
            RT   = SQRT(RF*RF+XP(3)*XP(3))
            CSTC = XP(3)/RT
            THTC = LOG( (1-CSTC)/(1+CSTC) )/2 - THMNHD
            ITHC = THTC/DTHTHD
C--
            ESUM   = RPLIST(8,IPT)
            XY(1)  = RF*(PHIC-IFIC*DPHIHD)
            XY(2)  = RF*(THTC-ITHC*DTHTHD)
            DXY(1) = RF*DPHIHD
            DXY(2) = RF*DTHTHD
            CALL SMRSHA(2,MXxHSG,DXY,XY,EXY)
            DO 1000 KTH = -MXxHSG, +MXxHSG
               ITHT = KTH + ITHC
               IF ( ITHT.LT.0 .OR. ITHT.GE.NTHTHD ) GO TO 1000
               DO 100 KFI = -MXxHSG, +MXxHSG
                  IFI  = KFI + IFIC
                  IPHI = MOD(IFI+NPHIHD,NPHIHD)
                  E    = ESUM*EXY(KFI,KTH)
                  ICL  = IBPINT(IPHI,ITHT)
                  IF ( ICL.EQ.0 ) THEN
                     NHDCLS   = NHDCLS + 1
                     IF ( NHDCLS.GT.MXxCL ) THEN
                        PRINT *, ' >>>> SMRHDC >>>> '
                        PRINT *, '    Too many cells in an event.'
                        PRINT *, '    Subsequent hits ignored. '
                        NHDCLS = NHDCLS - 1
                        IRET   = 1
                                                 GO TO 3500
                     ENDIF
                     IBPINT(IPHI,ITHT) = NHDCLS
                     RDATA( 1,NHDCLS)  = E
                     IDATA( 2,NHDCLS)  = 1000*ITHT + IPHI
                     IDATA( 3,NHDCLS)  = 1
                     IDATA(LNxHDC+1,NHDCLS) = IPT
                  ELSE
                     NPT = IDATA( 3,ICL)
                     NPT = NPT + 1
                     IF ( NPT.GT.MXxNW-LNxHDC ) THEN
                        PRINT *, ' >>>> SMRHDC >>>> '
                        PRINT *, '    Too many particles in a cell.'
                        PRINT *, '    STOP. '
                        STOP
                     ENDIF
                     RDATA( 1,ICL) = RDATA( 1,ICL) + E
                     IDATA( 3,ICL) = NPT
                     IDATA(LNxHDC+NPT,ICL) = IPT
                  ENDIF
100            CONTINUE
1000        CONTINUE
C--Endcap.
         ELSE
            IZE  = 2*IRT - 5
            IF ( IZE.EQ.-1 ) THEN
               RDMNHD = RMMNHD
               DRADHD = DRDMHD
            ELSE
               RDMNHD = RPMNHD
               DRADHD = DRDPHD
            ENDIF
C--
            PHIC = MOD(ATAN2(XP(2),XP(1))+x2PID,x2PID)
            IFIC = PHIC/DPHIHD
            RF   = SQRT(XP(1)*XP(1)+XP(2)*XP(2))
            RT   = SQRT(RF*RF+XP(3)*XP(3))
            ZMAX = SQRT(XP(3)*XP(3))
            SNRC = RF/RT
            RADC = LOG( (1+SNRC)/(1-SNRC) )/2 - RDMNHD
            IRDC = RADC/DRADHD
C--
            ESUM   = RPLIST(8,IPT)
            NCEL   = 0
            XY(1)  = RF*(PHIC-IFIC*DPHIHD)
            XY(2)  = ZMAX*(RADC-IRDC*DRADHD)
            DXY(1) = RF*DPHIHD
            DXY(2) = ZMAX*DRADHD
            CALL SMRSHA(2,MXxHSG,DXY,XY,EXY)
            DO 2000 KRD = -MXxHSG, +MXxHSG
               IRAD = KRD + IRDC
               IF ( IRAD.LT.0 .OR. IRAD.GE.NRADHD ) GO TO 2000
               DO 200 KFI = -MXxHSG, +MXxHSG
                  IFI  = KFI + IFIC
                  IPHI = MOD(IFI+NPHIHD,NPHIHD)
                  E    = ESUM*EXY(KFI,KRD)
                  ICL  = IEPINT(IPHI,IRAD,IRT-1)
                  IF ( ICL.EQ.0 ) THEN
                     NHDCLS   = NHDCLS + 1
                     IF ( NHDCLS.GT.MXxCL ) THEN
                        PRINT *, ' >>>> SMRHDC >>>> '
                        PRINT *, '    Too many cells in an event.'
                        PRINT *, '    Subsequent hits ignored. '
                        NHDCLS = NHDCLS - 1
                        IRET   = 1
                                                 GO TO 3500
                     ENDIF
                     IEPINT(IPHI,IRAD,IRT-1) = NHDCLS
                     RDATA( 1,NHDCLS)  = E
                     IDATA( 2,NHDCLS)
     .                        = IZE*( 1000000 + 1000*IRAD + IPHI )
                     IDATA( 3,NHDCLS)  = 1
                     IDATA(LNxHDC+1,NHDCLS) = IPT
                  ELSE
                     NPT = IDATA( 3,ICL)
                     NPT = NPT + 1
                     IF ( NPT.GT.MXxNW-LNxHDC ) THEN
                        PRINT *, ' >>>> SMRHDC >>>> '
                        PRINT *, '    Too many particles in a cell.'
                        PRINT *, '    STOP. '
                        STOP
                     ENDIF
                     RDATA( 1,ICL) = RDATA( 1,ICL) + E
                     IDATA( 3,ICL) = NPT
                     IDATA(LNxHDC+NPT,ICL) = IPT
                  ENDIF
200            CONTINUE
2000        CONTINUE
         ENDIF
C>>>
#ifdef TIME
      CALL CLOCK(T2,2,2)
#endif 
#ifdef TIME
      TIMER(16) = TIMER(16) + T2 - T1
#endif 
C>>>
30000 CONTINUE
C--
C  Smear energy deposits.
C--
3500  NCL = 0
C>>>
#ifdef TIME
      CALL CLOCK(T1,2,2)
#endif 
C>>>
      DO 4000 ICL = 1, NHDCLS
C--
         E    = RDATA( 1,ICL)
         IF ( E.LT.0.1*ETHRHD )                  GO TO 4000
C--
         ICEL = IDATA( 2,ICL)
         NPT  = IDATA( 3,ICL)
         IZE  = ICEL/1000000
C--
         IF ( IZE.EQ.0 ) THEN
            SIGE = SQRT(SGHDB0**2+(SGHDB/SQRT(E))**2)
            E    = E*( 1 + SIGE*RANN(ISEED) )
         ELSE
            SIGE = SQRT(SGHDE0**2+(SGHDE/SQRT(E))**2)
            E    = E*( 1 + SIGE*RANN(ISEED) )
         ENDIF
C--
C  Energy threshold.
C--
         IF ( E.LT.ETHRHD )                      GO TO 4000
         NCL  = NCL + 1
         IDATA( 1,NCL) = E*1000
         IDATA( 2,NCL) = ICEL
         IDATA( 3,NCL) = NPT
         DO 400 JPT = 1, NPT
            IDATA(LNxHDC+JPT,NCL) = IDATA(LNxHDC+JPT,ICL)
400      CONTINUE
         NWRD (   NCL) = LNxHDC + NPT
         NELM (   NCL) = NCL
4000  CONTINUE
C--
C  TBMPT 'Production:HDC;Hit_Cell'.
C        IBUFF(   1) = E( MeV)
C             (   2) = ICEL
C             (   3) = # contributing particles
C             (4-->) = 1-st gnerator particle #
C--
      IF ( NCL.GT.0 ) THEN
         CALL TBMPT(1,BKNAME,NCL,MXxNW,NELM,NWRD,IDATA,IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, 'SMRHDC failed to TBMPT ', BKNAME
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
      TIMER(17) = TIMER(17) + T2 - T1
#endif 
C>>>
C--
C  That's it.
C--
      RETURN
      END
