C***********************************************************************
C*
C*================================----===
C* Subroutine SMRVTX(LEVEL,IDEBUG,IRET)
C*================================----===
C*
C* (Purpose)
C*     Create Production:VTX;Space_Point and modify helix parameters
C*     taking mulitiple scattering into account.
C* (Inputs)
C*        LEVEL    : (I*4) :  level flag.
C*        IDEBUG   : (I*4) :  debug flag.
C* (Output)
C*        IRET     : (I*4) :  return flag.
C* (Relation)
C*     Called by SMREVT.
C*     Calls the following routines;
C* (Updaterecord)
C*     2/30/91  K.Fujii      Original version.
C*                           Treatments exploited in this version
C*                           has the following defects.
C*                           If a particle decays in the vertex
C*                           detector, the position of the decay
C*                           do not necessary agree with the
C*                           production points of its daughter
C*                           particles due to multiple scattering.
C*                           Decay or production points are tested
C*                           using 'Generator:Particle_List'
C*                           which could be inconsistent with the
C*                           points after multiple scattering is
C*                           taken into account. Most unstable
C*                           particles of our interest should decay
C*                           inside the first measuring layer, thus
C*                           these defects should be minor.
C*
C*    12/24/98  A.Miyamoto   Allow VTX space resolution less than
C*                           pixel size/sqrt(12).  When NERRVX=2,
C*                           hit point, RDATA(i,k), is smeared with 
C*                           gaussian resolution given by ERRVX(1).
C*                           Error of each hit points are saved in
C*                           Production:VTX:Space_Point_Error, element
C*                           corresponds to that of Production:VTX:Space_Point 
C*
C*     3/29/99  A.Miyamoto   Output smeared Helix parameter and hits for IT
C*    
C***********************************************************************
 
      SUBROUTINE SMRVTX(LEVEL,IDEBUG,IRET)
 
#include "smrgdt.inc"
#include "smrrnd.inc"
#include "smrvgo.inc"
#include "smrvtp.inc"
#include "unumcn.inc"
      PARAMETER     ( LNxVTX = 3 )
      PARAMETER     ( LNeVTX = 2 )
      COMMON /SSWORK/ RDATA(LNxVTX,100), XV(3), PV(3), QV(3), EE(3,3),
     .                RCY(2), EDATA(LNeVTX,100)
      REAL   *4       RDATA, XV, PV, QV, EE, RCY
      REAL   *4       SMRHLX(100)
      INTEGER*4       NSMRHL(100)
      EQUIVALENCE     (SMRHLX(1),NSMRHL(1))
      REAL   *8       HLX8(100)
      CHARACTER*64    BKNAME / 'Production:VTX;Space_Point' /
      CHARACTER*64    BKHELX / 'Production:VTX;Track_Parameter' /
      CHARACTER*64    BKIT / 'Production:IT;Space_Point' /
      DATA CNSMS      / 14.1E-3 /
C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--

      IRET = 0
      IF ( NSMPVX.LT.0 ) RETURN
C--
C  Loop over final state particles.
C--
      DO 1000 KPT = 1, NPRTCL
         IPT = IEPLST(KPT)
C--
C  Skip neutral particles.
C--
         ICHG = RPLIST(4,IPT)
         IF ( ICHG.EQ.0 )                        GO TO 1000
C--
C  Calculate R of the production point.
C--
         XPR = RPLIST( 9,IPT)
         YPR = RPLIST(10,IPT)
         ZPR = RPLIST(11,IPT)
         RPR = SQRT(XPR*XPR+YPR*YPR)
         IF ( RPR.GT.RCYVX(NSMPVX) )             GO TO 1000
C--
C  Calculate R of the decay point.
C--
         NDOT = RPLIST(12,IPT)
         IF ( NDOT.GT.0 ) THEN
            IDT = RPLIST(13,IPT)
            XDK = RPLIST( 9,IDT)
            YDK = RPLIST(10,IDT)
            ZDK = RPLIST(11,IDT)
            RDK = SQRT(XDK*XDK+YDK*YDK)
         ELSE
            RDK = 99999.
         ENDIF
         IF ( RDK.LT.RCYVX(0) )                  GO TO 1000
C--
C  Loop over layers.
C     LYR = 0      : beampipe.
C         =  ...   : measuring layer.
C         = NSMPVX : CDC inner cylinder.
C  For LYR = 0 and NSMPVX, only multiple scattering treatment
C  is applied.
C--
         NHIT = 0
         NSMRED = 0
         DO 100 LYR = 0, NSMPVX
C--
C  If unstable, check if decay point inside the measuring layer.
C  If yes, skip this particle.
C--

            IF ( RDK.LT.RCYVX(LYR) )             GO TO 1000
C--
C  Check if production point outside the measuring layer.
C  If yes, skip this layer.
C--
            IF ( RPR.GT.RCYVX(LYR) )             GO TO 100
C--
C  Get hit point. If no intersection, go to next layer.
C--
            RCY(1) = 1.E-2
            RCY(2) = RCYVX(LYR)
            CALL UHLXCY(RCY,ZCYVX(1,LYR),EPSL,
     .                  RHELIX(1,IPT),PHI,XV,IRT)
            IF ( IRT.NE.1 )                      GO TO 100
C--
C  If LYR is a measuring layer, save this hit.
C--
            RXV  = SQRT(XV(1)*XV(1)+XV(2)*XV(2))
            IF ( LYR.GT.0 .AND. LYR.LT.NSMPVX ) THEN
               NHIT = NHIT + 1
               RDATA(1,NHIT) = RXV
               IF( NERRVX .EQ. 1 ) THEN
                 DPHI = DPHIVX/RXV
                 IFXV = MOD(ATAN2(XV(2),XV(1))+x2PI,x2PI)/DPHI
                 IZXV = XV(3)/DZEEVX
                 RDATA(2,NHIT) = (IFXV+0.5)*DPHI
                 RDATA(3,NHIT) = (IZXV+SIGN(0.5,XV(3)))*DZEEVX
               ELSEIF( NERRVX.EQ.2 ) THEN
                 EDATA(1,NHIT) = ERRVX(1) 
                 EDATA(2,NHIT) = ERRVX(1) 
                 DPHI = ERRVX(1)/RXV*RANN(ISEED)
                 RDATA(2,NHIT) = ATAN2(XV(2),XV(1))+DPHI
                 RDATA(3,NHIT) = XV(3)+ERRVX(1)*RANN(ISEED)
               ELSE
                 IPE = 1
                 IF( LYR .GT. NUMVTX ) IPE=3
                 EDATA(1,NHIT) = ERRVX(IPE) 
                 EDATA(2,NHIT) = ERRVX(IPE+1) 
                 DPHI = ERRVX(IPE)/RXV*RANN(ISEED)
                 RDATA(2,NHIT) = ATAN2(XV(2),XV(1))+DPHI
                 RDATA(3,NHIT) = XV(3)+ERRVX(IPE+1)*RANN(ISEED)
               ENDIF
            ENDIF
C--
C  Modify momentum according to multiple scattering.
C--
            CALL UPHELX(RHELIX(1,IPT),PHI,XV,PV)
            AM    = RPLIST(3,IPT)
            APV   = PV(1)*PV(1) + PV(2)*PV(2) + PV(3)*PV(3)
            BET   = SQRT(APV/(APV+AM*AM))
            APV   = SQRT(APV)
            CSTH  = (XV(1)*PV(1)+XV(2)*PV(2))/(RXV*APV)
            X0    = RDLVX(LYR)/CSTH
            THT0  = (CNSMS/(BET*APV))*ABS(ICHG)*
     .              SQRT(X0)*(1+LOG10(X0)/9)
            QV(1) = THT0*RANN(ISEED)
            QV(2) = THT0*RANN(ISEED)
            QV(3) = 0
            CALL USETRF(PV,EE)
            CALL UBTRAN(QV,EE,QV)
            CALL UADD3(QV,EE(1,3),QV)
            CALL UNRMV3(QV,QV)
            CALL USCLM3(APV,QV,PV)
C--
C  Update helix paramters.
C--

            CALL UP2HLX(XV,PV,ICHG,RHELIX(1,IPT))

C--
C  Updated smeared VTX;Track_Parameter when NERRVX > 2
C--
            IF(NERRVX.GT.2.AND.NHIT.EQ.1.AND.
     >              LYR.LE.2.AND.NSMRED.EQ.0) THEN
C
C ... See the comment in smrcht.F for the format of SMRVHL
C
               CALL SMRVHL(RPLIST(1,IPT),RHELIX(1,IPT),
     >              SMRHLX(11),IRTH)
               DO 120 I = 1, 8
                  HLX8(I)=SMRHLX(I+10)
 120           CONTINUE
               CALL UFDCAP(HLX8, SMRHLX(5), SMRHLX(1))
               NSMRED=1

               SMRHLX(4)=SQRT(SMRHLX(1)**2+SMRHLX(2)**2+SMRHLX(3)**2)
               NSMRHL(8)=0
               NSMRHL(9)=SIGN(1.0,SMRHLX(13))
               NSMRHL(10)=0
               NSMRHL(56)=1
               NSMRHL(57)=IPT
            ENDIF

100      CONTINUE
C--
C  TBPUT 'Production:VTX;Space_Point' where element# is IPT.
C        RDATA(1,LYR) = r
C             (2,LYR) = phi
C             (3,LYR) = z
C  TBPUT 'Production:VTX;Space_Point_Error' where element# is IPT.
C        EDATA(1,LYR) = r*phi error
C             (2,LYR) = z error
C--
         IF ( NHIT.GT.0 ) THEN
            CALL TBPUT(1,BKNAME,IPT,LNxVTX*NHIT,RDATA(1,1),IRT)
            IF ( IRT.LT.0 ) THEN
               PRINT *, 'SMRVTX failed to TBPUT e#', IPT, ' of ',BKNAME
               PRINT *, '    IRT = ', IRT
               IRET = IRT
               RETURN
            ENDIF
            IF( NERRVX .GE. 2 ) THEN
               CALL TBPUT(1,'Production:VTX;Space_Point_Error',
     >              IPT, LNeVTX*NHIT, EDATA(1,1), IRT)
               IF ( IRT.LT.0 ) THEN
                 PRINT *, 'SMRVTX failed to TBPUT e#', IPT, ' of ',
     >              'Production:VTX;Space_Point_Error'
                 PRINT *, '    IRT = ', IRT
                 IRET = IRT
                 RETURN
              ENDIF
            ENDIF

            IF( NERRVX.EQ.3 .AND. NSMRED.EQ.1 ) THEN
               CALL TBPUT(1,'Production:VTX;Track_Parameter',
     >              IPT, 57, SMRHLX, IRT) 
               IF ( IRT.LT.0 ) THEN
                 PRINT *, 'SMRVTX failed to TBPUT e#', IPT, ' of ',
     >              'Production:VTX;Track_Parameter'
                 PRINT *, '    IRT = ', IRT
                 IRET = IRT
                 RETURN
               ENDIF
            ENDIF
   
         ENDIF
1000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
