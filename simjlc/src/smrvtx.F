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
C***********************************************************************
 
      SUBROUTINE SMRVTX(LEVEL,IDEBUG,IRET)
 
#include "smrgdt.inc"
#include "smrrnd.inc"
#include "smrvgo.inc"
#include "unumcn.inc"
      PARAMETER     ( LNxVTX = 3 )
      COMMON /SSWORK/ RDATA(LNxVTX,100), XV(3), PV(3), QV(3), EE(3,3),
     .                RCY(2)
      REAL   *4       RDATA, XV, PV, QV, EE, RCY
      CHARACTER*64    BKNAME / 'Production:VTX;Space_Point' /
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
               DPHI = DPHIVX/RXV
               IFXV = MOD(ATAN2(XV(2),XV(1))+x2PI,x2PI)/DPHI
               IZXV = XV(3)/DZEEVX
               RDATA(1,NHIT) = RXV
               RDATA(2,NHIT) = (IFXV+0.5)*DPHI
               RDATA(3,NHIT) = (IZXV+SIGN(0.5,XV(3)))*DZEEVX
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
100      CONTINUE
C--
C  TBPUT 'Production:VTX;Space_Point' where element# is IPT.
C        RDATA(1,LYR) = r
C             (2,LYR) = phi
C             (3,LYR) = z
C--
         IF ( NHIT.GT.0 ) THEN
            CALL TBPUT(1,BKNAME,IPT,LNxVTX*NHIT,RDATA(1,1),IRT)
            IF ( IRT.LT.0 ) THEN
               PRINT *, 'SMRVTX failed to TBPUT e#', IPT, ' of ',BKNAME
               PRINT *, '    IRT = ', IRT
               IRET = IRT
               RETURN
            ENDIF
         ENDIF
1000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
