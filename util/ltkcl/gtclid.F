CC**********************************************************************
C*
C*=============================--------------------===
C*  Subrouitne GTCLID(IEH,HELX,XP,NCELL,ICELL,DCELL)
C*=============================--------------------===
C*
C* (Purpose)
C*     Get possible hit cells for a given track.
C* (Inputs)
C*     IEH          :(I*4): (1,2) = (EMC,HDC).
C*     HELX(1-8)    :(R*4): helix parameters.
C* (Outputs)
C*     XP   ( 1-3 ) :(R*4): expected hit position.
C*     NCELL        :(I*4): # possible hit cells.
C*     ICELL(  ICL) :(I*4): ID of ICL-th possible hit cell.
C*     DCELL(1,ICL) :(R*4): delta_zeta in units of cell width.
C*          (2,ICL) :(R*4): delta_phi  in units of call width.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*     7/09/92  K. Fujii               Original version.
C*
CC**********************************************************************
 
      SUBROUTINE GTCLID(IEH,HELX,XP,NCELL,ICELL,DCELL)
 
#include "smrego.inc"
#include "smrhgo.inc"
#include "unumcn.inc"
      INTEGER*4   IEH, NCELL, ICELL(*)
      REAL   *4   HELX(8), XP(3), DCELL(2,*)
C--
      REAL   *4   RCY(2), ZCY(2)
      DATA  EPSL  / 0.1  /
      DATA  DTST  / 0.75 /
      EXTERNAL    UCONST
C
C========< Entry Point >================================================
C
C--
C  Set cylinder geometry according to calorimeter type.
C--
      IF ( IEH.EQ.1 ) THEN
         CALL UVCOPY(2,RCYEM(1),RCY(1))
         CALL UVCOPY(2,ZCYEM(1),ZCY(1))
         NPHI = NPHIEM
         NTHT = NTHTEM
         NRAD = NRADEM
         DTHT = DTHTEM
         DPHI = DPHIEM
         THMN = THMNEM
         RMMN = RMMNEM
         DRDM = DRDMEM
         RPMN = RPMNEM
         DRDP = DRDPEM
      ELSE IF ( IEH.EQ.2 ) THEN
         CALL UVCOPY(2,RCYHD(1),RCY(1))
         CALL UVCOPY(2,ZCYHD(1),ZCY(1))
         NPHI = NPHIHD
         NTHT = NTHTHD
         NRAD = NRADHD
         DTHT = DTHTHD
         DPHI = DPHIHD
         THMN = THMNHD
         RMMN = RMMNHD
         DRDM = DRDMHD
         RPMN = RPMNHD
         DRDP = DRDPHD
      ELSE
         PRINT *, ' >>>> GETCEL >>>>'
         PRINT *, '   Invalid IEH = ', IEH, ' STOP.'
         STOP
      ENDIF
C--
C  Calculate intersection.
C--
      CALL UHLXCY(RCY,ZCY,EPSL,HELX,PHI,XP,IRT)
C>>>
C     IF ( IRT.LE.0 .AND. NEV.LE.20 ) THEN
C        PRINT *, ' IRT     = ', IRT
C        PRINT *, ' RCY     = ', RCY(1), RCY(2)
C        PRINT *, ' ZCY     = ', ZCY(1), ZCY(2)
C        PRINT *, ' HELX(1) = ', HELX(1)
C        PRINT *, '     (2) = ', HELX(2)
C        PRINT *, '     (3) = ', HELX(3)
C        PRINT *, '     (4) = ', HELX(4)
C        PRINT *, '     (5) = ', HELX(5)
C     ENDIF
C>>>
      NCELL = 0
      IF ( IRT.LE.0 ) RETURN
C--
C  Decide cell number.
C--
C--Barrel.
      IF ( IRT.EQ.1 ) THEN
         PHI  = MOD(ATAN2(XP(2),XP(1))+x2PID,x2PID)
         RF   = RCY(2)
         RT   = SQRT(RF*RF+XP(3)*XP(3))
         CST  = XP(3)/RT
         THT  = LOG( (1-CST)/(1+CST) )/2 - THMN
         IFI  = PHI/DPHI
         ITH  = THT/DTHT
C--
         DO 10 JFI = IFI-1, IFI+1
            JTHMN = MAX(ITH-1,0)
            JTHMX = MIN(ITH+1,NTHT)
            DO 1 JTH = JTHMN, JTHMX
               DF    = PHI/DPHI - ( JFI + 0.5 )
               DT    = THT/DTHT - ( JTH + 0.5 )
               IF ( ABS(DF).LT.DTST .AND. ABS(DT).LT.DTST ) THEN
                  IF ( JFI.GE.NPHI ) THEN
                     KFI = JFI - NPHI
                  ELSE IF ( JFI.LT.0 ) THEN
                     KFI = JFI + NPHI
                  ELSE
                     KFI = JFI
                  ENDIF
                  NCELL          = NCELL + 1
                  ICEL           = 1000*JTH + KFI
                  ICELL(  NCELL) = ICEL
                  DCELL(1,NCELL) = DF
                  DCELL(2,NCELL) = DT
               ENDIF
1           CONTINUE
10       CONTINUE
C--Endcap.
      ELSE
         IZE  = 2*IRT - 5
         IF ( IZE.EQ.-1 ) THEN
            RDMN   = RMMN
            DRAD   = DRDM
         ELSE
            RDMN   = RPMN
            DRAD   = DRDP
         ENDIF
         PHI  = MOD(ATAN2(XP(2),XP(1))+x2PID,x2PID)
         RF   = SQRT(XP(1)*XP(1)+XP(2)*XP(2))
         RT   = SQRT(RF*RF+XP(3)*XP(3))
         SNR  = RF/RT
         RAD  = LOG( (1+SNR)/(1-SNR) )/2 - RDMN
         IFI  = PHI/DPHI
         IRD  = RAD/DRAD
C--
         DO 20 JFI = IFI-1, IFI+1
            JRDMN = MAX(IRD-1,0)
            JRDMX = MIN(IRD+1,NRAD)
            DO 2 JRD = JRDMN, JRDMX
               DF    = PHI/DPHI - ( JFI + 0.5 )
               DR    = RAD/DRAD - ( JRD + 0.5 )
               IF ( ABS(DF).LT.DTST .AND. ABS(DR).LT.DTST ) THEN
                  IF ( JFI.GE.NPHI ) THEN
                     KFI = JFI - NPHI
                  ELSE IF ( JFI.LT.0 ) THEN
                     KFI = JFI + NPHI
                  ELSE
                     KFI = JFI
                  ENDIF
                  NCELL          = NCELL + 1
                  ICEL           = IZE*( 1000000 + 1000*JRD + KFI )
                  ICELL(  NCELL) = ICEL
                  DCELL(1,NCELL) = DF
                  DCELL(2,NCELL) = DR
               ENDIF
2           CONTINUE
20       CONTINUE
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
