C***********************************************************************
C*
C*=======================================----===
C* Subroutine SWMEVT(IRECID,LEVEL,IDEBUG,IRET)
C*=======================================----===
C*
C* (Purpose)
C*     Handle swimming of Generator:Particle_List.
C* (Inputs)
C*        IDREC    : (I*4) :  record ID.
C*        LEVEL    : (I*4) :  level flag.
C*        IDEBUG   : (I*4) :  debug flag.
C* (Output)
C*        IRET     : (I*4) :  return flag.
C* (Relation)
C*     Called by steering.
C*     Calls the following routines;
C*        TBNOEL, TBGET, TBPUT, UVZERO, UP2HLX, and UPHELX.
C* (Updaterecord)
C*     5/21/90  K.Fujii           Original version.
C*     6/11/90  K.Fujii           Modified to use input decay length
C*                                if nonzero.
C*    29/02/00  A.Miyamoto        Fix problem of zero momentum particle
C*
C***********************************************************************
 
#ifndef HP
      SUBROUTINE SWMEVT(IRECID,LEVEL,IDEBUG,IRET)
#else
        SUBROUTINE SWMEVT_(IRECID,LEVEL,IDEBUG,IRET)
#endif 
 
#ifdef MSP
CMSP =EXPAND 'T#FP.S#JLC.FORT/SWMRND.inc'
CMSP =EXPAND 'T#PR.P#STER.FORT/PRFELD.inc'
#endif 
#if defined(SUN) || defined(HIUXF77) || defined(MACHTEN_PPC)
#include "swmrnd.inc"
#include "prfeld.inc"
#endif 

#ifdef HP
      COMMON /SWMRND_/ ISEED
      INTEGER*4       ISEED
C
      COMMON /PRFELD_/ BFIELD, PTOR, ROPT
      REAL*4          BFIELD, PTOR, ROPT
#endif 

      CHARACTER*64 BKNAME  / 'Generator:Particle_List' /
      PARAMETER (  LNxPT = 20, MXxPT = 2000 )
#ifndef HP
      COMMON /SSWORK/ IELM(MXxPT), RPLIST(LNxPT,MXxPT), HELX(8),
#else
        COMMON /SSWORK_/ IELM(MXxPT), RPLIST(LNxPT,MXxPT), HELX(8),
#endif 
     .                DPHI(2,MXxPT), PV(3), IMT(0:MXxPT), IDIDIT(MXxPT)
C
C========< Entry Point >================================================
C
C--
C  Check record ID.
C--
      IRET = 0
      IF ( IRECID.NE.1 ) RETURN
C--
C  Get #elements in 'Generator:Particle_List'.
C--
#ifndef HP
      CALL TBNOEL(1,BKNAME,NPRTCL,IELM)
#else
       CALL TBNOEL_(1,BKNAME,NPRTCL,IELM)
#endif 
      IELMX = IELM(NPRTCL)
      IF ( IELMX.GT.MXxPT ) THEN
         PRINT *, '>>>>>> Error in SWMEVT >>>>>>'
         PRINT *, '   MXxPT < IELMX = ', IELMX
         PRINT *, '   STOP.'
         STOP
      ENDIF
C--
C  TBGET 'Generator:Particle_List'.
C--
      DO 1000 IE = 1, NPRTCL
         IPT = IELM(IE)
         IF ( IPT.NE.IE ) THEN
            PRINT *, ' >>>>> Fatal Error in SWMEVT >>>>>>>'
            PRINT *, '    El# is not serial in ',BKNAME
            PRINT *, '    STOP. '
            STOP
         ENDIF
#ifndef HP
         CALL TBGET(1,BKNAME,IPT,NW,RPLIST(1,IPT),IRT)
#else
          CALL TBGET_(1,BKNAME,IPT,NW,RPLIST(1,IPT),IRT)
#endif 
         IF ( IRT.LT.0 ) THEN
            IRET = IRT
            PRINT *, ' SWMEVT failed to TBGET el#', IPT, ' of ', BKNAME
            PRINT *, '   IRT = ', IRT
            RETURN
         ELSE IF ( NW.NE.LNxPT ) THEN
            PRINT *, ' SWMEVT TBGETs wrong #words = ', NW
            PRINT *, ' from el#', IPT, ' of ', BKNAME
            PRINT *, ' STOP.'
            STOP
         ENDIF
1000  CONTINUE
C--
C  Loop over unstable particles and set decay informations.
C     DPHI(1,IPT) = deflection angle before decay.
C         (2,IPT) = total deflection angle before creation.
C--
#ifndef HP
      CALL UVZERO(2*MXxPT,DPHI)
      CALL UVZERO(MXxPT,IDIDIT)
#else
       CALL UVZERO_(2*MXxPT,DPHI)
       CALL UVZERO_(MXxPT,IDIDIT)
#endif 
      DO 200 KPT = 1, NPRTCL
         IPT  = IELM(KPT)
         NDT  = RPLIST(12,IPT)
         CTU  = RPLIST(16,IPT)
         IF ( NDT.EQ.0 .OR. CTU.LE.0. )          GO TO 200
C--
C  Decide decay length.
C--
         AM   = RPLIST(3,IPT)
         ICHG = RPLIST(4,IPT)
         PX   = RPLIST(5,IPT)
         PY   = RPLIST(6,IPT)
         PL   = RPLIST(7,IPT)
         PT2  = PX*PX+PY*PY
         PT   = SQRT(PT2)
         AP   = SQRT(PT2+PL*PL)
         DL   = RPLIST(17,IPT)
         IF ( DL.LE.0. ) DL = - (AP/AM)*CTU*LOG(1-RAN(ISEED))

C--
C  Calculate deflection angle.
C--
         IF ( ICHG.NE.0 ) THEN
            R   = PTOR*PT
            if( ap.eq.0.0 ) then
               dfi=0.0
            else  
               DFI = -ICHG*DL*PT/AP/R
            endif
         ELSE
            DFI = 0
         ENDIF
C--
         RPLIST(17,IPT) = DL
         DPHI  ( 1,IPT) = DFI
C--
C  TBPUT primary finite decay length particles.
C--

         IPR = RPLIST(14,IPT)
         IF ( IPR.LE.0 ) THEN
#ifndef HP
            CALL TBPUT(1,BKNAME,IPT,LNxPT,RPLIST(1,IPT),IRT)
#else 
             CALL TBPUT_(1,BKNAME,IPT,LNxPT,RPLIST(1,IPT),IRT)
#endif 
            IF ( IRT.LT.0 ) THEN
               IRET = IRT
               PRINT *, ' SWMEVT failed to TBPUT el#',
     .                      IPT, ' of ', BKNAME
               PRINT *, '   IRT = ', IRT
               RETURN
            ENDIF
         ENDIF
200   CONTINUE
C--
C  Loop over daughter particles and modify XV and PV.
C     IDIDIT(I) = 1 : I-th track already swam.
C               = 0 : I-th track is to swim.
C--
      DO 3000 KPT = 1, NPRTCL
         IPT  = IELM(KPT)
         IPR  = RPLIST(14,IPT)
         IDID = IDIDIT(IPT)
         IF ( IPR.LE.0 .OR. IDID.NE.0 )          GO TO 3000
C--
C  Decide parents of this daughter.
C     IMT(0) = particle # of this daughter.
C        (I) = particle # of IMT(I-1)-th particle's mother.
C  where I runs from 1 to NPR, the number of ancestors.
C--
         IMT(0) = IPT
         IDT    = IPT
         NPR    = 0
3        IPR    = RPLIST(14,IDT)
         IF ( IPR.GT.0 ) THEN
            NPR      = NPR + 1
            IMT(NPR) = IPR
            IDT      = IPR
            IDID     = IDIDIT(IPR)
            IF ( IDID.EQ.0 )                     GO TO 3
         ENDIF
C--
C  Loop over parents to accumulate deflections and displacements.
C--
         DO 300 KPR = NPR, 1, -1
C--
C  IPR-th particle already swam or does not need to swim.
C  IDT-th particle is to swim.
C--
            IPR   = IMT(KPR)
            IDT   = IMT(KPR-1)
C--
            DFISM = DPHI(2,IPR)
            ICHG  = RPLIST( 4,IPR)
C--
C  Modify the production point.
C--
            IF ( ICHG.NE.0 ) THEN
               DFI = DPHI(1,IPR)
#ifndef HP
               CALL UP2HLX(RPLIST(9,IPR),RPLIST(5,IPR),ICHG,HELX)
               CALL UPHELX(HELX,DFI,RPLIST(9,IDT),PV)
#else
                CALL UP2HLX_(RPLIST(9,IPR),RPLIST(5,IPR),ICHG,HELX)
                CALL UPHELX_(HELX,DFI,RPLIST(9,IDT),PV)
#endif 
               DFISM = DFISM + DFI
            ELSE
               PX = RPLIST( 5,IPR)
               PY = RPLIST( 6,IPR)
               PZ = RPLIST( 7,IPR)
               DL = RPLIST(17,IPR)
               AP = SQRT(PX*PX+PY*PY+PZ*PZ)
               if( ap.ne.0 ) then
               RPLIST( 9,IDT) = RPLIST( 9,IPR) + DL*PX/AP
               RPLIST(10,IDT) = RPLIST(10,IPR) + DL*PY/AP
               RPLIST(11,IDT) = RPLIST(11,IPR) + DL*PZ/AP
               endif
            ENDIF
C--
C  Modify the momentum.
C--
            CSF = COS(DFISM)
            SNF = SIN(DFISM)
            PX  = RPLIST(5,IDT)
            PY  = RPLIST(6,IDT)
            RPLIST(5,IDT) = CSF*PX - SNF*PY
            RPLIST(6,IDT) = SNF*PX + CSF*PY
C--
C  Flag this daughter not to modify it again and change DPHI
C  to the overall deflection angle measured from the primary
C  parent direction.
C--
            IDIDIT(IDT) = 1
            DPHI(2,IDT) = DFISM
C--
C  TBPUT modified 'Particle_List'.
C--
#ifndef HP
            CALL TBPUT(1,BKNAME,IDT,LNxPT,RPLIST(1,IDT),IRT)
#else
             CALL TBPUT_(1,BKNAME,IDT,LNxPT,RPLIST(1,IDT),IRT)
#endif 
            IF ( IRT.LT.0 ) THEN
               IRET = IRT
               PRINT *, ' SWMEVT failed to TBPUT el#',
     .                      IDT, ' of ', BKNAME
               PRINT *, '   IRT = ', IRT
               RETURN
            ENDIF
300      CONTINUE
3000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
