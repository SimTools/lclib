C***********************************************************************
C*
C*====================
C* Subroutine SMRJIN
C*====================
C*
C* (Purpose)
C*     Job initialization for searing module.
C* (Inputs) None.
C* (Output) None.
C* (Relation)
C*     Called by steering.
C*     Calls no subroutines.
C* (Updaterecord)
C*     4/18/90  K.Fujii           Original version.
C*     5/25/90  K.Fujii           Initialize sampling step for
C*                                tracking device.
C*     2/04/92  K.Fujii           Modify radii and zees of measuring
C*                                layers to equalize pixel size.
C*     5/20/92  K.Fujii           Modify segmentation scheme for the
C*                                barrl parts of calorimeters to
C*                                equalize block size.
C*     7/01/92  K.Fujii           Fixed a bug.
C*
C***********************************************************************
 
      SUBROUTINE SMRJIN
 
#include "smrvgo.inc"
#include "smrvtp.inc"
#include "smrtgo.inc"
#include "smrego.inc"
#include "smrhgo.inc"
#include "unumcn.inc"
C>>>>
#ifdef TIME
      PARAMETER ( MXxTIM = 20 )
#endif 
#ifdef TIME
      COMMON /TIMER/ TIMER(MXxTIM)
#endif 
C>>>>
      EXTERNAL UCONST, SMRBLK
C
C========< Entry Point >================================================
C
C--
C  Reset timer.
C--
C>>>>
#ifdef TIME
      CALL UVZERO(MXxTIM,TIMER)
#endif 
C>>>>
C--
C  Set version number and date.
C--
C      CALL PRPVRS('Smearing:',203,910204)
C
C 95/09/02  K.Fujii	modified for rundom number continuation.
C
      CALL PRPVRS('Smearing:',204,950902)
C--
C  Initialize vertex detector smearing.
C--
      IF ( NSMPVX.GT.1 .AND. NERRVX .EQ. 1) THEN
         PRINT *, ' ******* SMRJIN modified /SMRVGO/ ******* '
         DO 10 LYR = 1, NSMPVX - 1
            NPHI         = x2PI*RCYVX(LYR)/DPHIVX
            NZEM         = ZCYVX(1,LYR)/DZEEVX
            NZEP         = ZCYVX(2,LYR)/DZEEVX
            RCYVX(  LYR) = DPHIVX*NPHI/x2PI
            ZCYVX(1,LYR) = DZEEVX*NZEM
            ZCYVX(2,LYR) = DZEEVX*NZEP
            PRINT *, '    RCYVX(  ', LYR ,') = ', RCYVX(LYR)
            PRINT *, '    ZCYVX(1,', LYR ,') = ', ZCYVX(1,LYR)
            PRINT *, '    ZCYVX(2,', LYR ,') = ', ZCYVX(2,LYR)
10       CONTINUE

      ELSEIF( NSMPVX.GT.1 .AND. NERRVX .EQ. 3 ) THEN
         RCYVT(1) = RCYVX(1)
         RCYVT(2) = RCYVX(NUMVTX)
         ZCYVT(1) = ZCYVX(1,NUMVTX)
         ZCYVT(2) = ZCYVX(2,NUMVTX)
         SIGRFV   = ERRVX(1)
         SIGZEV   = ERRVX(2)
         DSMPVT = (RCYVT(2)-RCYVT(1))/(NUMVTX+1)

      ENDIF
C--
C  Initialize charged track smearing.
C--
      DSMPTK = (RCYTK(2)-RCYTK(1))/(NSMPTK+1)
C--
C  Initialize EM cluster smearing.
C  THT now(92/05/20) means zeta defined below. Equal spacing
C  segmentation in zeta corresponds to equal counter size.
C     2*zeta     = ln( ( 1 - cos(theta) )/( 1 + cos(theta) ) )
C     cos(theta) = (1-exp(2*zeta))/(1+exp(2*zeta))
C--
      DPHIEM = x2PI/NPHIEM
      RZ1    = SQRT(RCYEM(2)**2+ZCYEM(1)**2)
      RZ2    = SQRT(RCYEM(2)**2+ZCYEM(2)**2)
      CSMN   = ZCYEM(2)/RZ2
      CSMX   = ZCYEM(1)/RZ1
      THMNEM = LOG((1-CSMN)/(1+CSMN))/2
      THMXEM = LOG((1-CSMX)/(1+CSMX))/2
      DTHTEM = (THMXEM-THMNEM)/NTHTEM
C--
      RZ1    = SQRT(RCYEM(1)**2+ZCYEM(1)**2)
      RZ2    = SQRT(RCYEM(2)**2+ZCYEM(1)**2)
      SNMN   = RCYEM(1)/RZ1
      SNMX   = RCYEM(2)/RZ2
      RMMNEM = LOG((1+SNMN)/(1-SNMN))/2
      RMMXEM = LOG((1+SNMX)/(1-SNMX))/2
      DRDMEM = (RMMXEM-RMMNEM)/NRADEM
C--
      RZ1    = SQRT(RCYEM(1)**2+ZCYEM(2)**2)
      RZ2    = SQRT(RCYEM(2)**2+ZCYEM(2)**2)
      SNMN   = RCYEM(1)/RZ1
      SNMX   = RCYEM(2)/RZ2
      RPMNEM = LOG((1+SNMN)/(1-SNMN))/2
      RPMXEM = LOG((1+SNMX)/(1-SNMX))/2
      DRDPEM = (RPMXEM-RPMNEM)/NRADEM
C--
C  Initialize HD cluster smearing.
C--
      DPHIHD = x2PI/NPHIHD
      RZ1    = SQRT(RCYHD(2)**2+ZCYHD(1)**2)
      RZ2    = SQRT(RCYHD(2)**2+ZCYHD(2)**2)
      CSMN   = ZCYHD(2)/RZ2
      CSMX   = ZCYHD(1)/RZ1
      THMNHD = LOG((1-CSMN)/(1+CSMN))/2
      THMXHD = LOG((1-CSMX)/(1+CSMX))/2
      DTHTHD = (THMXHD-THMNHD)/NTHTHD
C--
      RZ1    = SQRT(RCYHD(1)**2+ZCYHD(1)**2)
      RZ2    = SQRT(RCYHD(2)**2+ZCYHD(1)**2)
      SNMN   = RCYHD(1)/RZ1
      SNMX   = RCYHD(2)/RZ2
      RMMNHD = LOG((1+SNMN)/(1-SNMN))/2
      RMMXHD = LOG((1+SNMX)/(1-SNMX))/2
      DRDMHD = (RMMXHD-RMMNHD)/NRADHD
C--
      RZ1    = SQRT(RCYHD(1)**2+ZCYHD(2)**2)
      RZ2    = SQRT(RCYHD(2)**2+ZCYHD(2)**2)
      SNMN   = RCYHD(1)/RZ1
      SNMX   = RCYHD(2)/RZ2
      RPMNHD = LOG((1+SNMN)/(1-SNMN))/2
      RPMXHD = LOG((1+SNMX)/(1-SNMX))/2
      DRDPHD = (RPMXHD-RPMNHD)/NRADHD
C--
C  That's it.
C--
      RETURN
      END


