CC**********************************************************************
C*
C*===========================
C*  Subrouitne INICLS(IRET)
C*===========================
C*
C* (Purpose)
C*     Initialize cluster finding.
C* (Inputs)
C* (Outputs)
C*     IRET   ; return flag.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*     7/09/92  K. Fujii               Original version.
C*
CC**********************************************************************
 
      SUBROUTINE INICLS(IRET)
 
#include "smrego.inc"
#include "smrhgo.inc"
#include "smrres.inc"
#include "unumcn.inc"
#include "prfeld.inc"
      PARAMETER      ( LNxPRM = 200 )
      REAL*4          SMRPRM(LNxPRM)
      CHARACTER*64    BKNAME / 'Smearing' /
      EXTERNAL UCONST
C
C==================< Entry Point >======================================
C
C--
C  TBGET smearing parameters.
C--
      CALL TBGET(2,BKNAME,2,NW,SMRPRM,IRT)
C--
C  Set /PRFELD/.
C--
      BSMR   = SMRPRM( 2)
      PTOR   = PTOR*BFIELD/BSMR
      ROPT   = 1/PTOR
      BFIELD = BSMR
C--
C  Set /SMRRES/.
C--
      SIGRF     = SMRPRM(17)
      SIGZE     = SMRPRM(18)
C--
      SGEMB0    = SMRPRM(38)
      SGEMB     = SMRPRM(39)
      SGEME0    = SMRPRM(40)
      SGEME     = SMRPRM(41)
C--
      SGHDB0    = SMRPRM(58)
      SGHDB     = SMRPRM(59)
      SGHDE0    = SMRPRM(60)
      SGHDE     = SMRPRM(61)
C--
C  Set /SMREGO/ and /SMRHGO/.
C--
      NPHIEM    = SMRPRM(31)
      NTHTEM    = SMRPRM(32)
      NRADEM    = SMRPRM(33)
      RCYEM(1)  = SMRPRM(34)
      RCYEM(2)  = SMRPRM(35)
      ZCYEM(1)  = SMRPRM(36)
      ZCYEM(2)  = SMRPRM(37)
C--
      NPHIHD    = SMRPRM(51)
      NTHTHD    = SMRPRM(52)
      NRADHD    = SMRPRM(53)
      RCYHD(1)  = SMRPRM(54)
      RCYHD(2)  = SMRPRM(55)
      ZCYHD(1)  = SMRPRM(56)
      ZCYHD(2)  = SMRPRM(57)
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
C  Set return flag and return.
C--
      IRET = 0
      RETURN
      END
