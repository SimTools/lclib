C***********************************************************************
C*
C*====================
C* Subroutine DSCSWM
C*====================
C*
C* (Function)
C*     Describe swimming of short-lived particles.
C* (Input & Output)
C*     None.
C* (Relation)
C*     CALLed by FLANLI
C* (Update Record)
C*    5/21/90  K.Fujii             Original version.
C*
C***********************************************************************
 
      SUBROUTINE DSCSWM
 
#include "fldscr.inc"
#include "fllevl.inc"
C
C FUNCTIONS
C
      EXTERNAL      SWMPRD
      EXTERNAL      SWMBLK
      EXTERNAL      SWMRIN
      EXTERNAL      SWMRTM
      EXTERNAL      SWMJIN
      EXTERNAL      SWMJTM
      EXTERNAL      SWMEVT
      CHARACTER*12  PRGNAM/'Swimmer:    '/
      DATA IDOLVL / 1 / IDBLVL / 0 /
C
C========< Entry Point >================================================
C
C--
C  Define module name.
C--
      NPROG = NPROG + 1
      NAMCOM(NPROG) = PRGNAM
      ENTDIA(NPROG) = 0
      ENTSTA(NPROG) = 0
      CALL EXTSUB ( ENTDIA(NPROG), SWMPRD )
C--
C  Define Initialization entry.
C--
      CALL EXTSUB ( ENTBRU(NPROG), SWMRIN )
      CALL EXTSUB ( ENTERU(NPROG), SWMRTM )
      CALL EXTSUB ( ENTINI(NPROG), SWMJIN )
      CALL EXTSUB ( ENTTER(NPROG), SWMJTM )
C--
C  Event Analysis routine.
C--
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB ( ENTEVT(NENTR), SWMEVT )
C--
C  Declear input banks.
C--
      INPBLK(NENTR) = 0
      INPBLK(NENTR) = INPBLK(NENTR) + 1
      INBNAM(NENTR,INPBLK(NENTR)) = 'Generator:Header'
      INPBLK(NENTR) = INPBLK(NENTR) + 1
      INBNAM(NENTR,INPBLK(NENTR)) = 'Generator:Particle_List'
C--
C  Declear output banks.
C--
      OUTBLK(NENTR) = 0
C--
C  Define default calling sequence.
C--
      LISTSP(0)     = NENTR
      LISTSP(NENTR) = NENTR
C--
C  Define Level and Debug operand.
C--
      EXELVL(NENTR) = IDOLVL
      DBGLVL(NENTR) = IDBLVL
C--
C  That's it.
C--
      RETURN
      END
