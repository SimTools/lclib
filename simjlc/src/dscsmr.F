C***********************************************************************
C*
C*====================
C* Subroutine DSCSMR
C*====================
C*
C* (Function)
C*     Describe smearing.
C* (Input & Output)
C*     None.
C* (Relation)
C*     CALLed by FLANLI
C* (Update Record)
C*    4/23/90  K.Fujii             Original version.
C*    6/07/90  K.Fujii             Output bank names changed.
C*    2/04/91  K.Fujii             Added vertex detector bank.
C*    3/19/93  K.Fujii             Added CDC_VTX track bank.
C*
C***********************************************************************
 
      SUBROUTINE DSCSMR
 
#include "fldscr.inc"
#include "fllevl.inc"
C
C FUNCTIONS
C
      EXTERNAL      SMRPRD
      EXTERNAL      SMRBLK
      EXTERNAL      SMRRIN
      EXTERNAL      SMRRTM
      EXTERNAL      SMRJIN
      EXTERNAL      SMRJTM
      EXTERNAL      SMREVT
      CHARACTER*12  PRGNAM/'Smearing:   '/
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
      CALL EXTSUB ( ENTDIA(NPROG), SMRPRD )
C--
C  Define Initialization entry.
C--
      CALL EXTSUB ( ENTBRU(NPROG), SMRRIN )
      CALL EXTSUB ( ENTERU(NPROG), SMRRTM )
      CALL EXTSUB ( ENTINI(NPROG), SMRJIN )
      CALL EXTSUB ( ENTTER(NPROG), SMRJTM )
C--
C  Event Analysis routine.
C--
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB ( ENTEVT(NENTR), SMREVT )
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
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:VTX;Space_Point'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:VTX;Space_Point_Error'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:CDC;Track_Parameter'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:EMC;Hit_Pad'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:EMC;Hit_Cell'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:HDC;Hit_Cell'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:EMC;Cluster'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:HDC;Cluster'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:EMC;Cluster_to_Cell'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:HDC;Cluster_to_Cell'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'Production:CDC_VTX;Track_Parameter'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
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
