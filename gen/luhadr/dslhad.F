C***********************************************************************
C*
C* -----------------
C* Subroutine DSLHAD
C* -----------------
C*
C*(Function)
C*   Describe LUND hadronizer.
C*
C*(Input & Output)
C*   None.
C*
C*(Relation)
C*   CALLed from FLANLI
C*
C*(Author)
C*   A. Miyamoto    9-Feb-1990  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DSLHAD
C
C =EXPAND 'T#PR.FULL.FORT(@FLDSCR)'
C =EXPAND 'T#PR.FULL.FORT(@FLLEVL)'
C =EXPAND 'T#PR.P#STER.FORT(@PRJFLG)'
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
C
C FUNCTIONS
C
      EXTERNAL      FLNPAR
      EXTERNAL      FLNBRU
      EXTERNAL      FLNERU
      EXTERNAL      LUHADR
      CHARACTER*12  PRGNAM/'LUND_Frag.  '/
C
C ======< Entry Point >=================================================
C
C
C-----------------------------------------------------------------------
C 1. Define initialization and disgnostic entry.
C-----------------------------------------------------------------------
C
      NPROG = NPROG + 1
      NAMCOM(NPROG) = PRGNAM
      ENTDIA(NPROG) = 0
      ENTSTA(NPROG) = 0
C
C ----------------------------------------------------------------------
C (2) Define Initialization entry.
C ----------------------------------------------------------------------
C
      CALL EXTSUB ( ENTDIA(NPROG), FLNPAR )
      CALL EXTSUB ( ENTBRU(NPROG), FLNBRU )
      CALL EXTSUB ( ENTERU(NPROG), FLNERU )
      ENTINI(NPROG) = 0
      ENTTER(NPROG) = 0
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),LUHADR)
C
C
C-----------------------------------------------------------------------
C 2.2 Define input and output banks of each event
C-----------------------------------------------------------------------
C
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
 
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Generator:Particle_List'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
 
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Generator:Header'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
C
C-----------------------------------------------------------------------
C 3. Define default calling sequence.
C-----------------------------------------------------------------------
C
      LISTSP(0)     = NENTR
      LISTSP(NENTR) = NENTR
C
C-----------------------------------------------------------------------
C 4. Define Level and Debug operand.
C-----------------------------------------------------------------------
C
      EXELVL(NENTR) = 1
      DBGLVL(NENTR) = 1
C
      RETURN
      END
