C***********************************************************************
C*
C* -----------------
C* Subroutine DSSQUZ
C* -----------------
C*
C*(Function)
C*   Select lepton-hadron mode of e+e- --> W+W-
C*
C*(Input & Output)
C*   None.
C*
C*(Relation)
C*   CALLed from FLANLI
C*
C*(Author)
C*   A. Miyamoto   23-Apr-1987  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DSSQUZ
C
C=EXPAND 'T#PR.FULL.FORT/FLDSCR.inc'
C=EXPAND 'T#PR.FULL.FORT/FLLEVL.inc'
C=EXPAND 'T#PR.P#STER.FORT/PRJFLG.inc'
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
C FUNCTIONS
C
      EXTERNAL      SQUZEV
      CHARACTER*12  PRGNAM/'SQUEZE_EVT'/
C
C ======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Define program name.
C ----------------------------------------------------------------------
C
      NPROG = NPROG + 1
      NAMCOM(NPROG) = PRGNAM
      ENTDIA(NPROG) = 0
      ENTSTA(NPROG) = 0
 
      ENTINI(NPROG) = 0
      ENTTER(NPROG) = 0
      ENTBRU(NPROG) = 0
      ENTERU(NPROG) = 0
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),SQUZEV)
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
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
      NJBLVL(20,1)  = 1
      EXELVL(NENTR) = NJBLVL(20,1)
      DBGLVL(NENTR) = NJBDBG(20,1)
C
      RETURN
      END
