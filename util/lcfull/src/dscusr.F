C***********************************************************************
C*
C* -----------------
C* Subroutine DSCUSR
C* -----------------
C*
C*(Function)
C*   Describe User routine.
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
      SUBROUTINE DSCUSR
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
C FUNCTIONS
C
      EXTERNAL      PRUCOM
      EXTERNAL      PRUJIN
      EXTERNAL      PRUJTM
      EXTERNAL      PRURIN
      EXTERNAL      PRURTM
      EXTERNAL      PRUSER
      CHARACTER*12  PRGNAM/'USER:      '/
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
 
      CALL EXTSUB (ENTDIA(NPROG),PRUCOM)
      CALL EXTSUB (ENTINI(NPROG),PRUJIN)
      CALL EXTSUB (ENTTER(NPROG),PRUJTM)
      CALL EXTSUB (ENTBRU(NPROG),PRURIN)
      CALL EXTSUB (ENTERU(NPROG),PRURTM)
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),PRUSER)
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
      EXELVL(NENTR) = NJBLVL(20,1)
      DBGLVL(NENTR) = NJBDBG(20,1)
C
      RETURN
      END
