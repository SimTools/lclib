C***********************************************************************
C*
C* -----------------
C* Subroutine DSGELS
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
C*   A. Miyamoto    21-Sep-1994  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DSGELS
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
C
C
C FUNCTIONS
C
#ifdef XX
     EXTERNAL      FLNPAR
#endif 
#ifdef XX
     EXTERNAL      SPJBIN
#endif 
#ifdef C
     EXTERNAL      FLNBRU
#endif 
#ifdef C
     EXTERNAL      FLNERU
#endif 
#ifdef X
   EXTERNAL      SPJBTM
#endif 
      EXTERNAL      GENELS
      CHARACTER*12  PRGNAM/'GENE_LIST   '/
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
C      CALL EXTSUB ( ENTDIA(NPROG), FLNPAR )
C      CALL EXTSUB ( ENTBRU(NPROG), FLNBRU )
C      CALL EXTSUB ( ENTERU(NPROG), FLNERU )
      ENTDIA(NPROG) = 0
      ENTBRU(NPROG) = 0
      ENTERU(NPROG) = 0
      ENTINI(NPROG) = 0
      ENTTER(NPROG) = 0
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),GENELS)
C
C
C-----------------------------------------------------------------------
C 2.2 Define input and output banks of each event
C-----------------------------------------------------------------------
C
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
 
C      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
C      OUBNAM(NENTR,OUTBLK(NENTR)) =
C     #   'Generator:Particle_List'
C      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
 
C      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
C      OUBNAM(NENTR,OUTBLK(NENTR)) =
C     #   'Generator:Header'
C      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
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
