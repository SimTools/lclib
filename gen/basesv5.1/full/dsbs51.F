C***************************************************************
C*
C*  DESCRIBE for BASES 5.1
C*  
      SUBROUTINE DSBS51
C* 
C*  Author A.Miyamoto  18-Oct-1994
C*
C****************************************************************
C
C COMMON
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
C
C FUNCTIONS
C
      EXTERNAL      BS51CM
      EXTERNAL      BS51JI
C      EXTERNAL      BS51JT
      EXTERNAL      BS51RI
      EXTERNAL      BS51RT
      EXTERNAL      BS51EV
C
C MAIN
C
C Subprogram Definition
C
      NPROG = NPROG + 1
      NAMCOM(NPROG) = 'BASES_V5.1'
 
      CALL EXTSUB (ENTDIA(NPROG),BS51CM)
      CALL EXTSUB (ENTINI(NPROG),BS51JI)
C      CALL EXTSUB (ENTTER(NPROG),BS51JT)
      ENTTER(NPROG) = 0
      ENTSTA(NPROG) = 0
      CALL EXTSUB (ENTBRU(NPROG),BS51RI)
      CALL EXTSUB (ENTERU(NPROG),BS51RT)
C
C 2. Event Analysis routine
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = NAMCOM(NPROG)
      CALL EXTSUB (ENTEVT(NENTR),BS51EV)
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Spring:Header'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Spring:Parton_List'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
C
C
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
      DBGLVL(NENTR) = 0
C
      RETURN
      END
