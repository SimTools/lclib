C   20/01/87 210112155  MEMBER NAME  DSGLUN   (FORT)     M  FORTRAN
C+
C File : DESCRIBED....
C Description : Description file for FULL
C
C Author : PROGRAM "DESCRIBE"
C-
C
      SUBROUTINE DSGPYT
C
C     For PYTHIA monte Calro.
C     A.Miyaoto 9-Nov-1989  Modified to set defalt value to
C                        LISTSP , EXELVL, and DBGLVL
C
C COMMON
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
C
C VARIABLES
C
C      INTEGER * 4      I, J
C
C FUNCTIONS
C
      EXTERNAL      P55COM
      EXTERNAL      P55INI
      EXTERNAL      P55TRM
      EXTERNAL      P55BRU
      EXTERNAL      P55ERU
      EXTERNAL      P55EVT
C
C MAIN
C
C Subprogram Definition
C
      NPROG = NPROG + 1
      NAMCOM(NPROG) = 'PYTHIA_55'
      CALL EXTSUB (ENTDIA(NPROG),P55COM)
      CALL EXTSUB (ENTINI(NPROG),P55INI)
      CALL EXTSUB (ENTTER(NPROG),P55TRM)
      ENTSTA(NPROG) = 0
      CALL EXTSUB (ENTBRU(NPROG),P55BRU)
      CALL EXTSUB (ENTERU(NPROG),P55ERU)
C
C 2. Event Analysis routine
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = 'PYTHIA_55   '
      CALL EXTSUB (ENTEVT(NENTR),P55EVT)
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Generator:Header'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Generator:Particle_List'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'Generator:Parton_List'
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
