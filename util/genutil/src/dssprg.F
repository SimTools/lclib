C***********************************************************************
C*
C* -----------------
C* Subroutine DSSPRG
C* -----------------
C*
C*(Function)
C*   Describe SPRING module.
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
      SUBROUTINE DSSPRG
C
C =EXPAND 'T#PR.FULL.FORT(@FLDSCR)'
C =EXPAND 'T#PR.FULL.FORT(@FLLEVL)'
C =EXPAND 'T#PR.P#STER.FORT(@PRJFLG)'
C
      INCLUDE 'fldscr.inc'
C  T#FP.TOPAZANL.COM
      INCLUDE 'fllevl.inc'
C  T#FP.TOPAZANL.COM
      INCLUDE 'prjflg.inc'
C  T#FP.TOPAZANL.COM
C
C FUNCTIONS
C
      EXTERNAL      SPGPAR
CXX      EXTERNAL      SPJBIN
      EXTERNAL      SPRNIN
      EXTERNAL      SPJBTM
      EXTERNAL      SPCALL
      CHARACTER*12  PRGNAM/'SPRING: '/
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
      CALL EXTSUB ( ENTDIA(NPROG), SPGPAR )
CXX      ENTBRU(NPROG) = 0
      CALL EXTSUB ( ENTBRU(NPROG), SPRNIN )
      ENTERU(NPROG) = 0
      ENTINI(NPROG) = 0
CXX      CALL EXTSUB ( ENTINI(NPROG), SPJBIN )
      CALL EXTSUB ( ENTTER(NPROG), SPJBTM )
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),SPCALL)
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
     >   'SPRING:Parton_List'
      OUBSIZ(NENTR,OUTBLK(NENTR)) =          0
 
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) =
     >   'SPRING:Header'
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
