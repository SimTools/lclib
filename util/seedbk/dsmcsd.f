C***********************************************************************
C*
C* -----------------
C* Subroutine DSMCSD
C* -----------------
C*
C*(Function)
C*   Describe MC seed handling.
C*
C*(Input & Output)
C*   None.
C*
C*(Relation)
C*   CALLed from FLANLI
C*
C*(Update Record)
C*   95/08/31  K.Fujii		Original version.
C*
C***********************************************************************

      SUBROUTINE DSMCSD

      INCLUDE 'FLDSCR.inc'
      INCLUDE 'FLLEVL.inc'
      INCLUDE 'PRJFLG.inc'
C
C FUNCTIONS
C
      EXTERNAL      MCSDPR
      EXTERNAL      MCSDRI
      EXTERNAL      MCSDRT
      EXTERNAL      MCSEED
      CHARACTER*12  PRGNAM/'Seed_Booker '/
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
      CALL EXTSUB ( ENTDIA(NPROG), MCSDPR )
      CALL EXTSUB ( ENTBRU(NPROG), MCSDRI )
      CALL EXTSUB ( ENTERU(NPROG), MCSDRT )
      ENTINI(NPROG) = 0
      ENTTER(NPROG) = 0
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),MCSEED)
C
C
C-----------------------------------------------------------------------
C 2.2 Define input and output banks of each event
C-----------------------------------------------------------------------
C
      INPBLK(NENTR) = 0
      OUTBLK(NENTR) = 0
 
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'SPRING:Seed_Now'
      OUBSIZ(NENTR,OUTBLK(NENTR)) = 0
 
      OUTBLK(NENTR) = OUTBLK(NENTR) + 1
      OUBNAM(NENTR,OUTBLK(NENTR)) = 'LUND73:Seed_Now'
      OUBSIZ(NENTR,OUTBLK(NENTR)) = 0
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
