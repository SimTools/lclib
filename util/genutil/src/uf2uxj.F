C***********************************************************************
C*                                                                     *
C*  -----------------------------------------====                      *
C*  Subroutine UF2UXJ( IDREC, LEVEL, IDEBUG, NRET )                    *
C*  -----------------------------------------====                      *
C*                                                                     *
C*(Function)
C*   Convert bank data to transfer JLC simulator data
C*                                                                     *
C*(Input)                                                              *
C*  IDREC ; record ID, use only ID=1                                   *
C*  LEVEL ; not used.                                                  *
C*  IDEBUG; not used.                                                  *
C*(Output)                                                             *
C*  NRET  ; Return code.                                               *
C*                                                                     *
C*(Author)                                                             *
C*  A.Miyamoto  20-Sep-1994  Original version.                         *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE UF2UXJ( IDREC, LEVEL, IDEBUG, NRET )
C
      PARAMETER (MXDATA=50000, MXDATH=MXDATA/2)
      COMMON /SSWORK/ IHEAD(20), NOELM(500), 
     >       HWLIST(500), G1DATA(MXDATA)
      INTEGER*2           HWLIST
      REAL*8              D1DATA(MXDATH)
      EQUIVALENCE        (G1DATA(1), D1DATA(1))
      CHARACTER*60 BNAME
      CHARACTER*60 BNARAY(2)

      DATA  ILEVT/0/
C
C ==== < Entry Point >==================================================
C
C
      NRET = 0
      IF( IDREC .NE. 1 ) RETURN
C
C  -----------------------------------------------------
C  (1)  Convert the bank "G1"
C  -----------------------------------------------------
C
      BNAME = 'G1'
      CALL TBGETH(1,BNAME,1,NTRK,HWLIST, IR1)
      IF( IR1.LT.0 ) GO TO 120
      IF( NTRK .GT. 500 ) THEN
         PRINT *,'Error in UF2UXJ .. Too many elements in ',BNAME
         STOP
      ENDIF
      CALL TBGET (1,BNAME,2,NW2,G1DATA, IR2)
C
      IF( NW2 .GT. MXDATA ) THEN
         PRINT *,'Error in UF2UXJ .. Too many data  NW2=',NW2,
     >           ' in ',BNAME
         STOP
      ENDIF
C
      IOFS = 0
      DO 100 IW = 1, NTRK
        IT = HWLIST(IW)
        IF( IT.EQ. 8 ) THEN
           CALL TBxR4U(G1DATA(IOFS+2), 7)
        ELSEIF( IT.EQ. 9 ) THEN
           CALL TBxR4U(G1DATA(IOFS+2), 2)
           CALL TBxR4U(G1DATA(IOFS+5), 4)
        ELSEIF( IT.EQ. 11 ) THEN
           CALL TBxR4U(G1DATA(IOFS+2), 10)
        ELSEIF( IT.EQ. 12 ) THEN
           CALL TBxR4U(G1DATA(IOFS+2), 2)
           CALL TBxR4U(G1DATA(IOFS+5), 7)
        ELSEIF( IT.EQ. 14 ) THEN
           CALL TBxR4U(G1DATA(IOFS+2), 2)
           CALL TBxR4U(G1DATA(IOFS+5), 7)
           CALL TBxR4U(G1DATA(IOFS+13), 2)
        ELSE
           PRINT *,'In UF2UXJ .. IT=',IT,' is invalid.'
           STOP
        ENDIF
        IOFS = IOFS+IT
 100  CONTINUE
      CALL TBPUT(1,BNAME,2,NW2,G1DATA,IRET)
 120  CONTINUE
C
C  -----------------------------------------------------
C (2) Convert "Z.Production;CDC_VTX;Track_Parameter"
C  -----------------------------------------------------
C
      BNAME = 'Z.Production:CDC_VTX;Track_Parameter'
      CALL TBGETH(1,BNAME,1,NTRK,HWLIST, IR1)
      IF( IR1.LT.0 ) GO TO 220
      IF( NTRK .GT. 500 ) THEN
         PRINT *,'Error in UF2UXJ .. Too many elements in ',BNAME
         STOP
      ENDIF
      CALL TBGET (1,BNAME,2,NW2,G1DATA, IR2)
      IF( NW2 .GT. MXDATA ) THEN
         PRINT *,'Error in UF2UXJ .. Too many data  NW2=',NW2,
     >           ' in ',BNAME
         STOP
      ENDIF
C
      IOFS = 0
      DO 200 IW = 1, NTRK
        IT = HWLIST(IW)
        CALL TBxR4U(G1DATA(IOFS+ 1), 7)
        CALL TBxR4U(G1DATA(IOFS+ 9), 8)
        IOFS = IOFS + IT
 200  CONTINUE
      CALL TBPUT(1,BNAME,2,NW2,G1DATA,IRET)
 220  CONTINUE
C
C  -----------------------------------------------------
C (3) Convert "Z.Production;EMC;Hit_Cell"
C  -----------------------------------------------------
C
      BNARAY(1) = 'Z.Production:EMC;Hit_Cell'
      BNARAY(2) = 'Z.Production:HDC;Hit_Cell'
  
      DO 300 IB = 1, 2
        BNAME = BNARAY(IB)
        CALL TBGET (1,BNAME,1,NW2,G1DATA, IR2)
        IF( IR2 .LT. 0 ) GO TO 300 
        IF( NW2 .GT. MXDATA ) THEN
           PRINT *,'Error in UF2UXJ .. Too many data  NW2=',NW2,
     >           ' in ',BNAME
          STOP
        ENDIF
C
        DO 310 IW = 1, NW2, 2
          CALL TBxR4U(G1DATA(IOFS+ 1), 1)
 310    CONTINUE
        CALL TBPUT(1,BNAME,1,NW2,G1DATA,IRET)
 300  CONTINUE
C
C  -----------------------------------------------------
C (4) Convert "Z.Production;EMC;Cluster"
C  -----------------------------------------------------
C
      BNARAY(1) = 'Z.Production:EMC;Cluster'
      BNARAY(2) = 'Z.Production:HDC;Cluster'
  
      DO 400 IB = 1, 2
        BNAME = BNARAY(IB)
        CALL TBGETH(1,BNAME,1,NTRK,HWLIST, IR1)
        IF( IR1.LT. 0 ) GO TO 400
        IF( NTRK .GT. 500 ) THEN
          PRINT *,'Error in UF2UXJ .. Too many elements in ',BNAME
          STOP
        ENDIF
        CALL TBGET (1,BNAME,2,NW2,G1DATA, IR2)
        IF( NW2 .GT. MXDATA ) THEN
           PRINT *,'Error in UF2UXJ .. Too many data  NW2=',NW2,
     >           ' in ',BNAME
          STOP
        ENDIF
C
        IOFS = 0
        DO 410 IW = 1, NTRK
          IT = HWLIST(IW)
          CALL TBxR4U(G1DATA(IOFS+ 1), 7)
          IOFS = IOFS + IT
 410    CONTINUE
        CALL TBPUT(1,BNAME,2,NW2,G1DATA,IRET)
 400  CONTINUE
C
C  -----------------------------------------------------
C (5) Convert "Production;VTX;Space_Point"
C  -----------------------------------------------------
C
      BNAME = 'Production;VTX;Space_Point'
      CALL TBNOEL(1, BNAME, NTRK, NOELM )
      IF( NTRK .LE. 0 ) GO TO 590
      IF( NTRK .GT. 500 ) THEN
        PRINT *,'Error in UF2UXJ .. Too many elements in ',BNAME
        STOP
      ENDIF
      DO 510 I = 1, NTRK
        CALL TBGET(1, BNAME, NOELM(I), NW, G1DATA, IRT)
        IF( IR .LT. 0 ) THEN
          PRINT *,'Error in UF2UXJ .. ',
     >            'Error to TBGET ',BNAME,' IRT=',IRT
          STOP
        ENDIF
        CALL TBxR4U(G1DATA, NW )
        CALL TBPUT(1, BNAME, NOELM(I), NW, G1DATA, IRT)
 510  CONTINUE

 590  CONTINUE

C
C  -----------------------------------------------------
C (6) Convert "Production;CDC;Track_Parameter"
C  -----------------------------------------------------
C
      BNARAY(1) = 'Production;CDC;Track_Parameter'
      BNARAY(2) = 'Production;CDC_VTX;Track_Parameter'
      DO 600 IB = 1, 2
         BNAME = BNARAY(IB)
         CALL TBNOEL(1, BNAME, NTRK, NOELM )
         IF( NTRK .LE. 0 ) GO TO 600
         IF( NTRK .GT. 500 ) THEN
           PRINT *,'Error in UF2UXJ .. Too many elements in ',BNAME
           STOP
         ENDIF
         DO 610 I = 1, NTRK
           CALL TBGET(1, BNAME, NOELM(I), NW, G1DATA, IRT)
          IF( IR .LT. 0 ) THEN
            PRINT *,'Error in UF2UXJ .. ',
     >            'Error to TBGET ',BNAME,' IRT=',IRT
            STOP
          ENDIF
          CALL TBxR4U(G1DATA(1),  7 )
          CALL TBxR4U(G1DATA(11), 8 )
          CALL TBxR8U(D1DATA(10), 15 )
          CALL TBxR4U(G1DATA(49), 2 )
          IF( IB.EQ.1 ) CALL TBxR4U(G1DATA(54), 2 )
          CALL TBPUT(1, BNAME, NOELM(I), NW, G1DATA, IRT)
 610    CONTINUE
 600  CONTINUE


C
C  -----------------------------------------------------
C (7) Convert "Production;EMC;Cluster"
C  -----------------------------------------------------
C
      BNARAY(1) = 'Production;EMC;Cluster'
      BNARAY(2) = 'Production;HDC;Cluster'
      DO 700 IB = 1, 2
         BNAME = BNARAY(IB)
         CALL TBNOEL(1, BNAME, NTRK, NOELM )
         IF( NTRK .LE. 0 ) GO TO 700
         IF( NTRK .GT. 500 ) THEN
           PRINT *,'Error in UF2UXJ .. Too many elements in ',BNAME
           STOP
         ENDIF
         DO 710 I = 1, NTRK
           CALL TBGET(1, BNAME, NOELM(I), NW, G1DATA, IRT)
          IF( IR .LT. 0 ) THEN
            PRINT *,'Error in UF2UXJ .. ',
     >            'Error to TBGET ',BNAME,' IRT=',IRT
            STOP
          ENDIF
          CALL TBxR4U(G1DATA(1),  7 )
          CALL TBPUT(1, BNAME, NOELM(I), NW, G1DATA, IRT)
 710    CONTINUE
 700  CONTINUE

      RETURN
      END
C***********************************************************************
C*                                                                     *
C*  -----------------------------------------====                      *
C*  Subroutine UF2UXB( LEVEL, IDEBUG )
C*  -----------------------------------------====                      *
C*                                                                     *
C*(Function)
C*   Convert bank data to transfer JLC simulator data
C*   To convert Begin_Run record.
C*                                                                     *
C*(Input)                                                              *
C*  LEVEL ; not used.                                                  *
C*  IDEBUG; not used.                                                  *
C*                                                                     *
C*(Author)                                                             *
C*  A.Miyamoto  20-Sep-1994  Original version.                         *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE UF2UXB( LEVEL, IDEBUG )
C
      PARAMETER (MXDATA=50000, MXDATH=MXDATA/2)
      COMMON /SSWORK/ IHEAD(20), NOELM(500), 
     >       HWLIST(500), G1DATA(MXDATA)
      INTEGER*2           HWLIST
      REAL*8              D1DATA(MXDATH)
      EQUIVALENCE        (G1DATA(1), D1DATA(1))
      CHARACTER*60 BNAME
      CHARACTER*60 HNAME, LNAME, WORK

      DATA  ILEVT/0/
C
C
C
      CALL GETLOG(LNAME)
      IRET = HOSTNM(HNAME)

      BNAME = 'FULL_ANALYSIS'
      CALL TBNOEL(2, BNAME, NOE, NOELM )
      IF( NOE .LE. 0 ) GO TO 190
      NCONV = 1
      DO 100 IE = 1, NOE
         IED = NOELM(IE)
         CALL TBGET(2, BNAME, IED, NW, G1DATA, IRET)
         IF( IRET .LE. 0 ) THEN
            PRINT *,'Error in UF2UXR .. to TBGET ',BNAME
            STOP
         ENDIF
         IT = MOD(IED,100)
         IF( IT .EQ.1 ) THEN
            WRITE(WORK,'(4A4)') (G1DATA(K),K=14,17)
            IF( WORK(1:8).EQ.LNAME(1:8) .AND. 
     >          WORK(9:16).EQ.HNAME(1:8)) GO TO 190
            CALL UCEBC(16, G1DATA( 3))
            CALL UCEBC(16, G1DATA( 8))
            CALL UCEBC(16, G1DATA(14))
         ELSEIF( IT.EQ.2 ) THEN
            DO 120 I = 1, NW, 7
              CALL UCEBC(12, G1DATA(I))
 120        CONTINUE
         ELSEIF( IT .EQ. 4 ) THEN
            IW = 4*NW
            CALL UCEBC(IW, G1DATA(1))
         ELSE
            GO TO 100
         ENDIF
         CALL TBPUT(2, BNAME, IED, NW, G1DATA, IRET )
 100  CONTINUE
 190  CONTINUE
C
C
      BNAME = 'SPRING'
      CALL TBGET(2, BNAME, 1, NW, G1DATA, IRET)
      IF( IRET .GT. 0 ) THEN
         CALL UCEBC(16, G1DATA(3))
         CALL TBPUT(2, BNAME, 1, NW, G1DATA, IRET)
      ENDIF
      CALL TBGET(2, BNAME, 2, NW, G1DATA, IRET)
      IF( IRET .GT. 0 ) THEN
         CALL TBxR4U(G1DATA(1), NW)
         CALL TBPUT(2, BNAME, 1, NW, G1DATA, IRET)
      ENDIF
C
C
      BNAME = 'SWIMMER'
      CALL TBGET(2, BNAME, 2, NW, G1DATA, IRET)
      IF( IRET .GT. 0 ) THEN
         CALL TBxR4U(G1DATA(1), NW)
         CALL TBPUT(2, BNAME, 2, NW, G1DATA, IRET)
      ENDIF
C
      BNAME = 'SMEARING'
      CALL TBGET(2, BNAME, 2, NW, G1DATA, IRET)
      IF( IRET .GT. 0 ) THEN
         CALL TBxR4U(G1DATA(1), NW)
         CALL TBPUT(2, BNAME, 2, NW, G1DATA, IRET)
      ENDIF
C
      RETURN
      END


C***********************************************************************
C*
C* -----------------
C* Subroutine DSF2UJ
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
      SUBROUTINE DSF2UJ
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
      EXTERNAL      UF2UXB
      EXTERNAL      UF2UXJ
      CHARACTER*12  PRGNAM/'F_TO_UNIX.J'/
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
C      CALL EXTSUB ( ENTERU(NPROG), FLNERU )
      ENTDIA(NPROG) = 0
      ENTBRU(NPROG) = 0
      ENTERU(NPROG) = 0
      ENTINI(NPROG) = 0
      ENTTER(NPROG) = 0
      CALL EXTSUB ( ENTBRU(NPROG), UF2UXB )
C
C-----------------------------------------------------------------------
C 2. Event Analysis routine
C-----------------------------------------------------------------------
C
      NENTR = NENTR + 1
      ENTNAM(NENTR) = PRGNAM
      CALL EXTSUB (ENTEVT(NENTR),UF2UXJ)
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
