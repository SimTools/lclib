CC**********************************************************************
C*
C*==========================
C*  Subrouitne HSTCEL(IRET)
C*==========================
C*
C* (Purpose)
C*     Make a 2-dim. histogram of hit calorimeter cells.
C* (Inputs)
C* (Outputs)
C*     IRET   ; return flag.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*     7/09/92  K. Fujii  Original version.
C*
CC**********************************************************************
 
      SUBROUTINE HSTCEL(IRET)
 
#include "prjunk.inc"
#include "smrhgo.inc"
C--
      PARAMETER  ( LNxBUF = 2000, MXxCEL = 5000 )
      COMMON /SSWORK/ IBUF(LNxBUF), IELM(MXxCEL), IPNT(MXxCEL)
      INTEGER*4       IBUF, IELM
      INTEGER*4       IPNT
C--
      CHARACTER*64 BKNAME / 'Production:HDC;Hit_Cell' /
C
C==================< Entry Point >======================================
C
C--
C  Reset cluster counters.
C--
      NBNON  = 0
      NCELLS = 0
      CALL UVZERO(MXxBD,JBNPNT)
      CALL UVZERO(MXxBD,NCLIN )
C--
C  Check # cells in this calorimeter.
C--
      CALL TBNOEL(1,BKNAME,NE,IELM)
      IF ( NE.LE.0 )                             GO TO 9999
      IF ( NE.GT.LNxBUF ) THEN
         print *,'Warning in Sub. HSTCEL of LTKCL..',
     >  'Number of HDC Hit cell(',NE,') exceeds ',
     >  'buffer size(',LNxBUF,')'
         print *,'It is trancated to ',LNxBUF
         NE=LNxBUF
      endif
C--
C  Loop over cells.
C--
C>>>
#ifdef CP
        PRINT *, ' ----- Dump ICLPNT ---- '
#endif 
C>>>
      DO 1000 IE = 1, NE
         CALL TBGET(1,BKNAME,IELM(IE),NW,IBUF,IRT)
         IENGY = IBUF(1)
C>>>
C        IF ( IENGY.LE.100 )                     GO TO 1000
C>>>
         IDCEL = IBUF(2)
         IDCLA = IABS(IDCEL)
         IZE   = IDCEL/1000000
         ITH   = MOD(IDCLA/1000,1000)
         IFI   = MOD(IDCLA,1000)
         IF ( IZE.EQ.-1 ) THEN
            IZT   = (NRADHD-ITH-1) + NTHTHD + NRADHD
         ELSE IF ( IZE.EQ.0 ) THEN
            IZT   = ITH + NRADHD
         ELSE
            IZT   = ITH
         ENDIF
         NCELLS = NCELLS + 1
         ICLPNT(1,NCELLS) = IENGY
         ICLPNT(2,NCELLS) = IDCEL
         ICLPNT(3,NCELLS) = IZT
         ICLPNT(4,NCELLS) = IFI
         ICLPNT(5,NCELLS) = IELM(IE)
C>>>
#ifdef CP
        PRINT *, ' ICEL = ', NCELLS
#endif 
#ifdef CP
        PRINT *, ' ICLPNT(1,ICEL) = ', ICLPNT(1,NCELLS)
#endif 
#ifdef CP
        PRINT *, '       (2,ICEL) = ', ICLPNT(2,NCELLS)
#endif 
#ifdef CP
        PRINT *, '       (3,ICEL) = ', ICLPNT(3,NCELLS)
#endif 
#ifdef CP
        PRINT *, '       (4,ICEL) = ', ICLPNT(4,NCELLS)
#endif 
#ifdef CP
        PRINT *, '       (5,ICEL) = ', ICLPNT(5,NCELLS)
#endif 
C>>>
1000  CONTINUE
C--
C  Sort bin data.
C--
      NREC = NCELLS
      CALL USORTI(NREC,5,4,ICLPNT,IPNT)
C--
C  Fill the cells in the zeta-phi histogram.
C--
      DO 2000 JCEL = 1, NCELLS
         ICEL            = IPNT(JCEL)
         IZT             = ICLPNT(3,ICEL)
         IFI             = ICLPNT(4,ICEL)
         JBNPNT(IZT,IFI) = JCEL
         IBNPNT( 1,JCEL) = IZT
         IBNPNT( 2,JCEL) = IFI
         NCLIN(JCEL)     = ICEL
2000  CONTINUE
      NBNON = NCELLS
C--
C  That's it. Now set return flag and return.
C--
      IRET = 0
      RETURN
C--
9999  IRET = 1
      RETURN
      END
