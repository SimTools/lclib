CC**********************************************************************
C*
C*===========================
C*  Subrouitne GTCDTK(IRET)
C*===========================
C*
C* (Purpose)
C*     Get CDC tracks and calculate expected hit points.
C* (Inputs)
C* (Outputs)
C*     IRET   ; return flag.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*     7/09/92  K. Fujii               Original version.
C*
CC**********************************************************************
 
      SUBROUTINE GTCDTK(IRET)
 
#include "prjunk.inc"
#include "smrhgo.inc"
C--
      COMMON /SSWORK/ IELM(MXxTRK), XP(3), ICEL(100), DCEL(2,100)
      CHARACTER*64 BKNAME / 'Production:CDC;Track_Parameter' /
      DATA  EPSL      / 0.1 /
C
C==================< Entry Point >======================================
C
C--
C  Reset return flag.
C--
      IRET = 1
C--
C  Reset track counters.
C--
      NCDCTK = 0
      CALL UVZERO(MXxBD,KBNPNT)
      CALL UVZERO(MXxBD,NTKIN )
      CALL UVZERO(MXxTRK,KCNTRK)
C--
C  Check # CDC tracks.
C--
      CALL TBNOEL(1,BKNAME,NE,IELM)
      IF ( NE.LE.0 )                             GO TO 9999
      IF ( NE.GT.MXxTRK ) THEN
         print *,'Warning in Sub. GTCDTK of LTKCL..',
     >  'Number of CDC Track(',NE,') exceeds ',
     >  'buffer size(',MXxTRK,')'
         print *,'It is trancated to ',MXxTRK
         NE=MXxTRK
      endif
C--
C  Loop over tracks.
C--
      NCDCTK = NE
      NTKBN  = 0
      DO 1000 IE = 1, NE
         CALL TBGET(1,BKNAME,IELM(IE),NW,RTKBNK(1,IE),IRT)
         CALL GTCLID(2,RTKBNK(11,IE),XP,NCL,ICEL,DCEL)
         ITKBNK(60,IE) = NCL
         RTKBNK(61,IE) = XP(1)
         RTKBNK(62,IE) = XP(2)
         RTKBNK(63,IE) = XP(3)
         DO 100 ICL = 1, NCL
            IOFF = 63 + 3*(ICL-1)
            ITKBNK(IOFF+1,IE) = ICEL(ICL)
            RTKBNK(IOFF+2,IE) = DCEL(1,ICL)
            RTKBNK(IOFF+3,IE) = DCEL(2,ICL)
C--
            IDCEL = ICEL(ICL)
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
            IF ( KBNPNT(IZT,IFI).EQ.0 ) THEN
               NTKBN = NTKBN + 1
               KBNPNT(IZT,IFI) = NTKBN
               JTKBN = NTKBN
            ELSE
               JTKBN = KBNPNT(IZT,IFI)
            ENDIF
            NTKIN(JTKBN)    = NTKIN(JTKBN) + 1
            ITKPNT(NTKIN(JTKBN),JTKBN) = IE
100      CONTINUE
1000  CONTINUE
C--
C  That's it.
C--
      IRET = 0
9999  RETURN
      END
