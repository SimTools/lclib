C***********************************************************************
C*
C*================================----===
C* Subroutine SMRCVT(LEVEL,IDEBUG,IRET)
C*================================----===
C*
C* (Purpose)
C*     Create Production:CDC_VTX;Track_Parameter.
C* (Inputs)
C*        LEVEL    : (I*4) :  level flag.
C*        IDEBUG   : (I*4) :  debug flag.
C* (Output)
C*        IRET     : (I*4) :  return flag.
C* (Relation)
C*     Called by SMREVT.
C*     Calls the following routines;
C* (Updaterecord)
C*     3/19/93  K.Fujii           Temporary version using track to
C*                                particle pointers.
C*    12/24/98  A.Miyamoto        Use only first and last VTX hit points
C*                                for combined track fit.
C*
C***********************************************************************
 
      SUBROUTINE SMRCVT(LEVEL,IDEBUG,IRET)
 
#include "smrvgo.inc"
      PARAMETER     ( LNxTRK = 100, MXxCDT = 500, MXxVHT = 20,
     .                LNxHLX = 52 )
      COMMON /SSWORK/ RCDTK(LNxTRK), RVTHT(3,MXxVHT), RCVTK(LNxTRK),
     .                XVHT(2,3,MXxVHT), IELM(MXxCDT)
      REAL   *8       HELX8(8)
      INTEGER*4       ICDTK(LNxTRK), ICVTK(LNxTRK)
      EQUIVALENCE    (ICDTK,RCDTK), (ICVTK,RCVTK)
      CHARACTER*64    BKNAM1 / 'Production:CDC;Track_Parameter'     /
      CHARACTER*64    BKNAM2 / 'Production:VTX;Space_Point'         /
      CHARACTER*64    BKNAM3 / 'Production:CDC_VTX;Track_Parameter' /
      PARAMETER     ( LNxCHT = 57 )
C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Check # CDC tracks.
C--
      CALL TBNOEL(1,BKNAM1,NE,IELM)
      IF ( NE.GT.MXxCDT ) THEN
         PRINT *, '>>>>>> Error in SMRCVT >>>>>>'
         PRINT *, '   MXxCDT < NE = ', NE
         PRINT *, '   STOP.'
         STOP
      ENDIF
C--
C  Loop over CDC tracks.
C--
      DO 2000 IE = 1, NE
         ITK = IELM(IE)
         CALL TBGET(1,BKNAM1,ITK,NW,RCDTK,IRT)
         IF ( IRT.LT.0 ) THEN
            IRET = IRT
            PRINT *, ' SMRCVT failed to TBGET el#', ITK, ' of ', BKNAM1
            PRINT *, '   IRT = ', IRT
            RETURN
         ENDIF
         NPT  = ICDTK(56)
         IF ( NPT.NE.1 ) THEN
            PRINT *, '>>>>>> Error in SMRCVT >>>>>>'
            PRINT *, '   More than 1 particle contributes to ',
     .               'track #', ITK
            PRINT *, '   Cannot be handled with the presnt SMRCVT.'
            PRINT *, '   Stop.'
            STOP
         ENDIF
C--
C  Get corresponding VTX hits.
C--
         IPT  = ICDTK(57)
         CALL TBGET(1,BKNAM2,IPT,NW,RVTHT,IRT)
         IF ( IRT.LT.0 ) NW = 0
         NNVHT = NW/3
C--
c         IF ( NVHT.GT.2 ) THEN
c            PRINT *, '>>>>>> Error in SMRCVT >>>>>>'
c            PRINT *, '   More than 2 VTX hits exist for track #', ITK
c            PRINT *, '   Cannot be handled with the presnt SMRCVT.'
C           PRINT *, '   Stop.'
C           STOP
c         ENDIF
C--
C  Branch on # VTX hits.
C--
          NVHT = 2
C--  When VTX Hit is more than 2, select only first and last hits for
C--  Combined fitting.
         IF ( NVHT.EQ.2 .AND. NNVHT .GE. 2 ) THEN
C--
C  Set XVHT.
C--
            DO 10 IVHT = 1, NNVHT, (NNVHT-1)
               R   = RVTHT(1,IVHT)
               PHI = RVTHT(2,IVHT)
               Z   = RVTHT(3,IVHT)
               CS  = COS(PHI)
               SN  = SIN(PHI)
               XVHT(1,1,IVHT) = R*CS
               XVHT(1,2,IVHT) = R*SN
               XVHT(1,3,IVHT) = Z
               XVHT(2,1,IVHT) = R*DPHIVX*ABS(SN)
               XVHT(2,2,IVHT) = R*DPHIVX*ABS(CS)
               XVHT(2,3,IVHT) = DZEEVX
10          CONTINUE

C--
C  Perform combined fit.
C--
            CALL ULCVTK(LNxHLX-11,RCDTK(11),NVHT,XVHT,RCVTK(11),IRT)
            IF ( IRT.LT.0 ) THEN
               ICND = -1
               CALL UVCOPY(LNxHLX,RCDTK,RCVTK)
            ELSE
               ICND = 2
               DO 20 I = 1, 8
                  HELX8(I) = RCVTK(I+10)
20             CONTINUE
               CALL UFDCAP(HELX8(1),RCVTK(5),RCVTK(1))
               RCVTK(4) = SQRT(RCVTK(1)**2+RCVTK(2)**2+RCVTK(3)**2)
               ICVTK(9) = SIGN(1.,RCVTK(13))
            ENDIF
         ELSE
            ICND = NVHT
            CALL UVCOPY(LNxHLX,RCDTK,RCVTK)
         ENDIF
C--
C  TBPUT combined tracks.
C--
         ICVTK(53) = ICND
         ICVTK(54) = ITK
         ICVTK(55) = IPT
         NW        = 55
         CALL TBPUT(1,BKNAM3,ITK,NW,RCVTK,IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, 'SMRCVT failed to TBPUT el#', ITK, ' of ',BKNAM3
            PRINT *, '    IRT = ', IRT
            IRET = IRT
            RETURN
         ENDIF
2000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
