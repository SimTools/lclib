CC**********************************************************************
C*
C*===========================
C*  Subrouitne HADTRK(NTRK)
C*===========================
C*
C* (Purpose)
C*     Link cluster to track.
C* (Inputs)
C* (Outputs)
C*     NTRK   ; track number.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*     7/09/92  K. Fujii               Original version.
C*
CC**********************************************************************
 
      SUBROUTINE HADTRK(NTRK)
 
#include "prjunk.inc"
#include "cmbcut.inc"
#include "smrhgo.inc"
#include "smrres.inc"
      INTEGER*4  NTRK
      PARAMETER ( LNxLTK = 12, MXxLTK = 1000 )
      COMMON /SSOWRK/ RLTKBF(1000), ITKCEL(MXxLTK), ETK(4), XP(3)
      CHARACTER*64 BKNAME / 'Production:Combined_Hadron_Track' /
C     DATA AMKL  / 0.49767 /
C
C==================< Entry Point >======================================
C
C--
C  TBCRTE track bank.
C--
      IF ( NTRK.EQ.0 ) THEN
         CALL TBCRTE(1,BKNAME,0,0,IRT)
      ENDIF
C--
C  Loop over cells to find associated tracks.
C--
      NTKCEL = 0
      ECL    = 0
      CALL UVZERO(4,ETK)
      DO 1000 JFI = 2, 2*IWORK(1), 2
         IPT  = IWORK(JFI+1)
         NCL  = IWORK(IPT)
         DO 100 ICL = 1, NCL
            ECL   = ECL + IWORK(IPT+1)*1.E-3
            IDCEL = IWORK(IPT+2)
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
            JTKBN = KBNPNT(IZT,IFI)
            IF ( JTKBN.GT.0 ) THEN
               DO 10 JTK = 1, NTKIN(JTKBN)
                  ITK  = ITKPNT(JTK,JTKBN)
                  IF ( KCNTRK(ITK).EQ.0 ) THEN
                     ECH            = RTKBNK(4,ITK)
                     KCNTRK(ITK)    = 1
                     ETK(1)         = ETK(1) + ECH*RTKBNK(61,ITK)
                     ETK(2)         = ETK(2) + ECH*RTKBNK(62,ITK)
                     ETK(3)         = ETK(3) + ECH*RTKBNK(63,ITK)
                     ETK(4)         = ETK(4) + ECH
                     NTKCEL         = NTKCEL + 1
                     ITKCEL(NTKCEL) = ITK
                  ENDIF
10             CONTINUE
            ENDIF
            IPT = IPT + 3
100      CONTINUE
1000  CONTINUE
C--
C  Define matching criterion.
C--
      ETEST = ENSGHD*ECL*SQRT( SGHDB**2/ECL + SGHDB0**2 )
C--
C  Purely neutral cluster.
C--
      IF ( NTKCEL.EQ.0 ) THEN
C>>>
#ifdef P
         PRINT *, ' Purely neutral cluster '
#endif 
C>>>
         ESM = 0
         PHI = 0
         ZET = 0
         DO 2000 JFI = 2, 2*IWORK(1), 2
            JPHI = IWORK(JFI  )
            IPT  = IWORK(JFI+1)
            NCL  = IWORK(IPT)
            DO 200 ICL = 1, NCL
               E     = IWORK(IPT+1)*1.E-3
               IDCEL = IWORK(IPT+2)
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
               PHI   = PHI + (JPHI+0.5)*E
               ZET   = ZET + (IZT +0.5)*E
               ESM   = ESM + E
               IPT = IPT + 3
200         CONTINUE
2000     CONTINUE
         ZET = ZET/ESM
         PHI = (PHI/ESM)*DPHIHD
         IF ( ZET.LT.NRADHD ) THEN
            ZET = ZET*DRDPHD + RPMNHD
            ZZ  = EXP(2*ZET)
            SN  = (ZZ-1)/(ZZ+1)
            CS  = SQRT((1-SN)*(1+SN))
         ELSE IF ( ZET.LT.NTHTHD+NRADHD  ) THEN
            ZET = (ZET-NRADHD)*DTHTHD + THMNHD
            ZZ  = EXP(2*ZET)
            CS  =-(ZZ-1)/(ZZ+1)
            SN  = SQRT((1-CS)*(1+CS))
         ELSE
            ZET = 2*NRADHD + NTHTHD - ZET
            ZET = ZET*DRDMHD + RMMNHD
            ZZ  = EXP(2*ZET)
            SN  = (ZZ-1)/(ZZ+1)
            CS  =-SQRT((1-SN)*(1+SN))
         ENDIF
         CSF        = COS(PHI)
         SNF        = SIN(PHI)
C--
         ICHG       = 0
         ITYP       = 3
         ISRC       = 100
C--
         NTRK       = NTRK + 1
         CALL UVZERO(LNxLTK,RLTKBF)
         RLTKBF( 1) = ESM*SN*CSF
         RLTKBF( 2) = ESM*SN*SNF
         RLTKBF( 3) = ESM*CS
         RLTKBF( 4) = ESM
         RLTKBF( 9) = ICHG
         RLTKBF(10) = ITYP
         RLTKBF(11) = ISRC
         CALL TBPUT(1,BKNAME,NTRK,LNxLTK,RLTKBF,IRT)
C--
C  Mixed cluster.
C--
      ELSE IF ( ECL-ETK(4).GT.ETEST ) THEN
C>>>
#ifdef P
         PRINT *, ' Mixed cluster. '
#endif 
C>>>
         DO 3000 JTK = 1, NTKCEL
            ITK  = ITKCEL(JTK)
C--
            ICHG       = SIGN(1,RTKBNK(13,ITK))
            ITYP       = 4
            ISRC       = 1
C--
            NTRK = NTRK + 1
            CALL UVZERO(LNxLTK,RLTKBF)
            RLTKBF( 1) = RTKBNK(1,ITK)
            RLTKBF( 2) = RTKBNK(2,ITK)
            RLTKBF( 3) = RTKBNK(3,ITK)
            RLTKBF( 4) = RTKBNK(4,ITK)
            RLTKBF( 8) = ITKBNK(57,ITK)
            RLTKBF( 9) = ICHG
            RLTKBF(10) = ITYP
            RLTKBF(11) = ISRC
            RLTKBF(12) = 1
            RLTKBF(13) = ITK
            CALL TBPUT(1,BKNAME,NTRK,LNxLTK+1,RLTKBF,IRT)
3000     CONTINUE
         ESM = 0
         PHI = 0
         ZET = 0
         DO 4000 JFI = 2, 2*IWORK(1), 2
            JPHI = IWORK(JFI  )
            IPT  = IWORK(JFI+1)
            NCL  = IWORK(IPT)
            DO 400 ICL = 1, NCL
               E     = IWORK(IPT+1)*1.E-3
               IDCEL = IWORK(IPT+2)
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
               PHI   = PHI + (JPHI+0.5)*E
               ZET   = ZET + (IZT +0.5)*E
               ESM   = ESM + E
               IPT = IPT + 3
400         CONTINUE
4000     CONTINUE
         ZET = ZET/ESM
         PHI = (PHI/ESM)*DPHIHD
         IF ( ZET.LT.NRADHD ) THEN
            ZET = ZET*DRDPHD + RPMNHD
            ZZ  = EXP(2*ZET)
            SN  = (ZZ-1)/(ZZ+1)
            CS  = SQRT((1-SN)*(1+SN))
            RG  = ZCYHD(2)/CS
         ELSE IF ( ZET.LT.NTHTHD+NRADHD  ) THEN
            ZET = (ZET-NRADHD)*DTHTHD + THMNHD
            ZZ  = EXP(2*ZET)
            CS  =-(ZZ-1)/(ZZ+1)
            SN  = SQRT((1-CS)*(1+CS))
            RG  = RCYHD(2)/SN
         ELSE
            ZET = 2*NRADHD + NTHTHD - ZET
            ZET = ZET*DRDMHD + RMMNHD
            ZZ  = EXP(2*ZET)
            SN  = (ZZ-1)/(ZZ+1)
            CS  =-SQRT((1-SN)*(1+SN))
            RG  = ZCYHD(1)/CS
         ENDIF
         CSF        = COS(PHI)
         SNF        = SIN(PHI)
C--
         ICHG       = 0
         ITYP       = 4
         ISRC       = 101
C--
         XP(1)      = ESM*RG*SN*CSF - ETK(1)
         XP(2)      = ESM*RG*SN*SNF - ETK(2)
         XP(3)      = ESM*RG*CS     - ETK(3)
         ENUT       = ESM           - ETK(4)
         CALL UNRMV3(XP,XP)
C--
         NTRK       = NTRK + 1
         CALL UVZERO(LNxLTK,RLTKBF)
         RLTKBF( 1) = ENUT*XP(1)
         RLTKBF( 2) = ENUT*XP(2)
         RLTKBF( 3) = ENUT*XP(3)
         RLTKBF( 4) = ENUT
         RLTKBF( 9) = ICHG
         RLTKBF(10) = ITYP
         RLTKBF(11) = ISRC
         RLTKBF(12) = NTKCEL
         DO 50 JTK = 1, NTKCEL
            RLTKBF(12+JTK) = ITKCEL(JTK)
50       CONTINUE
         CALL TBPUT(1,BKNAME,NTRK,LNxLTK+NTKCEL,RLTKBF,IRT)
C--
C  Purely charged cluster.
C--
      ELSE
C>>>
#ifdef P
         PRINT *, ' Pure charged cluster.'
#endif 
C>>>
         DO 6000 JTK = 1, NTKCEL
            ITK  = ITKCEL(JTK)
C--
            ICHG = SIGN(1,RTKBNK(13,ITK))
            ITYP = 5
            ISRC = 1
C--
            NTRK = NTRK + 1
            CALL UVZERO(LNxLTK,RLTKBF)
            RLTKBF( 1) = RTKBNK(1,ITK)
            RLTKBF( 2) = RTKBNK(2,ITK)
            RLTKBF( 3) = RTKBNK(3,ITK)
            RLTKBF( 4) = RTKBNK(4,ITK)
            RLTKBF( 8) = ITKBNK(57,ITK)
            RLTKBF( 9) = ICHG
            RLTKBF(10) = ITYP
            RLTKBF(11) = ISRC
            RLTKBF(12) = 1
            RLTKBF(13) = ITK
            CALL TBPUT(1,BKNAME,NTRK,LNxLTK+1,RLTKBF,IRT)
6000     CONTINUE
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
