CC**********************************************************************
C*
C*====================----==
C*  Subrouitne GAMTRK(IRET)
C*====================----==
C*
C* (Purpose)
C*     Output gamma track candidates.
C* (Inputs)
C* (Outputs)
C*     IRET   ; return flag.
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*    93/05/10  K. Fujii               Original version.
C*
CC**********************************************************************
 
      SUBROUTINE GAMTRK(IRET)
 
#include "prjunk.inc"
#include "cmbcut.inc"
#include "smrres.inc"
      INTEGER*4 IRET
      PARAMETER ( LNxLTK = 12  )
      COMMON /SSWORK/ IELM(MXxTRK), RDATA(LNxTRK,MXxTRK),
     .                ICEL(100), DCEL(2,100), RLTKBF(1000),
     .                KCN(MXxTRK), ETK(4,MXxTRK), IETK(50,MXxTRK)
      REAL   *4       XP(3), DX(3)
      CHARACTER*64 BKNAM1 / 'Production:EMC;Cluster'           /
      CHARACTER*64 BKNAM2 / 'Production:Combined_Gamma_Track' /
C
C==================< Entry Point >======================================
C
C--
C  Reset return flag.
C--
      IRET = 1
C--
C  TBGET EMC clusters.
C--
      CALL TBNOEL(1,BKNAM1,NE,IELM)
      IF ( NE.LE.0 )                             GO TO 9999
      IF ( NE.GT.MXxTRK ) THEN
         print *,'Warning in Sub. GAMTRK of LTKCL..',
     >  'Number of EMC Cluster(',NE,') exceeds ',
     >  'buffer size(',MXxTRK,')'
         print *,'It is trancated to ',MXxTRK
         NE=MXxTRK
      endif
C--
      DO 100 IE = 1, NE
         CALL TBGET(1,BKNAM1,IELM(IE),NW,RDATA(1,IE),IRT)
100   CONTINUE
C--
C  TBCRTE track bank.
C--
      CALL TBCRTE(1,BKNAM2,0,0,IRT)
C--
C  Loop over electron tracks to remove corresponding EMC clusters.
C--
      CALL UVZERO(  MXxTRK,KCN)
      CALL UVZERO(4*MXxTRK,ETK)
      NETK = 0
      DO 2000 ITK = 1, NCDCTK
         IF ( KCNTRK(ITK).NE.11 )                GO TO 2000
         CALL GTCLID(1,RTKBNK(11,ITK),XP,NCL,ICEL,DCEL)
         NETK  = NETK + 1
         ADXMN = 1.E20
         DO 200 ICL = 1, NE
            CALL USUB3(XP,RDATA(5,ICL),DX)
            ADX = UABSV3(DX)
            IF ( ADX.LT.ADXMN ) THEN
               ICLE  = ICL
               ADXMN = ADX
            ENDIF
200      CONTINUE
         IF ( ADXMN.LT.ADXECT ) THEN
            E             = RTKBNK(4,ITK)
            KCN(  ICLE)   = KCN(  ICLE) + 1
            ETK(1,ICLE)   = ETK(1,ICLE) + E*XP(1)
            ETK(2,ICLE)   = ETK(2,ICLE) + E*XP(2)
            ETK(3,ICLE)   = ETK(3,ICLE) + E*XP(3)
            ETK(4,ICLE)   = ETK(4,ICLE) + E
            RTKBNK(8,ITK) = RDATA(4,ICLE)
            RTKBNK(9,ITK) = ICLE
            IETK(KCN(ICLE),ICLE) = NETK
         ENDIF
2000  CONTINUE
C--
C  Output gamma tracks.
C--
      NTRK = 0
      DO 300 ICL = 1, NE
         ECL   = RDATA(4,ICL)
         ETEST = ENSGEM*ECL*SQRT( SGEMB**2/ECL + SGEMB0**2 )
C--
C  Pure gamma.
C--
         IF ( KCN(ICL).EQ.0 ) THEN
            ITYP = 1
            ISRC = 10
            ICHG = 0
            NTRK = NTRK + 1
            CALL UVZERO(LNxLTK,RLTKBF)
            RLTKBF( 1) = RDATA(1,ICL)
            RLTKBF( 2) = RDATA(2,ICL)
            RLTKBF( 3) = RDATA(3,ICL)
            RLTKBF( 4) = RDATA(4,ICL)
            RLTKBF( 7) = ICL
            RLTKBF( 9) = ICHG
            RLTKBF(10) = ITYP
            RLTKBF(11) = ISRC
            CALL TBPUT(1,BKNAM2,NTRK,LNxLTK,RLTKBF,IRT)
C--
C  Possible electron-gamma mixed cluster.
C--
         ELSE IF ( ECL-ETK(4,ICL).GT.ETEST ) THEN
            ITYP  = 2
            ISRC  = 11
            ICHG  = 0
            XP(1) = RDATA(4,ICL)*RDATA(5,ICL) - ETK(1,ICL)
            XP(2) = RDATA(4,ICL)*RDATA(6,ICL) - ETK(2,ICL)
            XP(3) = RDATA(4,ICL)*RDATA(7,ICL) - ETK(3,ICL)
            CALL UNRMV3(XP,XP)
            E     = RDATA(4,ICL) - ETK(4,ICL)
            NTRK  = NTRK + 1
            CALL UVZERO(LNxLTK,RLTKBF)
            RLTKBF( 1) = E*XP(1)
            RLTKBF( 2) = E*XP(2)
            RLTKBF( 3) = E*XP(3)
            RLTKBF( 4) = E
            RLTKBF( 7) = ICL
            RLTKBF( 8) = ICL
            RLTKBF( 9) = ICHG
            RLTKBF(10) = ITYP
            RLTKBF(11) = ISRC
            RLTKBF(12) = KCN(ICL)
            DO 30 JTK = 1, KCN(ICL)
               RLTKBF(12+JTK) = IETK(JTK,ICL)
30          CONTINUE
            CALL TBPUT(1,BKNAM2,NTRK,LNxLTK+KCN(ICL),RLTKBF,IRT)
         ENDIF
300   CONTINUE
C--
C  That's it.
C--
      IRET = 0
9999  RETURN
      END
