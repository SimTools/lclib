      SUBROUTINE TAUDEC(KTO,NPNT,HELTAU,P4TAU)

      INTEGER*4  KTO, NPNT
      REAL*4     HELTAU, P4TAU(4)
C--
      REAL*4     POL(4)
C
C====================< Entry Point >====================================
C
C--
C  Reset NHEP in /HEPEVT/.
C--
      CALL RESLU
C--
C  Fill /HEPEVT/ for the tau.
C--
      CALL TAUFIL(KTO,P4TAU)
C--
C  Decay the tau. 
C     KTO = 1 : tau+
C         = 2 : tau-
C--
      AP     = SQRT( P4TAU(1)**2 + P4TAU(2)**2 + P4TAU(3)**2 )
      POL(1) = HELTAU*P4TAU(1)/AP
      POL(2) = HELTAU*P4TAU(2)/AP
      POL(3) = HELTAU*P4TAU(3)/AP
C--
      CALL DEXAY(KTO,POL)
C--
C  Pack /HEPEVT/.
C--
      CALL PACHEP(KTO,NPNT)
C--
C  That's it.
C--
      RETURN
      END
