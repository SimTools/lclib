CC**********************************************************************
C*
C*============================================-----------==
C*  Subroutine ULCVTK(LNxTRK,TRAKIN,NVHT,XVHT,TRAKOT,IRET)
C*============================================-----------==
C*
C* (Purpose)
C*    Refit input trakc with IP constraint.
C* (Inputs)
C*      LNxTRK      : (I*4) ; length of track parameter ( > 37 ).
C*      TRAKIN(*)   : (R*4) ; helix parameter vector. 1-st 38 words
C*                            should have the same format as that of
C*                            11-th to 48-th words of
C*                            Production:CDC;Track_Parameter.
C*      NVHT        : (I*4) ; # vtx hits.
C*      XVHT(1,i)   : (R*4) ; x of i-th vtx hit.
C*          (2,i)   : (R*4) ; y of i-th vtx hit.
C*          (3,i)   : (R*4) ; Z of i-th vtx hit.
C* (Outputs)
C*      TRAKOT(*)   : (R*4) ; helix parameter vector with vtx hits.
C*                            New pivot is now at the inner vtx hit.
C* (Relation)
C*    Calls : UVZERO, UVCOPY, UMATIN, UTRKMV.
C* (Update Record)
C*    3/12/93  K.Fujii        Temporary version assuming no vtx
C*                            cooridnate errors. This version does
C*                            not fit combined tracks but simply
C*                            use CDC momenta and VTX hits.
C*
CC**********************************************************************
 
      SUBROUTINE ULCVTK(LNxTRK,TRAKIN,NVHT,XVHT,TRAKOT,IRET)
 
      IMPLICIT REAL*8 ( A-H, O-Z )
#include "prfeld.inc"
      INTEGER*4      LNxTRK, NVHT, IRET
      REAL   *4      TRAKIN(LNxTRK), XVHT(2,3,NVHT), TRAKOT(LNxTRK)
      PARAMETER      ( LNxHLX = 38 )
      REAL   *4      HELXIN(LNxHLX), XP(3,2), HELXOT(LNxHLX)
      REAL   *8      EE(5,5), EEP(15)
C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--
      IRET = 0
      IF ( NVHT.NE.2 ) THEN
         PRINT *, ' >>>>> Error in ULCVTK >>>>> '
         PRINT *, '   NVHT = ', NVHT, ' not equal 2.'
         PRINT *, '   Forced stop.'
         STOP
      ENDIF
C--
C  Store track informations to HELXIN.
C--
      CALL UVCOPY(30,TRAKIN(9),EEP)
      N = 0
      DO 100 J = 1, 5
         DO 10 I = 1, J
            N       = N + 1
            EE(I,J) = EEP(N)
            EE(J,I) = EE (I,J)
10       CONTINUE
100   CONTINUE
C--
      N = 0
      DO 200 J = 1, 5
         DO 20 I = J, 5
            N      = N + 1
            EEP(N) = EE(I,J)
20       CONTINUE
200   CONTINUE
C--
      CALL UVCOPY(8,TRAKIN(1),HELXIN(1))
      CALL UVCOPY(30,EEP(1),HELXIN(9))
C--
C  Move pivot to the inner vtx hit.
C--
      XP(1,1) = XVHT(1,1,1)
      XP(2,1) = XVHT(1,2,1)
      XP(3,1) = XVHT(1,3,1)
      XP(1,2) = XVHT(1,1,2)
      XP(2,2) = XVHT(1,2,2)
      XP(3,2) = XVHT(1,3,2)
      CALL UTRKMV(LNxHLX,HELXIN,XP(1,1),HELXOT)
C--
C  Create a combined track by using VTX hits and CDC momentum.
C--
      CPA   = HELXOT(3)
      X12   = XP(1,2) - XP(1,1)
      Y12   = XP(2,2) - XP(2,1)
      Z12   = XP(3,2) - XP(3,1)
      DD12  = X12*X12 + Y12*Y12
      D12   = SQRT( DD12 )
C--
      RHO   = PTOR/CPA
      RHOI  = 1/RHO
      SHF   = -RHOI*D12/2
      CHF   = SQRT( (1-SHF)*(1+SHF) )
      SNF   = 2*SHF*CHF
      CSF   = ( CHF - SHF )*( CHF + SHF )
      FI    = ATAN2(SNF,CSF)
      CSF0  = RHO*( X12*(1-CSF) - Y12*SNF )/DD12
      SNF0  = RHO*( X12*SNF - Y12*(1-CSF) )/DD12
C--
C  Calculate new track parameter.
C--
      FI0   = ATAN2(SNF0,CSF0)
      TNL   = - Z12*RHOI/FI
      TRAKOT(1) = 0
      TRAKOT(2) = FI0
      TRAKOT(3) = CPA
      TRAKOT(4) = 0
      TRAKOT(5) = TNL
      TRAKOT(6) = XP(1,1)
      TRAKOT(7) = XP(2,1)
      TRAKOT(8) = XP(3,1)
C--
C  Calculate new error matrix.
C--
      CALL UVCOPY(30,HELXOT(9),EEP(1))
C--
      N = 0
      DO 300 J = 1, 5
         DO 30 I = J, 5
            N       = N + 1
            EE(I,J) = EEP(N)
            EE(J,I) = EE (I,J)
30       CONTINUE
300   CONTINUE
C--
      N = 0
      DO 400 J = 1, 5
         DO 40 I = 1, J
            N      = N + 1
            EEP(N) = EE(I,J)
40       CONTINUE
400   CONTINUE
C--
      CALL UVCOPY(12,EEP,TRAKOT(9))
C--
C  That's it.
C--
      RETURN
      END
