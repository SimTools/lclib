#ifdef DBG
#include "taym.a#jlc.fort(ewinit)"
#endif 
CDBG
#ifdef DBG
      IMPLICIT REAL*8 (A-H,O-Z,c_$)
#endif 
#ifdef DBG
      REAL*8 PK(0:3), PF1(0:3), PF2(0:3)
#endif 
#ifdef DBG
#include "taym.a#jlc.fort(@ewparm)"
#endif 
CDBG
#ifdef DBG
      CALL EWINIT
#endif 
#ifdef DBG
      CV    = -0.25D0 + c_$SN2TW
#endif 
#ifdef DBG
      CA    =  0.25D0
#endif 
#ifdef DBG
      AMH   = 100.
#endif 
#ifdef DBG
      AMF   = 0.0
#endif 
CDBG
#ifdef DBG
      PK(0) = 200.0
#endif 
#ifdef DBG
      PABS  = DSQRT( PK(0)**2 - AMH*AMH )
#endif 
#ifdef DBG
C      TH    = 30.0/180.*c_$PI
#endif 
#ifdef DBG
C      PHI   = 45./180.*c_$PI
#endif 
#ifdef DBG
      TH    = 0.0/180.*c_$PI
#endif 
#ifdef DBG
      PHI   = 0.0/180.*c_$PI
#endif 
#ifdef DBG
      PK(1) = PABS*SIN(TH)*COS(PHI)
#endif 
#ifdef DBG
      PK(2) = PABS*SIN(TH)*SIN(PHI)
#endif 
#ifdef DBG
      PK(3) = PABS*COS(TH)
#endif 
CDBG
#ifdef DBG
      NPOL  = 1
#endif 
#ifdef DBG
      CALL DECAY2( PK, AMH, AMF, AMF, PF1, PF2 )
#endif 
CDBG
#ifdef DBG
      PRINT *,' PF1=',PF1
#endif 
#ifdef DBG
      PRINT *,' PF2=',PF2
#endif 
CDBG
#ifdef DBG
      XSM2 = (PF1(0)+PF2(0))**2 - (PF1(1)+PF2(1))**2
#endif 
#ifdef DBG
     >     - (PF1(2)+PF2(2))**2 - (PF1(3)+PF2(3))**2
#endif 
#ifdef DBG
      XMAS = DSQRT(XSM2)
#endif 
#ifdef DBG
      PBALE = PK(0) - PF1(0) - PF2(0)
#endif 
#ifdef DBG
      PBALX = PK(1) - PF1(1) - PF2(1)
#endif 
#ifdef DBG
      PBALY = PK(2) - PF1(2) - PF2(2)
#endif 
#ifdef DBG
      PBALZ = PK(3) - PF1(3) - PF2(3)
#endif 
#ifdef DBG
      PRINT *,' XMAS =',XMAS
#endif 
#ifdef DBG
      PRINT *,' PBALE=',PBALE
#endif 
#ifdef DBG
      PRINT *,' PBALX=',PBALX
#endif 
#ifdef DBG
      PRINT *,' PBALY=',PBALY
#endif 
#ifdef DBG
      PRINT *,' PBALZ=',PBALZ
#endif 
CDBG
#ifdef DBG
      STOP
#endif 
#ifdef DBG
      END
#endif 
C***********************************************************************
C*
C* --------------------------------------=========
C*  Subroutine DECAY2( PK, AMK, AM1, AM2, PF1, PF2 )
C* --------------------------------------=========
C*(Function)
C*  Does two body phase space decay.
C*(Input)
C*   PK   : Initial Z 4-momentum (E,Px,Py,Pz)
C*   AMK  : Mass of parent particle.
C*   AM1  : Daughter particle-1 mass.
C*   AM2  : Daughter particle-2 mass.
C*(Output)
C*   PF1  : 4 momentum of particle 1  Z lab system
C*   PF2  : 4 momentum of fbar at Z lab system
C*(Author)
C*   A.Miyamoto  13-Feb-1990  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DECAY2( PK, AMK, AM1, AM2, PF1, PF2 )
C
      IMPLICIT REAL*8 (A-H,O-Z)
c      IMPLICIT REAL*8  (A-H,O-Z,c_$)
C
C   =EXPAND 'TAYM.A#JLC.FORT(@EWPARM)'
C
      REAL*8   PK(0:3), PF1(0:3), PF2(0:3)
      REAL*4   SKLAB(4), SE(3,3), P1CM(4), P2CM(4), PMOM(4)
      DATA     ITEMP/14242314/
      REAL*4   RAN
      DATA     c_$2PI/6.283185307D0/
C
C ====< Entry Point >==================================================
C
C ---------------------------------------------------------------------
C (1) Determine CM momentum
C ---------------------------------------------------------------------
C
      E1  = ( AMK*AMK - AM2*AM2 + AM1+AM1 ) / ( 2.*AMK)
      E2  = AMK - E1
      PCM = DSQRT( (E1-AM1)*(E1+AM1) )
C
      COSTH = -1.D0 + 2.D0*RAN(ITEMP)
      PHI   = c_$2PI*RAN(ITEMP)
      SIN2  = ( 1.D0 - COSTH)*(1.D0+COSTH)
      IF( SIN2 .LT. 0.D0 ) THEN
        SINTH = 0.0
        COSTH = 1.0D0
      ELSEIF( SIN2 .GT. 1.0D0 ) THEN
        SINTH = 1.0
        COSTH = 0.0
      ELSE
        SINTH = DSQRT(SIN2)
      ENDIF
C
      P1CM(1) = PCM*SINTH*COS(PHI)
      P1CM(2) = PCM*SINTH*SIN(PHI)
      P1CM(3) = PCM*COSTH
      P1CM(4) = E1
      P2CM(1) = -P1CM(1)
      P2CM(2) = -P1CM(2)
      P2CM(3) = -P1CM(3)
      P2CM(4) = E2
C
C --------------------------------------------------------------------
C (3) Transfer frame in Z rest system
C --------------------------------------------------------------------
C
      SKLAB(1) = PK(1)
      SKLAB(2) = PK(2)
      SKLAB(3) = PK(3)
      SKLAB(4) = PK(0)
      CALL USETRF( SKLAB, SE )
      CALL UBTRAN( P1CM, SE, PMOM )
      PMOM(4)  = E1
      CALL UBSTBK( PMOM, SKLAB, P1CM )
 
      CALL UBTRAN( P2CM, SE, PMOM )
      PMOM(4)  = E2
      CALL UBSTBK( PMOM, SKLAB, P2CM )
 
C
C --------------------------------------------------------------------
C (4) Copy Buffer.
C --------------------------------------------------------------------
C
      PF1(0) = P1CM(4)
      PF1(1) = P1CM(1)
      PF1(2) = P1CM(2)
      PF1(3) = P1CM(3)
C
      PF2(0) = P2CM(4)
      PF2(1) = P2CM(1)
      PF2(2) = P2CM(2)
      PF2(3) = P2CM(3)
C
      RETURN
      END
