C***********************************************************************
C*
C*================================----===
C* Subroutine SMRCHT(LEVEL,IDEBUG,IRET)
C*================================----===
C*
C* (Purpose)
C*     Create Production:Charged_Track by smearing 4-momenta in
C*     Generator:Particle_List.
C* (Inputs)
C*        LEVEL    : (I*4) :  level flag.
C*        IDEBUG   : (I*4) :  debug flag.
C* (Output)
C*        IRET     : (I*4) :  return flag.
C* (Relation)
C*     Called by SMREVT.
C*     Calls the following routines;
C* (Updaterecord)
C*     4/18/90  K.Fujii           Original version.
C*     5/23/90  K.Fujii           Do not ignore unstable particles.
C*                                Caution: This version ignores
C*                                particles which curl back into the
C*                                inner wall of the tracking device
C*                                and decay.
C*     5/25/90  K.Fujii           Arguments of SMRHLX was changed.
C*     6/07/90  K.Fujii           Changed track bank name to store
C*                                track parameters and corresponding
C*                                error matrix.
C*
C***********************************************************************
 
      SUBROUTINE SMRCHT(LEVEL,IDEBUG,IRET)
 
#include "smrgdt.inc"
#include "smrtgo.inc"
      COMMON /SSWORK/ RDATA(100), HELX8(100)
      REAL   *8       HELX8
      INTEGER*4       IDATA(100)
      EQUIVALENCE    (IDATA,RDATA)
      CHARACTER*64    BKNAME / 'Production:CDC;Track_Parameter' /
      PARAMETER     ( LNxCHT = 57 )
C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Loop over final state particles.
C--
      NCHTRK = 0
      DO 1000 KPT = 1, NPRTCL
         IPT = IEPLST(KPT)
C--
C  Skip neutral particles.
C--
         ICHG = RPLIST( 4,IPT)
         IF ( ICHG.EQ.0 )                        GO TO 1000
C--
C  If unstable, check if decay point inside the inner cylinder of
C  the tracking device. If yes, skip this  particle.
C--
         NDOT = RPLIST(12,IPT)
         IF ( NDOT.GT.0 ) THEN
            IDT = RPLIST(13,IPT)
            XDK = RPLIST( 9,IDT)
            YDK = RPLIST(10,IDT)
            ZDK = RPLIST(11,IDT)
            RDK = SQRT(XDK*XDK+YDK*YDK)
            IF ( RDK.LT.RCYTK(1) )               GO TO 1000
         ENDIF
C--
C  Smear helix paramters.
C--
         CALL SMRHLX(RPLIST(1,IPT),RHELIX(1,IPT),RDATA(11),IRT)
         IF ( IRT.LT.0 )                         GO TO 1000
C--
C  Get smeared momentum and postion.
C--
         DO 100 I = 1, 8
            HELX8(I) = RDATA(I+10)
100      CONTINUE
         CALL UFDCAP(HELX8(1),RDATA(5),RDATA(1))
C--
C  TBPUT 'Production:CDC;Track_Parameter'.
C        RDATA(    1) = PX
C             (    2) = PY
C             (    3) = PZ
C             (    4) = E ( assuming massless particle )
C             (    5) = X
C             (    6) = Y
C             (    7) = Z
C             (    8) = not used
C        IDATA(    9) = apparent charge ( +/-1 )
C             (   10) = not used
C        RDATA(   11) = d_rho
C             (   12) = phi_0
C             (   13) = kappa
C             (   14) = d_Z
C             (   15) = tan(lambda)
C             (   16) = X0
C             (   17) = Y0
C             (   18) = Z0
C        DDATA(10-24) = error matrix (lower triangle only)
C        RDATA(   49) = not used ( reserved for confidence level )
C             (   50) = not used ( reserved for chi**2 )
C        HDATA( 1,51) = # degrees of freedom
C             ( 2,51) = not used ( reserved for duplicate flag )
C             ( 1,52) = # track parameters ( 5 for helices )
C             ( 2,52) = not used ( reserved for track finding type )
C             ( 1,53) = not used ( reserved for pointer to 1-st hit )
C             ( 2,53) = not used ( reserved for pointer to last hit )
C        RDATA(   54) = radius of innermost hit
C             (   55) = radius of outermost hit
C        IDATA(   56) = # contributing particle
C             (   57) = 1-st gnerator particle #
C--
         RDATA( 4) = SQRT(RDATA(1)**2+RDATA(2)**2+RDATA(3)**2)
         IDATA( 8) = 0
         IDATA( 9) = SIGN(1.1,RDATA(13))
         IDATA(10) = 0
         IDATA(56) = 1
         IDATA(57) = IPT
C--
         NCHTRK   = NCHTRK + 1
         CALL TBPUT(1,BKNAME,NCHTRK,LNxCHT,RDATA(1),IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, 'SMRCHT failed to TBPUT el#', NCHTRK, ' of ',BKNAME
            PRINT *, '    IRT = ', IRT
            IRET = IRT
            RETURN
         ENDIF
1000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
