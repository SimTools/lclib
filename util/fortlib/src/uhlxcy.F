CC********************************************************************CC
C*                                                                    *C
C*======================================-----------===                *C
C*  Subroutine UHLXCY(RCY,ZCY,EPSL,HELX,PHI,XP,IRET)                  *C
C*======================================-----------===                *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Gives phi of the intersection of a helix and a cylinder whose   *C
C*    axis coincides with Z-axis.                                     *C
C* (Inputs)                                                           *C
C*      RCY(1)      : (R*4) ; inner radius of the endcap.             *C
C*         (2)      : (R*4) ; radius of the cylinder.                 *C
C*      ZCY(1)      : (R*4) ; Z of -ve endcap.                        *C
C*         (2)      : (R*4) ; Z of +ve endcap.                        *C
C*      EPSL        : (R*4) ; tolerance for intersection.             *C
C*      HELX(*)     : (R*4) ; helix parameter vector. 1-st  8 words   *C
C*                            should have the same format as that of  *C
C*                            Production:TPC;Track_Parameter.         *C
C* (Outputs)                                                          *C
C*      PHI         : (R*4) ; deflection angle at 1-st intersection.  *C
C*      XP(3)       : (R*4) ; 1-st intersection. In order to decide   *C
C*                          ; the 1-st intersection, the pivotal      *C
C*                          ; point is used as the starting position. *C
C*      IRET        : (I*4) ; return flag.                            *C
C*                     < 0  ; error.                                  *C
C*                     = 0  ; no intersection.                        *C
C*                     = 1  ; barrel hit.                             *C
C*                     = 2  ; -Z endcap hit.                          *C
C*                     = 3  ; +Z endcap hit.                          *C
C* (Relation)                                                         *C
C*    Calls : UCRCLX.                                                 *C
C* (Update Record)                                                    *C
C*    6/02/88  K.Fujii        Original version.                       *C
C*    4/19/90  K.Fujii        Modified to handle endcap hits.         *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UHLXCY(RCY,ZCY,EPSL,HELX,PHI,XP,IRET)
   
      IMPLICIT   REAL*8 ( A-H, O-Z )
#include "unumcn.inc"
#include "prfeld.inc"
      INTEGER*4      IRET
      REAL   *4      RCY(2), ZCY(2), EPSL, HELX(*), PHI, XP(3)
      REAL   *8      XC(3), XCV(2), XX(2,2)
      DATA  XCV      /  2*0.0D0          /
      EXTERNAL       UCONST, PRBLK1
C  
C========< Entry Point >================================================
C  
#ifdef HIUXF77
      CALL PRBLK1
      CALL UCONST
#endif
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Change helix parametrization.
C     X = XC(1) - R*cos(FI+FI0)
C     Y = XC(2) - R*sin(FI+FI0)
C     Z = XC(3) - R*TNL*FI
C--
      DR    = HELX(1)
      FI0   = HELX(2)
      CPA   = HELX(3)
      DZ    = HELX(4)
      TNL   = HELX(5)
      X0    = HELX(6)
      Y0    = HELX(7)
      Z0    = HELX(8)
C--
C  Charge must be nonzero.
C--
      ICHG  = SIGN(1.1D0,CPA)
      IF ( ICHG.EQ.0 ) THEN
         PRINT *, 'UHLXCY gets a straight line.'
         IRET = -1
         RETURN
      ENDIF
C--
      R     = PTOR/CPA
      RDR   = R  + DR
      ZDZ   = Z0 + DZ
      CSF0  = COS(FI0)
      SNF0  = SIN(FI0)
      XC(1) = X0 + RDR*CSF0
      XC(2) = Y0 + RDR*SNF0
      XC(3) = ZDZ
C--
C  Calculate intersection of 2 circles.
C--
      RV  = RCY(2)
      EPS = EPSL
      CALL UCRCLX(ABS(R),XC(1),RV,XCV(1),EPS,XX(1,1),XX(1,2),NX)
C--
C  Tracks hitting barrel part.
C--
      IF ( NX.NE.2 )                             GO TO 2000
C--
C  Loop over intersections to choose the first intersection.
C--
      DFIS = 9999.D0
      DO 100 IX = 1, 2
         X    = XX(1,IX) - XC(1)
         Y    = XX(2,IX) - XC(2)
         DFI  = ATAN2(Y,X) - FI0 - xHPID*( 1 + ICHG )
         DFI  = MOD(DFI+4.D0*x2PID,x2PID)
         IF ( DFI.GT.0.D0 .AND. ICHG.GT.0 ) DFI = DFI - x2PID
         IF ( ABS(DFI).LT.ABS(DFIS) ) THEN
            DFIS  = DFI
            XP(1) = XX(1,IX)
            XP(2) = XX(2,IX)
            XP(3) = XC(3) - R*TNL*DFI
         ENDIF
100   CONTINUE
      IF ( XP(3).GE.ZCY(1) .AND. XP(3).LE.ZCY(2) ) THEN
         IRET = 1
                                                 GO TO 3000
      ENDIF
C--
C  Tracks hitting endcaps.
C--
2000  IF ( ABS(TNL).LT.0.1D0 ) THEN
         RETURN
      ELSE IF ( TNL.GT.0.D0 ) THEN
         ZEE  = ZCY(2)
         IRET = 3
      ELSE
         ZEE  = ZCY(1)
         IRET = 2
      ENDIF
      DFIS = (ZDZ-ZEE)/R/TNL
C--
C  Require DFI < 2*PI.
C--
      IF ( ABS(DFIS).GT.x2PID ) THEN
         IRET = 0
         RETURN
      ELSE
         XP(1) = X0 + RDR*CSF0 - R*COS(DFIS+FI0)
         XP(2) = Y0 + RDR*SNF0 - R*SIN(DFIS+FI0)
         XP(3) = ZEE
         TEST  = XP(1)*XP(1) + XP(2)*XP(2) - RCY(1)*RCY(1)
         IF ( TEST.LT.0.D0 ) THEN
            IRET = 0
            RETURN
         ENDIF
      ENDIF
C--
C  Set PHI and return.
C--
3000  PHI = DFIS
C--
C  This is it.
C--
      RETURN
      END
