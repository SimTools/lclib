CC********************************************************************CC
C*                                                                    *C
C*======================================-----------===                *C
C*  Subroutine ULINCY(RCY,ZCY,EPSL,HELX,TEE,XP,IRET)                  *C
C*======================================-----------===                *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Gives TEE of the intersection of a straight line and a cylinder *C
C*    whose axis coincides with the z-axis.                           *C
C* (Inputs)                                                           *C
C*      RCY         : (R*4) ; radius of the cylinder.                 *C
C*      ZCY(2)      : (R*4) ; Z lower and upper limits.               *C
C*      EPSL        : (R*4) ; tolerance for intersection.             *C
C*      HELX(*)     : (R*4) ; helix parameter vector. 1-st  8 words   *C
C*                            should have the same format as that of  *C
C*                            Production:TPC;Track_Parameter.         *C
C* (Outputs)                                                          *C
C*      TEE         : (R*4) ; xy-projected path length from pivot.    *C
C*      XP(3)       : (R*4) ; intersection.                           *C
C*      IRET        : (I*4) ; return flag.                            *C
C*                     < 0  ; error.                                  *C
C*                     = 0  ; no intersection.                        *C
C*                     = 1  ; barrel hit.                             *C
C*                     = 2  ; -Z endcap hit.                          *C
C*                     = 3  ; +Z endcap hit.                          *C
C* (Relation)                                                         *C
C*    Calls no subroutines.                                           *C
C* (Update Record)                                                    *C
C*    4/19/90  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE ULINCY(RCY,ZCY,EPSL,HELX,TEE,XP,IRET)
   
      IMPLICIT   REAL*8 ( A-H, O-Z )
      INTEGER*4      IRET
      REAL   *4      RCY(2), ZCY(2), EPSL, HELX(*), TEE, XP(3)
      REAL   *8      XC(3)
C  
C========< Entry Point >================================================
C  
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Change straight line parametrization.
C     X = XC(1) - t*sin(FI0)
C     Y = XC(2) + t*cos(FI0)
C     Z = XC(3) + t*TNL
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
C  Charge must be zero.
C--
      ICHG  = 1.1D0*CPA
      IF ( ICHG.NE.0 ) THEN
         PRINT *, 'ULINCY gets a helix.'
         IRET = -1
         RETURN
      ENDIF
C--
      ZDZ   = Z0 + DZ
      CSF0  = COS(FI0)
      SNF0  = SIN(FI0)
      XC(1) = X0 + DR*CSF0
      XC(2) = Y0 + DR*SNF0
      XC(3) = ZDZ
C--
C  Calculate intersection.
C--
      B   = -SNF0*XC(1) + CSF0*XC(2)
      C   =  XC(1)*XC(1) + XC(2)*XC(2) - RCY(2)*RCY(2)
      D   =  B*B - C
      IF ( D.LT.0.D0 ) RETURN
      T   = -B + SQRT(D)
C--
      XP(1) = XC(1) - T*SNF0
      XP(2) = XC(2) + T*CSF0
      XP(3) = XC(3) + T*TNL
C--
C  Tracks hitting barrel part.
C--
      IF ( XP(3).GE.ZCY(1) .AND. XP(3).LE.ZCY(2) ) THEN
         IRET = 1
                                                 GO TO 1000
C--
C  Tracks hitting endcaps.
C--
      ELSE IF ( TNL.GT.0.D0 ) THEN
         ZEE  = ZCY(2)
         IRET = 3
      ELSE
         ZEE  = ZCY(1)
         IRET = 2
      ENDIF
      T = (ZEE-ZDZ)/TNL
      XP(1) = XC(1) - T*SNF0
      XP(2) = XC(2) + T*CSF0
      XP(3) = ZEE
      TEST  = XP(1)*XP(1) + XP(2)*XP(2) - RCY(1)*RCY(1)
      IF ( TEST.LT.0.D0 ) THEN
         IRET = 0
         RETURN
      ENDIF
C--
C  Set TEE.
C--
1000  TEE = T
C--
C  This is it.
C--
      RETURN
      END
