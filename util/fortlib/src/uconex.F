CC********************************************************************CC
C*                                                                    *C
C*====================================-------===                      *C
C*  Subroutine UCONEX(XT,RZR,ZR,X0,ED,XX,IRET)                        *C
C*====================================-------===                      *C
C*                                                                    *C
C* (Function)                                                         *C
C*    Calulate intersection of a directed straight line with a        *C
C*    elliptic cone. Axes of the elliptic cone must be aligned with   *C
C*    X and Y axes. If not, user should rotate the line and the cone  *C
C*    before calling this routine.                                    *C
C* (Inputs)                                                           *C
C*    XT(1-3)    : (R*4) ; position of apex.                          *C
C*    RZR(1)     : (R*4) ; X-radius at Z = ZR.                        *C
C*       (2)     : (R*4) ; Y-radius at Z = ZR.                        *C
C*    ZR         : (R*4) ; reference Z.                               *C
C*    X0(1-3)    : (R*4) ; starting position of straight line.        *C
C*    ED(1-3)    : (R*4) ; direction vector (not necessarily          *C
C*                         normalized).                               *C
C* (Outputs)                                                          *C
C*    XX(1-3)    : (R*4) ; first intersection.                        *C
C*    IRET       : (I*4) ; return flag.                               *C
C*                         =(0,<0)=(OK,no intersection)               *C
C* (Update Record)                                                    *C
C*    12/13/89   K.Fujii              Original version.               *C
C*     3/30/90   K.Fujii              REAL*4 --> REAL*8 with          *C
C*                                    modification relating to        *C
C*                                    ghost hit handling.             *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UCONEX(XT,RZR,ZR,X0,ED,XX,IRET)
   
      IMPLICIT  REAL*8 ( A-H, O-Z )
      INTEGER*4 IRET
      REAL   *8 XT(3), RZR(2), ZR, X0(3), ED(3), XX(3)
      REAL   *8 X0P(3), EP(3), R(2)
C  
C========< Entry Point >================================================
C  
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Translate origin to XT.
C--
      DO 10 I = 1, 3
         X0P(I) = X0(I) - XT(I)
10    CONTINUE
      ZRP = ZR - XT(3)
C--
C  Project every thing from XT to Z = ZR plane and change scales
C  of X and Y axes to transform the ellipse to a circle.
C--
C>>>> Check if X0P(3) = 0
      IF ( X0P(3).EQ.0.D0 ) THEN
C        PRINT *, ' >>>>> Error in UCONEX.'
C        PRINT *, '   Starting point has the same Z as the apex.'
C        PRINT *, '   Should move the starting point a little bit.'
         IRET = -1
         RETURN
      ENDIF
      F = ZRP/X0P(3)
      DO 20 I = 1, 2
         R  (I) = RZR(I)/ZRP
         FP     = F/R(I)
         EP (I) = FP*(X0P(3)*ED(I)-X0P(I)*ED(3))
         X0P(I) = FP*X0P(I)
20    CONTINUE
C--
C  Calculate intersection in the projected plane.
C--
      A  = EP(1)*EP(1) + EP(2)*EP(2)
      B  = X0P(1)*EP(1) + X0P(2)*EP(2)
      C  = X0P(1)*X0P(1) + X0P(2)*X0P(2) - ZRP*ZRP
      D  = B*B - A*C
C>>>> Check if A = 0 or D < 0.
      IF ( A.EQ.0.D0 ) THEN
C        PRINT *, ' >>>>> Error in UCONEX.'
C        PRINT *, '   The line passing through the apex of the cone'
         IRET = -2
         RETURN
      ELSE IF ( D.LT.0.D0 ) THEN
C        PRINT *, ' >>>>> Error in UCONEX.'
C        PRINT *, '   No intersection.'
         IRET = -3
         RETURN
      ENDIF
C--
C  Choose the first intersection by checking if the starting point
C  is inside or outside the cone.
C--
      SQD = SQRT(D)
      TP  = (-B-SQD)/A
      T1  = X0P(3)*TP/(1-ED(3)*TP)
      TP  = (-B+SQD)/A
      T2  = X0P(3)*TP/(1-ED(3)*TP)
      TMN = MIN(T1,T2)
      TMX = MAX(T1,T2)
C>>>> Check if T < 0
      IF ( TMX.LT.0.D0 ) THEN
C        PRINT *, ' >>>>> Error in UCONEX.'
C        PRINT *, '   No intersection.'
         IRET = -4
         RETURN
      ENDIF
C--
      TEST = X0P(1)*X0P(1) + X0P(2)*X0P(2)
      IF ( TEST.LT.ZRP*ZRP ) THEN
         IF ( TMN.GT.0.D0 ) THEN
            IF ( X0P(3)*ZRP.GT.0.D0 ) THEN
               T = TMN
            ELSE
               T = TMX
            ENDIF
         ELSE
            T = TMX
         ENDIF
      ELSE
         IF ( TMN.LT.0.D0 ) THEN
            T = TMX
         ELSE
            T = TMN
         ENDIF
      ENDIF
C--
C  Transform back to the original frame.
C--
      DO 30 I = 1, 3
         XX(I) = X0(I) + T*ED(I)
30    CONTINUE
C>>>> Check if intersection is not a ghost one.
      IF ( (ZR-XT(3))*(XX(3)-XT(3)).LT.0.D0 ) THEN
C        PRINT *, ' >>>>> Error in UCONEX.'
C        PRINT *, '   No intersection.'
         IRET = -5
         RETURN
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
