CC********************************************************************CC
C*                                                                    *C
C*======================================----------===                 *C
C*  Subroutine UCRCLX(R1,XC1,R2,XC2,EPS,X1,X2,ISCT)                   *C
C*======================================----------===                 *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates intersections of 2 circles.                          *C
C* (Inputs)                                                           *C
C*      R1     : (R*8) ; radius of 1-st circle.                       *C
C*      XC1(*) : (R*8) ; center of 1-st circle.                       *C
C*      R2     : (R*8) ; radius of 2-nd circle.                       *C
C*      XC2(*) : (R*8) ; center of 2-nd circle.                       *C
C*      EPS    : (R*8) ; tolerance for non-intersection 2 circles     *C
C*                       in cm.                                       *C
C* (Outputs)                                                          *C
C*      X1 (*) : (R*8) ; 1-st intersection.                           *C
C*      X2 (*) : (R*8) ; 2-nd intersection.                           *C
C*      ISCT   : (I*4) ; # intersections.                             *C
C* (Relation)                                                         *C
C*    Requires no subroutines or functions.                           *C
C* (Update Record)                                                    *C
C*    5/22/87  K.Fujii        Original version.                       *C
C*    6/26/90  K.Fujii        Fixed a bug. This routine has been      *C
C*                            giving a fake intersection when         *C
C*                            one of the given two circle contains    *C
C*                            the other completely.                   *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UCRCLX(R1,XC1,R2,XC2,EPS,X1,X2,ISCT)
   
      IMPLICIT    REAL*8 ( A-H, O-Z )
      REAL   *8   R1,XC1(2),R2,XC2(2),EPS,X1(2),X2(2)
      INTEGER*4   ISCT
      REAL   *8   X12(2),D1(2),D2(2)
C  
C========< Entry Point >================================================
C  
C--
C  Check if 2 circles intersect.
C--
      X12(1) = XC2(1) - XC1(1)
      X12(2) = XC2(2) - XC1(2)
      A      = X12(1)*X12(1) + X12(2)*X12(2)
      SQA    = DSQRT( A )
      RADD   = R1 + R2
      RSUB   = ABS( R1 - R2 )
      IF ( SQA.GT.RADD+EPS .OR. SQA.LE.1.D-3 .OR.
     .     SQA.LT.RSUB-EPS ) THEN
C--
C  (1) No intersection.
C--
         ISCT  = 0
         RETURN
      ELSE IF ( SQA.GT.RADD-EPS ) THEN
C--
C  (2) Single intersection.
C--
         D     = R1/SQA
         X1(1) = XC1(1) + D*X12(1)
         X1(2) = XC1(2) + D*X12(2)
         ISCT  = 1
         RETURN
      ENDIF
C--
C  (3) 2 intersectons.
C--
      D     = (R1+R2)*(R1-R2) + A
      A     = 1.D0 / SQA
      D     = 0.5D0*D*A
      DP    = (R1+D)*(R1-D)
      IF ( DP.LE.0.D0 ) THEN
         DP = 0.D0
      ELSE
         DP = DSQRT(DP)*A
      ENDIF
      D     = D*A
      D1(1) = XC1(1) + D*X12(1)
      D1(2) = XC1(2) + D*X12(2)
      D2(1) =  DP*X12(2)
      D2(2) = -DP*X12(1)
      X1(1) = D1(1) + D2(1)
      X1(2) = D1(2) + D2(2)
      X2(1) = D1(1) - D2(1)
      X2(2) = D1(2) - D2(2)
C--
C  This is it.
C--
      ISCT = 2
   
      RETURN
      END
