CC********************************************************************CC
C*                                                                    *C
C*================================------------====                    *C
C*  Subroutine UFPAIR(HELX1,HELX2,XV,QUAL,IRET)                       *C
C*================================------------====                    *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Get approximate intersection of 2 helices by 1-st calculating   *C
C*    intersections of 2 circles and then checking their Z coords.    *C
C* (Inputs)                                                           *C
C*      HELX1(): (R*4) ; helix parameters for 1-st track.             *C
C*      HELX2(): (R*4) ; helix parameters for 2-nd track.             *C
C*                       1-st 8 words in                              *C
C*                       Production:TPC;Track_Parameter.              *C
C* (Outputs)                                                          *C
C*      XV()   : (R*4) ; vertex vector.                               *C
C*      QUAL   : (R*4) ; vertex quality. ( Z difference in cm )       *C
C*      IRET   : (I*4) ; return code. (0,<0)=(OK,Error).              *C
C* (Relation)                                                         *C
C*    Calls    :  UCRCLX.                                             *C
C* (Update Record)                                                    *C
C*    5/22/87  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UFPAIR(HELX1,HELX2,XV,QUAL,IRET)
   
      IMPLICIT   REAL*8  ( A-H, O-Z )
   
#include "unumcn.inc"
#include "prfeld.inc"
      INTEGER*4  IRET
      REAL   *4  HELX1(8),HELX2(8),XV(3),QUAL
      REAL   *4  HELX(8,2),QUA
      REAL   *8  XC(3,2),XX(2,2),FI0(2),TNL(2),R(2),Z(2)
      INTEGER*4  ICHG(2)
   
      DATA  EPS  /   1.0D0       /
   
      EXTERNAL UCONST, PRBLK1
   
C  
C========< Entry Point >================================================
C  
C  
C--
C  Reset return code and quality.
C--
      IRET = -1
      QUAL = 9999.E0
C--
C  Fill track parameters in HELX.
C--
      DO 10 I = 1, 8
         HELX(I,1) = HELX1(I)
         HELX(I,2) = HELX2(I)
10    CONTINUE
C--
C  Change helix parametrization.
C     X = XC(1) - R*cos(FI+FI0)
C     Y = XC(2) - R*sin(FI+FI0)
C     Z = XC(3) - R*TNL*FI
C--
      DO 20 ITK = 1,2
         FI0 (ITK) = HELX(2,ITK)
         TNL (ITK) = HELX(5,ITK)
         ICHG(ITK) = SIGN(1.1,HELX(3,ITK))
         R   (ITK) = PTOR/HELX(3,ITK)
         RDR       = R(ITK) + HELX(1,ITK)
         XC(1,ITK) = HELX(6,ITK) + RDR*DCOS(FI0(ITK))
         XC(2,ITK) = HELX(7,ITK) + RDR*DSIN(FI0(ITK))
         XC(3,ITK) = HELX(8,ITK) + HELX(4,ITK)
20    CONTINUE
C--
C  Galculate intersection of 2 circles.
C--
      CALL UCRCLX(DABS(R(1)),XC(1,1),DABS(R(2)),XC(1,2),
     .                           EPS,XX(1,1),XX(1,2),NX)
C--
C  If no intersection found, return with IRET = -1.
C--
      IF ( NX.LE.0 ) RETURN
C--
C  Loop over intersections.
C--
      DO 300 IX = 1, NX
         DO 30 ITK = 1, 2
            X    = XX(1,IX) - XC(1,ITK)
            Y    = XX(2,IX) - XC(2,ITK)
            DFI  = DATAN2(Y,X) - FI0(ITK) - xHPID*( 1 + ICHG(ITK) )
            DFI  = DMOD(DFI,x2PID)
            IF ( DFI.GT. xPID ) DFI = DFI - x2PID
            IF ( DFI.LT.-xPID ) DFI = DFI + x2PID
            IF ( ICHG(ITK)*DFI.LT.0.D0 )         GO TO 300
            Z(ITK) = XC(3,ITK) - R(ITK)*TNL(ITK)*DFI
30       CONTINUE
         IRET = 0
         QUA  = DABS( Z(1) - Z(2) )
         IF ( QUA.LT.QUAL ) THEN
            QUAL  = QUA
            XV(1) = XX(1,IX)
            XV(2) = XX(2,IX)
            XV(3) = 0.5D0 * ( Z(1) + Z(2) )
         ENDIF
300   CONTINUE
C--
C  This is it.
C--
      RETURN
      END
