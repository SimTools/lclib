C***********************************************************************
C*
C*===============================----------===
C* Subroutine SMRHLX(PLIST,HLXCT,HLSMR,IRET)
C*===============================----------===
C*
C* (Purpose)
C*     Smear helix parameters, taking track-parameter-dependence
C*     into account.
C* (Inputs)
C*        PLIST(1-20) : (R*4) :  particle list (20 words)
C*        HLXCT(1- 8) : (R*4) :  exact helix parameters.
C*                            :  (d_r,phi_0,kappa,d_z,tan(lambda))
C* (Output)
C*        HLSMR(1- 8) : (R*4) :  smeared track parameters.
C*             (9-38) : (R*8) :  error matrix ( lower triangle ).
C*             (  39) : (R*4) :  not used.
C*             (  40) : (R*4) :  not used.
C*             (  41) : (I*2) :  # degrees of freedom   / not used.
C*             (  42) : (I*2) :  # track parameters = 5 / not used.
C*             (  43) : (I*2) :  not used               / not used.
C*             (  44) : (R*4) :  R1.
C*             (  45) : (R*4) :  R2.
C*        IRET        : (I*4) :  return code.
C*                            :  = 0 ; OK.
C*                            :  < 0 ; un-trackable.
C* (Relation)
C*     Called by SMRCHT.
C*     Calls the following routines;
C* (Updaterecord)
C*     4/18/90  K.Fujii           Original version.
C*     5/25/90  K.Fujii           Heavily modified to take parameter
C*                                dependence and error correlation
C*                                into account.
C*     6/07/90  K.Fujii           Modified to store error matrix and
C*                                related information.
C*     7/16/92  K.Fujii           Fixed a bug concerning shallow
C*                                tracks.
C*     2/3/99   A.Miyamoto        Fix bug to set error matrix.
C*                                Add type defs of RANN, otherwise don't
C*                                work in HIUX
C*
C***********************************************************************
 
      SUBROUTINE SMRHLX(PLIST,HLXCT,HLSMR,IRET)
 
      IMPLICIT REAL*8    ( A-H, O-Z )
#include "prfeld.inc"
#include "smrres.inc"
#include "smrtgo.inc"
#include "smrrnd.inc"
      INTEGER*4    IRET
      REAL   *4    PLIST(20), HLXCT(8), HLSMR(45)
C--
      REAL   *4    EPSL, PHI, XV(3), PV(3), RCY(2)
      REAL   *8    SGB(5), DADB1(3,3), DADB2(2,2), SGBS(5), DADB1S(3,3),
     .             DADB2S(2,2), ERROR(15)
      REAL   *4    RDATA(10)
      INTEGER*2    HDATA(2,10)
      EQUIVALENCE ( RDATA, HDATA )
      REAL*4       RANN
      EXTERNAL     RANN
      DATA EPSL  / 0.1 /
      DATA NCALL / 0 /

C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--
      IRET = -1
C--
C  Initialization.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL  = 1
         RCY1   = RCYTK(1)
         RCY2   = RCYTK(2)
         ZCY1   = ZCYTK(1)
         ZCY2   = ZCYTK(2)
         ARTEST = RCY1 + RCY2
         TNTEST = 0.8D0*ZCY2/RCY2
         TNTST2 = ZCY2/RCY1
         NHIT   = NSMPTK
         CALL SMRHLE(NHIT,DSMPTK,SIGRF,SIGZE,SGBS,DADB1S,DADB2S,IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, ' >>>>>> Fatal Error in SMRHLX >>>>>>>'
            PRINT *, '    Cannot calculate error matrix. '
            PRINT *, '       NSMPTK = ', NSMPTK
            PRINT *, '       DSMPTK = ', DSMPTK
            PRINT *, '       SIGRF  = ', SIGRF
            PRINT *, '       SIGZE  = ', SIGZE
            PRINT *, '       PTOR   = ', PTOR
            PRINT *, '    STOP.'
            STOP
         ENDIF
      ENDIF
C--
C  Decide radii of 1-st and last sampling points.
C--
      DR   = HLXCT(1)
      FI0  = HLXCT(2)
      CPA  = HLXCT(3)
      TNL  = HLXCT(5)
      X0   = HLXCT(6)
      Y0   = HLXCT(7)
      Z0   = HLXCT(8)
      R    = PTOR/CPA
      AR   = ABS(R)
      RDR  = R + DR
      CSF0 = COS(FI0)
      SNF0 = SIN(FI0)
      XC   = X0 + RDR*CSF0
      YC   = Y0 + RDR*SNF0
C--
C  Check starting point.
C--
      R1   = SQRT(X0*X0+Y0*Y0)
      IF ( R1.GT.RCY2 .OR. Z0.LT.ZCY1 .OR.
     .     Z0.GT.ZCY2 ) RETURN
      R1   = MAX(R1,RCY1)
C--
C  Check end point.
C--
      NDOT = PLIST(12)
      IF ( NDOT.GT.0 ) THEN
         DL  = PLIST(17)
         PHI = -DL/R
         CALL UPHELX(HLXCT,PHI,XV,PV)
         RDK = SQRT(XV(1)**2+XV(2)**2)
      ELSE
         RDK = 99999.
      ENDIF
C--
      IF ( R1.EQ.RCY1 .AND. NDOT.LE.0 .AND. AR.GT.ARTEST .AND.
     .     ABS(TNL).LT.TNTEST ) THEN
         IRT = 1
      ELSE
         CALL UHLXCY(RCYTK,ZCYTK,EPSL,HLXCT,PHI,XV,IRT)
      ENDIF
C--
      IF ( IRT.LE.0 ) THEN
         IF ( ABS(TNL).LT.TNTST2 ) THEN
            R2  = SQRT(XC**2+YC**2) + AR
         ELSE
            RETURN
         ENDIF
      ELSE IF ( IRT.EQ.1 ) THEN
         R2  = RCY2
      ELSE
         R2  = SQRT(XV(1)**2+XV(2)**2)
      ENDIF
      R2 = MIN(R2,RDK)
C--
C  Reject tracks with insufficient # hits.
C--
      NHIT = (R2-R1-0.1)/DSMPTK
      IF ( NHIT.LT.NSMPCT ) RETURN
C--
C  Move the pivot from the production point to the 1-st
C  measured point.
C--
      CALL UVCOPY(8,HLXCT,HLSMR)
      IF ( R1.EQ.RCY1 ) THEN
         RCY(1) = 1.E-2
         RCY(2) = RCYTK(1)
         CALL UHLXCY(RCY,ZCYTK,EPSL,HLXCT,PHI,XV,IRT)
         HLSMR(2) = FI0 + PHI
         CALL UVCOPY(3,XV(1),HLSMR(6))
      ENDIF
C--
C  Calculate inverse error matrix and smear helix parameters.
C--
      IF ( NHIT.GE.NSMPTK ) THEN
         CALL UVCOPY(10,SGBS,SGB)
         CALL UVCOPY(18,DADB1S,DADB1)
         CALL UVCOPY( 8,DADB2S,DADB2)
      ELSE
         CALL SMRHLE(NHIT,DSMPTK,SIGRF,SIGZE,SGB,DADB1,DADB2,IRT)
         IF ( IRT.LT.0 ) THEN
            IRET = - 2
            RETURN
         ENDIF
      ENDIF
C--
C  Calculate error matrix.
C--
      CALL UVZERO(30,ERROR)
      N = 0
      DO 400 J = 1, 3
         DO 40 I = 1, J
            N = N + 1
            DO 4 M = 1, 3
               ERROR(N) = ERROR(N)
     .                    + DADB1(I,M)*SGB(M)**2*DADB1(J,M)
4           CONTINUE
40       CONTINUE
400   CONTINUE
C--
      DO 500 J = 1, 2
         N = N + 3
         DO 50 I = 1, J
            N = N + 1
            DO 5 M = 1, 2
               ERROR(N) = ERROR(N)
     .                    + DADB2(I,M)*SGB(M+3)**2*DADB2(J,M)
5           CONTINUE
50       CONTINUE
500   CONTINUE
      CALL UVCOPY(30,ERROR(1),HLSMR(9))

C--
      DO 10 I = 1, 5
         SGB(I) = SGB(I)*RANN(ISEED)
10    CONTINUE
C--
      DO 200 I = 1, 3
         DEV = 0
         DO 20 J = 1, 3
            DEV = DEV + DADB1(I,J)*SGB(J)
20       CONTINUE
         HLSMR(I) = HLSMR(I) + DEV
200   CONTINUE
C--
      DO 300 I = 1, 2
         DEV = 0
         DO 30 J = 1, 2
            DEV = DEV + DADB2(I,J)*SGB(J+3)
30       CONTINUE
         HLSMR(I+3) = HLSMR(I+3) + DEV
300   CONTINUE
C--
C  Set other information.
C--
      HLSMR(39)  = 0
      HLSMR(40)  = 0
      HDATA(1,1) = 2*NHIT - 5
      HDATA(2,1) = 0
      HDATA(1,2) = 5
      HDATA(2,2) = 0
      HDATA(1,3) = 0
      HDATA(2,3) = 0
      CALL UVCOPY(3,HDATA,HLSMR(41))
      HLSMR(44)  = R1
      HLSMR(45)  = R2
C--
C  OK.
C--
      IRET = 0
C--
C  That's it.
C--
      RETURN
      END
