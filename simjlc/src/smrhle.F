C***********************************************************************
C*
C*=======================================--------------------===
C* Subroutine SMRHLE(NHIT,DSMP,SGRF,SGZE,SGB,DADB1,DADB2,IRET)
C*=======================================--------------------===
C*
C* (Purpose)
C*     Diagonalize error matrix fro helix smearing.
C* (Inputs)
C*        NHIT       : (I*4) :  # sampling points.
C*        DSMP       : (R*4) :  sampling step.
C*        SGRF       : (R*4) :  sigma_(r-phi).
C*        SGZE       : (R*4) :  sigma_z.
C* (Output)
C*        SGB(1-5)   : (R*8) :  sigma of independent parameters.
C*        DADBi(I,J) : (R*8) :  a matrix which diagonalizes inverse
C*                           :  error matrix.
C*                           :  i = (1,2) = (r-phi,r-z).
C*        IRET       : (I*4) :  return code.
C*                           :  IRET = 0 ; OK.
C*                           :       < 0 ; error.
C* (Relation)
C*     Called by SMRHLX.
C*     Calls the following routines;
C*        UVCOPY and DSEIG1.
C* (Updaterecord)
C*     5/26/90  K.Fujii           Original version.
C*
C***********************************************************************
 
      SUBROUTINE SMRHLE(NHIT,DSMP,SGRF,SGZE,SGB,DADB1,DADB2,IRET)
 
      IMPLICIT REAL*8    ( A-H, O-Z )
#include "prfeld.inc"
      INTEGER*4  NHIT, IRET
      REAL   *4  DSMP, SGRF, SGZE
      REAL   *8  SGB(5), DADB1(3,3), DADB2(2,2)
C--
      REAL   *8  DD(6), VW(6), DDS(3)
C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Calculate inverse error matrix and smear helix parameters.
C--
      EN    = NHIT - 1
      ENP1  = NHIT
      TENP1 = 2*EN + 1
      EL    = EN*DSMP
      EL2   = EL*EL
C--
C  R-Phi part.
C--
      DD(1) =  ENP1
      DD(2) = -EL*ENP1/2
      DD(3) =  EL2*ENP1*TENP1/(6*EN)
      DD(4) =  DD(3)/(2*PTOR)
      DD(5) = -EL*EL2*ENP1**2/(8*EN*PTOR)
      DD(6) = (EL2/(2*PTOR))**2*ENP1*TENP1*(3*EN*ENP1-1)/(30*EN**3)
      CALL UVCOPY(6,DD(1),DDS(1))
C--
      CALL DSEIG1(DD,3,SGB,DADB1,3,NE,VW,IRT)
      IF ( IRT.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in SMRHLE >>>>>>> '
         PRINT *, '    DSEIG1 retuns IRT = ', IRT
         IRET = -1
         RETURN
      ENDIF
C--
      DO 10 I = 1, 3
         SGB(I) = SGRF/SQRT(SGB(I))
10    CONTINUE
C--
C  R-Z part.
C--
      DDS(2) = -DDS(2)
C--
      CALL DSEIG1(DDS,2,SGB(4),DADB2,2,NE,VW,IRT)
      IF ( IRT.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in SMRHLE >>>>>>> '
         PRINT *, '    DSEIG1 retuns IRT = ', IRT
         IRET = -2
         RETURN
      ENDIF
C--
      DO 30 I = 4, 5
         SGB(I) = SGZE/SQRT(SGB(I))
30    CONTINUE
C--
C  That's it.
C--
      RETURN
      END
