C***********************************************************************
C*
C*==============================================================
C* Subroutine SMRHLM(NHIT,DSMP,SGRF,SGZE,XRAD,HLX,MODE, 
C*                   SGB,DADB1,DADB2,IRET)
C*===================----------------------------------------===
C*
C* (Purpose)
C*     Diagonalize error matrix fro helix smearing.
C* (Inputs)
C*        NHIT       : (I*4) :  # sampling points.
C*        DSMP       : (R*4) :  sampling step.
C*        SGRF       : (R*4) :  sigma_(r-phi).
C*        SGZE       : (R*4) :  sigma_z.
C*        XRAD       : (R*4) :  matterial thickness in unit of 
C*                              radiation length
C*        HLX(5)     : (R*4) :  Helix parameter (dr, phi0, kappa,...)
C*        MODE       : (I*4) :  How to calculate error matrix.
C*                     = 1 , Use exact formular
C*                     > 2 , ( to be implemented later )
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
C*     5/26/99  A.Miyamoto  	Created using SMRHLE
C*     5/28/01  K.Fujii		EL = EN*DSMP instead of EL = NHIT*DSMP.
C*
C***********************************************************************
 
      SUBROUTINE SMRHLM(NHIT,DSMP,SGRF,SGZE, XRAD, HLX, MODE,
     >                  SGB,DADB1,DADB2,IRET)
 
      IMPLICIT REAL*8    ( A-H, O-Z )
#include "prfeld.inc"
      INTEGER*4  NHIT, IRET, MODE
      REAL   *4  DSMP, SGRF, SGZE
      REAL   *4  XRAD, HLX(5)
      REAL   *8  SGB(5), DADB1(3,3), DADB2(2,2)
C--
      REAL   *8  DD(6), VW(6), DDS(3)
      REAL   *8  DDM(6), DDMS(6)
C
      PARAMETER  ( CFACT=0.0141*0.0141 )
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
C      EL    = NHIT*DSMP
      EL2   = EL*EL
C--
C  R-Phi part.
C--
C      DD(1) =  ENP1
C      DD(2) = -EL*ENP1/2
C      DD(3) =  EL2*ENP1*TENP1/(6*EN)
C      DD(4) =  DD(3)/(2*PTOR)
C      DD(5) = -EL*EL2*ENP1**2/(8*EN*PTOR)
C      DD(6) = (EL2/(2*PTOR))**2*ENP1*TENP1*(3*EN*ENP1-1)/(30*EN**3)
C
C 
      EN2=EN*EN
      EN3=EN2*EN
      EN4=EN2*EN2
      EL3=EL2*EL
      EDENOM=1./((EN+1)*(EN+2)*(EN+3))
C
C ... Error matrix due to measurement error
      DDM(1) = 3*(3*EN2+3*EN+2)*EDENOM
      DDM(2) = (36*EN2+18*EN)*EDENOM/EL
      DDM(3) = 12*EN*(2*EN+1)*(8*EN-3)*EDENOM/(EL2*(EN-1))
      DDM(4) = 60*PTOR*EN2*EDENOM/EL2
      DDM(5) = 360*PTOR*EN3*EDENOM/(EL3*(EN-1))
      DDM(6) = 720*PTOR*PTOR*EN3*EDENOM/(EL2*EL2*(EN-1))
C
C ... Error matrix due to multiple-scattering
      DDMS(1)=(2*EN3+489*EN2+481*EN-1299)*(EN-1)*(EN-2)
     >       *EDENOM*EL3/(1260*EN2)
      DDMS(2)=(14*EN4+2631*EN3+4526*EN2-3025*EN+834)*(EN-2)*EL2
     >       *EDENOM/(840*EN2)
      DDMS(3)=(16*EN4*EN2+1862*EN4*EN+1299*EN4-6182*EN3
     >        +2065*EN2-1886*EN-24)*EL*EDENOM
     >        /(70*EN2*(EN-1))
      DDMS(4)=PTOR*EL*(EN3+600*EN2+1121*EN-726)*(EN-2)
     >       *EDENOM/(84*EN)
      DDMS(5)=PTOR*(3*EN3*EN2+869*EN4+787*EN3-2969*EN2+650*EN-480)
     >  *EDENOM/(14*EN*(EN-1)) 
      DDMS(6)=10*PTOR*PTOR*EN*(EN3+112*EN2+135*EN-362)
     >  *EDENOM/(7*EL*(EN-1))

      TANLS1 = 1+HLX(5)*HLX(5)
      P2NOW  = TANLS1/(HLX(3)*HLX(3))
      FACTK  = TANLS1*SQRT(TANLS1)*CFACT*XRAD/P2NOW

c      print *,' el=',el,' en=',en,' sgrf=',sgrf
c      print *,' p2now=',p2now,' factk=',factk,' xrad=',xrad

      DO 100 I = 1, 6
         DD(I)=SGRF*SGRF*DDM(I)+FACTK*DDMS(I)
         DD1=SGRF*SGRF*DDM(I)
         DD2=FACTK*DDMS(I)
c         print *,' I=',I,' DD(I)=',DD(I),' DDM(I)=',DD1,
c     >  ' DDMS(I)=',DD2,' DDMS(I)=',DDMS(I)
 100  CONTINUE

C--
      CALL DSEIG1(DD,3,SGB,DADB1,3,NE,VW,IRT)
      IF ( IRT.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in SMRHLM >>>>>>> '
         PRINT *, '    DSEIG1 retuns IRT = ', IRT
         IRET = -1
         RETURN
      ENDIF
C--
      DO 10 I = 1, 3
         SGB(I) = SQRT(SGB(I))
10    CONTINUE
C--
C  R-Z part.
C--
C      DDS(2) = -DDS(2)

      EDENOM = 1/((EN+1)*(EN+2))
      DDM(1) = 2*(2*EN+1)*EDENOM
      DDM(2) = -6*EN*EDENOM/EL
      DDM(3) = 12*EN*EDENOM/EL2

      DDMS(1) = EL3*(EN-1)*(6*EN3+177*EN2+163*EN-172)*EDENOM
     >       / ( 630*EN2 )
      DDMS(2) = EL2*(1-EN)*(22*EN3+390*EN2+523*EN-122)*EDENOM
     >       / (420*EN2)
      DDMS(3) = EL*(26*EN4+265*EN3+287*EN2-166*EN+8)*EDENOM
     >       / (70*EN2) 


      DO 200 I = 1, 3
         DDS(I)=SGZE*SGZE*DDM(I)+FACTK*DDMS(I)
 200  CONTINUE
C--
      CALL DSEIG1(DDS,2,SGB(4),DADB2,2,NE,VW,IRT)
      IF ( IRT.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in SMRHLM >>>>>>> '
         PRINT *, '    DSEIG1 retuns IRT = ', IRT
         IRET = -2
         RETURN
      ENDIF
C--
      DO 30 I = 4, 5
         SGB(I) = SQRT(SGB(I))
30    CONTINUE
C--
C  That's it.
C--
      RETURN
      END
