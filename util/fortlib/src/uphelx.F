CC********************************************************************CC
C*                                                                    *C
C*=============================-----===                               *C
C*  Subroutine UPHELX(HELX,PHI,XV,PV)                                 *C
C*=============================-----===                               *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates position and momentum of a helix track at given      *C
C*    deflection angle.                                               *C
C* (Inputs)                                                           *C
C*      HELX(*)     : (R*4) ; helix parameters.                       *C
C*                            1-st 8 words in                         *C
C*                            Production:TPC;Track_Parameter.         *C
C*      PHI         : (R*4) ; deflection angle.                       *C
C* (Outputs)                                                          *C
C*      XV  (*)     : (R*4) ; positon vector.                         *C
C*      PV  (*)     : (R*4) ; momentum vector.                        *C
C* (Relation)                                                         *C
C*    Calls no subroutines.                                           *C
C* (Update Record)                                                    *C
C*    6/20/87  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UPHELX(HELX,PHI,XV,PV)
   
      IMPLICIT    REAL*8 ( A-H, O-Z )
#include "unumcn.inc"
#include "prfeld.inc"
      REAL   *4   HELX(8),PHI,XV(3),PV(3)
      REAL   *8   XC(3)
   
      EXTERNAL    PRBLK1, UCONST
   
C  
C========< Entry Point >================================================
C  
#if defined(HIUXF77) || defined(JavaVM)
      CALL PRBLK1
      CALL UCONST
#endif
C--
C  Convert helix parameterization.
C     X = XC(1) - R*cos(FI+FI0)
C     Y = XC(2) - R*sin(FI+FI0)
C     Z = XC(3) - R*TNL*FI
C--
      FI0    = HELX(2)
      TNL    = HELX(5)
      ICHG   = SIGN(1.1,HELX(3))
      PA     = 1.D0/HELX(3)
      R      = PTOR*PA
      PA     = DABS(PA)
      XC(1)  = HELX(6) + ( R + HELX(1) )*DCOS(FI0)
      XC(2)  = HELX(7) + ( R + HELX(1) )*DSIN(FI0)
      XC(3)  = HELX(8) +       HELX(4)
      FI     = PHI
C--
C  Get X0 and TNV(tangent verctor at X0).
C--
      CSF    = DCOS(FI+FI0)
      SNF    = DSIN(FI+FI0)
      XV(1)  = XC(1) - R*CSF
      XV(2)  = XC(2) - R*SNF
      XV(3)  = XC(3) - R*TNL*FI
      PV(1)  = -PA*SNF
      PV(2)  =  PA*CSF
      PV(3)  =  PA*TNL
C--
C  That's it.
C--
      RETURN
      END
