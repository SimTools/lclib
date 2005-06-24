C***********************************************************************
C*
C*================================----===
C* Subroutine SMRSHW(ITYP,ESUM,XY,EPRT)
C*================================----===
C*
C* (Purpose)
C*     Calculate shower energy deposit in a rectangular cell,
C*     (X_min,X_max)*(Y_min,Y_max), with the shower center at (0,0).
C* (Inputs)
C*        ITYP     : (I*4) :  (1,2) = (EM,HD).
C*        ESUM     : (R*4) :  shower total energy.
C*        XY(1,1)  : (R*4) :  X_min.
C*          (2,1)  : (R*4) :  X_max.
C*          (1,2)  : (R*4) :  Y_min.
C*          (2,2)  : (R*4) :  Y_max.
C* (Output)
C*        EPRT     : (R*4) :  energy deposit in the cell.
C* (Relation)
C*     Called by SMREMC and SMRHDC.
C*     Calls no subroutines.
C* (Updaterecord)
C*     5/22/90  K.Fujii           Original version.
C*     6/22/05  A.Miyamoto        Shower shape parameters are obtained
C*                                from common /smrclp/
C*
C***********************************************************************
 
      SUBROUTINE SMRSHW(ITYP,ESUM,XY,EPRT)
#include "smrclp.inc"
      INTEGER*4  ITYP
      REAL   *4  ESUM, XY(2,2), EPRT
      REAL   *4  A(2), AL(2)
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Initialize a lateral shower shape function:
C     f(x) = ANM(1,i)*EXP(-!x!/ALM(1,i)) + ANM(2,i)*EXP(-!x!/ALM(2,i))
C                   core part                 peripheral part
C  where i = (1,2) = (EM,HD).
C--
C--
C  Decide calorimeter type.
C--
      IF ( ITYP.NE.1 .AND. ITYP.NE.2 ) THEN
         PRINT *, ' >>>>>> Error in SMRSHW >>>>>>> '
         PRINT *, '     ITYP = ', ITYP, ' is not supported.'
         PRINT *, '     STOP.'
         STOP
      ENDIF
      AL(1) = CLSSH_ALM(1,ITYP)
      AL(2) = CLSSH_ALM(2,ITYP)
      A(1)  = AL(1)*CLSSH_ANM(1,ITYP)
      A(2)  = AL(2)*CLSSH_ANM(2,ITYP)
C--
C  Integrate f(x)*f(y) over the cell.
C--
      PRT = 1
      DO 1000 K = 1, 2
         PX = 0
         IS = 1
         DO 100 J = 1, 2
            X  = XY(J,K)
            AX = ABS(X)
            IS = IS*(-1)
            IX = IS*SIGN(1.1,X)
            DO 10 I = 1, 2
               PX = PX + IX*A(I)*(1-EXP(-AX/AL(I)))
10          CONTINUE
100      CONTINUE
         PRT = PRT*PX
1000  CONTINUE
C--
C  Calculate EPRT.
C--
      EPRT = ESUM*PRT
C--
C  That's it.
C--
      RETURN
      END
