CC********************************************************************CC
C*                                                                    *C
C*=============================-----==                                *C
C*  Subroutine UCPSER(PHI,HELX,PSERR)                                 *C
C*=============================-----==                                *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculate position error matrix at specified deflection angle.  *C
C* (Inputs)                                                           *C
C*      PHI       : R*4 ; deflection angle in degree measured from    *C
C*                        pivot postion ( HELX(6-8) ).                *C
C*      HELX(*)   : R*4 ; helix parameter vector and its error matrix.*C
C*                        The 1-st 38 words in                        *C
C*                        Production:TPC;Track_Parameter.             *C
C* (Output)                                                           *C
C*      PSERR(I,J): R*8 ; position error matrix.                      *C
C*                        I,J = (X,Y,Z) = (1,2,3)                     *C
C* (Relation)                                                         *C
C*    Calls UCDXDA, UVCOPY.                                           *C
C* (Update Record)                                                    *C
C*    6/12/87  K.Fujii            Original version.                   *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UCPSER(PHI,HELX,PSERR)
   
      IMPLICIT REAL*8 ( A-H, O-Z )
   
#include "unumcn.inc"
#include "prfeld.inc"
      REAL*4 PHI,HELX(38)
      REAL*8 ERRMAT(15),PSERR(3,3)
      REAL*8 DXDA(3,5), EE(5,5)
   
      EXTERNAL UCONST, PRBLK1
   
C  
C========< Entry Point >================================================
C  
C--
C  Copy error matrix to ERRMAT.
C--
      CALL UVCOPY(30,HELX(9),ERRMAT)
C--
C  Calculate @X/@a matrix at phi = PHI.
C--
      CALL UCDXDA(PHI,HELX,DXDA)
C--
C  Symmetrize error matrix for helix parameters.
C--
      N = 0
      DO 100 J = 1, 5
         DO 10 I = J, 5
            N = N + 1
            EE(I,J) = ERRMAT(N)
            EE(J,I) = EE(I,J)
10       CONTINUE
100   CONTINUE
C--
C  Calculate error matrix for position at phi = PHI.
C--
      DO 2000 I = 1, 3
         DO 200 J = 1, 3
            PSERR(I,J) = 0.D0
            DO 20 L = 1, 5
               DO 2 K = 1, 5
                  PSERR(I,J) = PSERR(I,J)
     .                         + DXDA(I,K)*EE(K,L)*DXDA(J,L)
2              CONTINUE
20          CONTINUE
200      CONTINUE
2000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
