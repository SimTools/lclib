CC********************************************************************CC
C*                                                                    *C
C*====================----=========                                   *C
C*  Subroutine UCONF8(PROB,N,CHI2)                                    *C
C*====================----=========                                   *C
C*                                                                    *C
C* 1) Purpose                                                         *C
C*     Give confidence level.                                         *C
C*     < Caution >                                                    *C
C* 2) Inputs                                                          *C
C*     N        ; degrees of freedom.                                 *C
C*     CHI2     ; chi-squared.                                        *C
C* 3) Outputs                                                         *C
C*     PROB     ; confidence level.                                   *C
C* 4) Relation                                                        *C
C*     Invoked by                                                     *C
C*     Calls                                                          *C
C* 5) Update record                                                   *C
C*      01/01/64  C.Letertre  Origival version.                       *C
C*      10/23/81  B.Schorr    Revised.                                *C
C*      02/04/82              CERN PROGLIB# G100    PROB              *C
C*                            Version KERNFOR                         *C
C*      04/16/86  K.Fujii     Copied from YLM.NAW.FORT01.             *C
C*      08/29/86  K.Fujii     Double precision version.               *C
C*                                                                    *C
CC********************************************************************CC
   
C =========================================
      SUBROUTINE UCONF8 ( PROB, N, CHI2 )
C =========================================
   
      IMPLICIT REAL*8 ( A-H, O-Z )
      DATA  NCAL   /      0 /
      DATA  UPL    / 3400.D0 /
   
C--
C  Prepare numerical constants.
C--
      IF ( NCAL.EQ.0 ) THEN
         NCAL   = 1
         SRTOPI = DSQRT(2.D0/DACOS(-1.D0))
         ROOT2I = DSQRT(.5D0)
      ENDIF
C--
C  Reset cofidence level.
C--
      PROB = 0.D0
C--
C  Check if N and CHI2 physical.
C--
      IF ( N .LE. 0 .OR. CHI2 .LT. 0.D0 )  RETURN
C--
C  Check if Gaussian approximation valid.
C--
CERN      IF ( N .LE. 100 ) THEN
      IF ( N .LE.  60 ) THEN
         IF ( CHI2 .GT. UPL )  RETURN
C--
C  Should use exact formula.
C--
         SUM  = DEXP( -0.5D0*CHI2 )
         TERM = SUM
         M    = N/2
         IF ( 2*M .EQ. N ) THEN
C <Even>
            IF ( M .NE. 1 ) THEN
               DO 10 I = 2, M
                  TERM = 0.5D0*CHI2*TERM/(I-1)
                  SUM  = SUM + TERM
10             CONTINUE
            ENDIF
            PROB = SUM
         ELSE
C <Odd>
            SRTY = DSQRT (CHI2)
CERN            PROB=2.*(1.-FREQ (SRTY))
            PROB = DERFC(ROOT2I*SRTY)
            IF ( N .NE. 1 ) THEN
               IF ( N .NE. 3 ) THEN
                  DO 20 I = 1, M-1
                     TERM = TERM*CHI2/(2*I+1)
                     SUM  = SUM + TERM
20                CONTINUE
               ENDIF
               PROB = SRTOPI*SRTY*SUM + PROB
            ENDIF
         ENDIF
      ELSE
C--
C  Gaussian asymptotic formula.
C--
         SRTY = DSQRT(CHI2) - DSQRT(N-0.5D0)
         IF ( SRTY.LT.12.D0 ) PROB = 0.5D0*DERFC(SRTY)
      ENDIF
      RETURN
      END
