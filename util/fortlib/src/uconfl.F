CC********************************************************************CC
C*                                                                    *C
C*====================----=========                                   *C
C*  Subroutine UCONFL(PROB,N,CHI2)                                    *C
C*====================----=========                                   *C
C*                                                                    *C
C* 1) Purpose                                                         *C
C*     GiveS confidence level.                                        *C
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
C*                                                                    *C
CC********************************************************************CC
   
C =========================================
      SUBROUTINE UCONFL ( PROB, N, CHI2 )
C =========================================
   
   
      DATA  SRTOPI, UPL / 0.7978846, 340. /,  ROOT2I / 0.70710678 /
   
      PROB=0.
      IF (N .LE. 0 .OR. CHI2 .LT. 0.) RETURN
CERN      IF (N .GT. 100) GO TO 30
      IF (N .GT. 60) GO TO 30
      IF (CHI2 .GT. UPL)  RETURN
      SUM=EXP(-0.5*CHI2)
      TERM=SUM
      M=N/2
      IF (2*M .NE. N) GO TO 1
C--                ENTRY IF N IS EVEN
      IF (M .EQ. 1) GO TO 11
      DO 10 I=2,M
      TERM=0.5*CHI2*TERM/(I-1)
   10 SUM=SUM+TERM
   11 PROB=SUM
      RETURN
C--                ENTRY IF N IS ODD
    1 SRTY=SQRT (CHI2)
CERN      PROB=2.*(1.-FREQ (SRTY))
      PROB=ERFC(ROOT2I*SRTY)
      IF (N .EQ. 1) RETURN
      IF (N .EQ. 3) GO TO 21
      K=M-1
      DO 20 I=1,K
      TERM=TERM*CHI2/(2*I+1)
   20 SUM=SUM+TERM
   21 PROB=SRTOPI*SRTY*SUM+PROB
      RETURN
C--       USE SIMPLE GAUSSIAN ASYMPTOTIC FORMULA
   30 SRTY= SQRT(CHI2) - SQRT(N-0.5)
      IF (SRTY.LT.12.) PROB=0.5*ERFC(SRTY)
      RETURN
      END
