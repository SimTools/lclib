CC********************************************************************CC
C*                                                                    *C
C*===========------============                                       *C
C*  Function UPSTAR(AM,AM1,AM2)                                       *C
C*===========------============                                       *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   Calculates the absolute value of daughter momenta in the parent  *C
C*   rest frame for 2-body decay.                                     *C
C* (Inputs)                                                           *C
C*     AM    = mass of the parent.                                    *C
C*     AM1   = mass of the 1-st daughter.                             *C
C*     AM2   = mass of the 2-nd daughter.                             *C
C* (Inputs)                                                           *C
C*     UPSTAR = absolute value of the daugher momenta in the parent   *C
C*              rest frame.                                           *C
C* (Relation)                                                         *C
C*   Calls                                                            *C
C*      UDELTA                                                        *C
C* (Relation)                                                         *C
C*   7/29/85  K. Fujii  Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =============------============
      FUNCTION UPSTAR(AM,AM1,AM2)
C =============------============
C  
      IF(AM.EQ.0.) GO TO 10
      X=AM*AM
      Y=AM1*AM1
      Z=AM2*AM2
      UPSTAR=UDELTA(X,Y,Z)
      UPSTAR=0.5*SQRT(UPSTAR)/AM
      RETURN
   10 UPSTAR=0.
      RETURN
      END
