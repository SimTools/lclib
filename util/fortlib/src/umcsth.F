CC********************************************************************CC
C*                                                                    *C
C*=========================================----------=====            *C
C*  Subroutine UMCSTH(COSTHT,AMT,AMDT,AMDF,AMF,COSTHF)                *C
C*=========================================----------=====            *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Gives COS(theta) and invariant mass for the hypothesis          *C
C* (Inputs)                                                           *C
C*       COSTHT : true COS(theta).                                    *C
C*       AMT    : true parent mass.                                   *C
C*       AMDT(*): true datghter masses.                               *C
C*       AMDF(*): assigned daughter masses.                           *C
C* (Outputs)                                                          *C
C*       AMF    : invariant mass for the hypothesis.                  *C
C*       COSTHF : COS(theta) for the hypothesis.                      *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       UPSTAR                                                       *C
C* (Update Record)                                                    *C
C*   7/29/85   K. Fujii   Original version.                           *C
C*                                                                    *C
CC********************************************************************CC
C  
C ===========================================----------=====
      SUBROUTINE UMCSTH(COSTHT,AMT,AMDT,AMDF,AMF,COSTHF)
C ===========================================----------=====
C  
      DIMENSION AMDT(2),AMDF(2)
C  
      AMT1   = AMDT(1)
      AMT2   = AMDT(2)
      AMF1   = AMDF(1)
      AMF2   = AMDF(2)
      SINTH2 = 1.-COSTHT**2
      IF(SINTH2.LT.0.) SINTH2 = 0.
      PSTR   = UPSTAR(AMT,AMT1,AMT2)
      PSTR2  = PSTR**2
      PSTH2  = PSTR2*SINTH2
C  
         A = SQRT(PSTR2+AMT1**2)/PSTR
         B = SQRT(PSTR2+AMT2**2)/PSTR
         A = A+COSTHT
         B = B-COSTHT
C  
C --- CALCULATE INVARIANT MASS
C  
      AMF = A*AMF1**2+B*AMF2**2+(A+B)*PSTH2
      AMF = AMF*(A+B)/(A*B)
      AMF = SQRT(AMF)
C  
C --- CALCULATE COS(THETA)
C  
      COSTHF = (A*AMF2-B*AMF1)**2+(A+B)**2*PSTH2
      COSTHF = COSTHF*((A*AMF2+B*AMF1)**2+(A+B)**2*PSTH2)
      COSTHF = 1./SQRT(COSTHF)
      COSTHF = COSTHF*((A*AMF2+B*AMF1)*(A*AMF2-B*AMF1)
     &         +(A+B)*(A-B)*PSTH2)
C  
      RETURN
      END
