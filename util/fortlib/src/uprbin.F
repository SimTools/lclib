CC*********************************************************************C
C*                                                                     *
C*============------================                                   *
C* FUNCTION   UPRBIN(S1,S2)                                            *
C*============------================                                   *
C*  (Purpose)                                                          *
C*       To estimate the probability exist in a region of              *
C*       Gaussian distribution.                                        *
C*  (Input)                                                            *
C*       (s1,s2) are the region in normal Gausian distribution.        *
C*  (Onput)                                                            *
C*       UPRBIN ; the probability.                                     *
C*  (Author)                                                           *
C*       H. KICHIMI  AUG  1986                                         *
C*       H. KICHIMI  JUNE, 1987   MODIFIED TO CALL UCONFL FOR UPROB    *
C*                                                                     *
CC**********************************************************************
C  
       FUNCTION   UPRBIN(S1,S2)
C  
        CHKSGN=S1*S2
        XI1=S1*S1
        XI2=S2*S2
        CALL  UCONFL(PROB1,1,XI1)
        CALL  UCONFL(PROB2,1,XI2)
        IF(CHKSGN.GT.0.0)  THEN
        PROB=0.5*ABS(PROB1-PROB2)
        ELSE
        PROB=1.0-0.5*(PROB1+PROB2)
        ENDIF
C  
        UPRBIN=PROB
        RETURN
        END
