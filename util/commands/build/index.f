C***********************************************************************
C* 
C*   Task library INDEX.
C* 
C*   A. Miyamoto   17-Oct-1985
C* 
C***********************************************************************
C  
      CHARACTER*137  ARGMNT
C  
C-----< Entry Point >--------------------------------------------------
C  
C (1) Parse input argument.
C  
C 1.1  Get parameter
C  
      CALL GPARM(ARGMNT,LARG)
CNGY  CALL RDPRM(ARGMNT,LARG)
   
      IF(ARGMNT(3:7).EQ.'INDEX') THEN
         IARG1 = 9
         IARGL = LARG - 2
      ELSE
        IARG1 = 1
        IARGL = LARG
      ENDIF
      ISEP  = INDEX(ARGMNT(IARG1:IARGL-1),ARGMNT(IARGL:IARGL))
      IOPER = IARG1 + ISEP
      IR    = INDEX(ARGMNT(IARG1:IOPER-2),ARGMNT(IOPER:IARGL-1))
      CALL SETRCD(IR)
      STOP
      END
