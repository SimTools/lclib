C  
      FUNCTION UCHISQ(NTERMS,PARAM)
C  
      COMMON /COMNXY/ NPNT,XI(11),YI(11)
      DIMENSION PARAM(3)
C  
      RO     = PARAM(1)
      XC     = PARAM(2)
      YC     = PARAM(3)
      CCHI   = 0.
      DO 1000 I = 1,NPNT
        RI    = SQRT((XI(I) - XC)**2 + (YI(I) - YC)**2)
        CCHI  = (RI - ABS(RO))**2 + CCHI
 1000 CONTINUE
      UCHISQ = CCHI
C  
      RETURN
      END
