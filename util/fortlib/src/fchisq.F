C**********************************************************************
C* 
C* -----------------------------------------------------
C* Function FCHISQ( Y, SIGMAY, NPTS, NFREE, MODE, YFIT )
C* -----------------------------------------------------
C* 
C*(Note)
C*    This program is exactly same as the original Bevington's.
C* 
C*(Function)
C*    Evaluate reduced chi-square for fit to data.
C* 
C*(Input)
C*    Y      ... Array of data points.
C*    SIGMAY ... Array of standard deviations for data points.
C*    NPTS   ... Number of data points.
C*    NFREE  ... Number of degree of freedum.
C*    MODE   ... Determines method of weighting least square fit.
C*               +1  (Instrumental) Weight = 1./SIGMAY(i)**2
C*                0  (No weighting) Weight = 1.
C*               -1  (Statistical)  Weight = 1./Y(i)
C*    YFIT   ... Array of calculated values of Y.
C* 
C*(Output)
C*    FCHISQ ... Reduced chi-square.
C* 
C*(Subroutine and function required)
C*    none.
C* 
C*(Reference)
C*    "Data Reduction and Error Analysis for the Physical Sciences"
C*     By. P. R. Bevington  page 194
C* 
C*     Typed by A. Miyamoto     8-Nov-1985
C* 
C**********************************************************************
C* 
      FUNCTION FCHISQ(Y, SIGMAY, NPTS, NFREE, MODE, YFIT )
C  
      REAL*8    CHISQ, WEIGHT
      REAL*4    Y(NPTS), SIGMAY(NPTS), YFIT(NPTS)
C  
C---------------<< Entry Point >>--------------------------------------
C  
C (1) Initialize.
C  
      CHISQ  = 0.
      IF(NFREE.LE.0) THEN
        FCHISQ = 0.
        GO TO 400
      ENDIF
C  
C (2) Accumulate chi square.
C  
      DO 300 I = 1, NPTS
        IF(MODE .GT. 0) THEN
          WEIGHT = 1./SIGMAY(I)**2
        ELSEIF(MODE .EQ. 0 .OR. Y(I) .EQ. 0. ) THEN
          WEIGHT = 1.
        ELSE
          WEIGHT = ABS(1./Y(I))
        ENDIF
        CHISQ = CHISQ + WEIGHT*(Y(I)-YFIT(I))**2
300   CONTINUE
C  
C (3) Devide CHISQ by number of freedum.
C  
      FCHISQ = CHISQ/FLOAT(NFREE)
C  
C (4) return to caller.
C  
400   CONTINUE
      RETURN
      END
