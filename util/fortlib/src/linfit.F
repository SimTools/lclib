CC*********************************************************************C
C*                                                                     *
C*----------------------------------------------=========              *
C* Subroutine LINFIT( X, Y, SIGMAY, NPTS, MODE, A, SIGMAA,             *
C*                                              B, SIGMAB, CHISQ )     *
C*----------------------------------------------==================     *
C*                                                                     *
C*(Caution)                                                            *
C*     Argument is deferent from the Bevington's original.             *
C*                                                                     *
C*  (Purpose)                                                          *
C*       Make a least-squares fit to data with a straight line         *
C*           Y = A + B*x                                               *
C*                                                                     *
C*  (Input)                                                            *
C*     X(i)       : x coordinate of the data points.                   *
C*     Y(i)       : y coordinate of the data points                    *
C*     SIGMAY(i)  : error of Y(i)                                      *
C*     NPTS       : Number of data points                              *
C*     MODE       : method of weighting least-squares fit              *
C*                  +1 (instrumental) WEIGHT = 1./SIGMAY(I)**2         *
C*                   0 (no weighting) WEIGHT = 1.                      *
C*                  -1 (statistical ) WEIGHT = 1./Y(I)                 *
C*                                                                     *
C*  (Output)                                                           *
C*       A      : The fitted line is expressed as                      *
C*       B      :       y = A + B*x                                    *
C*       SIGMAA : standard deviation of A                              *
C*       SIGMAB : standard deviation of B                              *
C*       CHISQ  : chi-square / number of reedom                        *
C*                                                                     *
C*(Reference)                                                          *
C*    "Data Reduction and Error Analysis for the Physical Sciences"    *
C*     By. P. R. Bevington  page 104                                   *
C*                                                                     *
C*     Modified from original version to output reduced-chi-square     *
C*     instead of linear-correlation coefficient.                      *
C*                                                                     *
C*  (Author)                                                           *
C*       T. Ishii     26-Sep-1985   Original version.                  *
C*       A. Miyamoto   8-Nov-1985   re-typed.                          *
C*                                                                     *
CC**********************************************************************
C  
      SUBROUTINE LINFIT(X,Y,SIGMAY,NPTS,MODE,A,SIGMAA,B,SIGMAB,CHISQ)
C  
      DOUBLE PRECISION SUM,SUMX,SUMY,SUMX2,SUMXY,SUMY2
      DOUBLE PRECISION XI,YI,WEIGHT,DELTA,VARNCE
      DIMENSION X(NPTS),Y(NPTS),SIGMAY(*)
C  
C ------------<< Entry Point >>----------------------------------------
C  
C  
C (1) Accumulate weighted sums.
C  
      SUM    = 0.
      SUMX   = 0.
      SUMY   = 0.
      SUMX2  = 0.
      SUMXY  = 0.
      SUMY2  = 0.
      DO 100 I = 1,NPTS
        XI     = X(I)
        YI     = Y(I)
        IF(MODE.GT.0) THEN
           WEIGHT = 1./SIGMAY(I)**2
        ELSEIF(MODE.EQ.0.OR.YI.EQ.0.) THEN
           WEIGHT = 1.
        ELSE
           WEIGHT = ABS(1./YI)
        ENDIF
        SUM    = SUM   + WEIGHT
        SUMX   = SUMX  + WEIGHT*XI
        SUMY   = SUMY  + WEIGHT*YI
        SUMX2  = SUMX2 + WEIGHT*XI*XI
        SUMXY  = SUMXY + WEIGHT*XI*YI
        SUMY2  = SUMY2 + WEIGHT*YI*YI
100   CONTINUE
C  
C (2) Calculate Coefficients and standard deviations.
C  
      DELTA  = SUM*SUMX2 - SUMX*SUMX
      A      = (SUMX2*SUMY - SUMX*SUMXY) / DELTA
      B      = (SUMXY*SUM  - SUMX*SUMY)  / DELTA
      C      = NPTS - 2
      VARNCE = (SUMY2 + A*A*SUM + B*B*SUMX2
     >         - 2.*(A*SUMY + B*SUMXY - A*B*SUMX)) / C
      CHISQ  = VARNCE
      IF(MODE.NE.0) THEN
        VARNCE = 1.
      ENDIF
      SIGMAA = DSQRT(VARNCE*SUMX2/DELTA)
      SIGMAB = DSQRT(VARNCE*SUM  /DELTA)
C  71 R      = (SUM*SUMXY - SUMX*SUMY) /
C    %       DSQRT(DELTA*(SUM*SUMY2 - SUMY*SUMY))
      RETURN
      END
