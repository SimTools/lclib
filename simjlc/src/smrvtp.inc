C 
C  Parameter common to create Smeared VTX Track Parameter
C
      INTEGER*4       NSMPCV, NSMPVT
      REAL   *4       DSMPVT, RCYVT, ZCYVT
      REAL   *4       SIGRFV, SIGZEV

      COMMON /SMRVTP/ NSMPVT, NSMPCV, DSMPVT, RCYVT(2), ZCYVT(2),
     >                SIGRFV, SIGZEV


