C 
C  Parameter common to create Smeared VTX Track Parameter
C
C
      INTEGER*4       NSMPCV, NSMPVT
      REAL   *4       DSMPVT, RCYVT, ZCYVT
      REAL   *4       SIGRFV, SIGZEV
      REAL   *4       VTXDEN

      COMMON /SMRVTP/ NSMPVT, NSMPCV, DSMPVT, RCYVT(2), ZCYVT(2),
     >                SIGRFV, SIGZEV, VTXDEN
C
C .. NSMPCV : Minimum number of vertex hits, below this number 
C             smearing of vertex track is abondoned.
C
C .. VTXDEN : VTX Density, namely VTX thickness by unit of radiation
C             length devided by lever arm of VTX layers. ( 1./cm )
C    DSMPVT : Distance between VTX layers.
C             
C .. These values are calculated in the subroutine SMRJIN.
C


