      SUBROUTINE MOMNTX(ENERGY,MASS,COSTH,PHI , P)
C
C This subroutine sets up a four-momentum from the four inputs.
C
C INPUT:
C       real    ENERGY         : energy
C       real    MASS           : mass
C       real    COSTH          : cos(theta)
C       real    PHI            : azimuthal angle
C
C OUTPUT:
C       real    P(0:3)         : four-momentum
C
      REAL*8  P(0:3),ENERGY,MASS,COSTH,PHI,PP,SINTH
C
      P(0) = ENERGY
      IF (ENERGY.EQ.MASS) THEN
         P(1) = 0.D0
         P(2) = 0.D0
         P(3) = 0.D0
      ELSE
         PP=DSQRT((ENERGY-MASS)*(ENERGY+MASS))
         SINTH=DSQRT((1.D0-COSTH)*(1.D0+COSTH))
         P(3) = PP*COSTH
         IF (PHI.EQ.0.D0) THEN
            P(1) = PP*SINTH
            P(2) = 0.D0
         ELSE
            P(1) = PP*SINTH*DCOS(PHI)
            P(2) = PP*SINTH*DSIN(PHI)
         ENDIF
      ENDIF
      RETURN
      END
