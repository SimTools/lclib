      SUBROUTINE SXXXXX(P,NSS , SC)
C
C This subroutine computes a complex SCALAR wavefunction.               
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum of scalar boson          
C       integer NSS  = -1 or 1 : +1 for final, -1 for initial           
C                                                                       
C OUTPUT:                                                               
C       complex SC(3)          : scalar wavefunction                   S
C
      COMPLEX SC(3)
      REAL    P(0:3)
      INTEGER NSS
C
      SC(1) = CMPLX( 1.0 )
      SC(2) = CMPLX(P(0),P(3))*NSS
      SC(3) = CMPLX(P(1),P(2))*NSS
C
      RETURN
      END
