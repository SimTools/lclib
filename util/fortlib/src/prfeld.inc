CC*******************************************************************CC
C*                                                                   *C
C*=====================                                              *C
C*  COMMON  /PRFELD/                                                 *C
C*=====================                                              *C
C*                                                                   *C
C* (Contents)                                                        *C
C*     Momentum to radius conversion factor.                         *C
C* (Update Record)                                                   *C
C*   10/14/86   K. Fujii    Original version                         *C
C*   10/21/86   A. Miyamoto Add BFIELD and ROPT.                     *C
C*   10/07/87   A. Miyamoto BFIELD modified to 1.01085 Tesla         *C
C*                          ( Value determined from E4.R305 NMR)     *C
C*                                                                   *C
CC*******************************************************************CC
C
      COMMON /PRFELD/ BFIELD, PTOR, ROPT
      REAL*4          BFIELD, PTOR, ROPT
C
C=    DATA            BFIELD/10.1085E0/
C=    DATA            PTOR  /329.9837713/
C=    DATA            ROPT  /3.03045206E-3/
COLD=    DATA            BFIELD/10.E0/
COLD=    DATA            PTOR  /333.5640952/
COLD=    DATA            ROPT  /2.99792458E-3/
C
C     BFIELD    ; Magnetic field strength (KGauss)
C     PTOR      ; Convert momentum to radius.
C                   R (cm)   = PTOR * Pt (GeV)
C         PTOR = 1./c/B    ( c : Light velocity, B = Mag. field.)
C              = 1./2.99792458/BFILED
C
C     ROPT      ; Convert radius to momentum.
C                   Pt (GeV) = ROPT * R (cm)
