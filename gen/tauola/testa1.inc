C--
C  Special switch for tests of dGamma/dQ**2 in a1 decay
C     KEYA1 = 1 constant width of a1 and rho
C           = 2 free choice of rho propagator (defined in function 
C               FPIK) and free choice of a1 mass and width. 
C               function g(Q**2) (see formula 3.48 in Comp. Phys. 
C               Comm. 64 (1991) 275) hard coded both in Monte Carlo 
C               and in testing distribution.
C           = 3 function g(Q**2) hardcoded in the Monte Carlo
C               (it is timy to calculate!), but appropriately adjusted
C               in testing distribution.
C--
      COMMON /TESTA1/ KEYA1
      INTEGER*4       KEYA1
