      COMMON /SMRRES/  SIGRF , SIGZE,
     .                 SGEMB0, SGEMB, SGEME0, SGEME, SGHDB0, SGHDB,
     .                 SGHDE0, SGHDE
      REAL*4           SIGRF , SIGZE,
     .                 SGEMB0, SGEMB, SGEME0, SGEME, SGHDB0, SGHDB,
     .                 SGHDE0, SGHDE
C--
C  Tracking resolution. Unit in cm.
C     Sigma_(r-phi,Z)
C--
C=    DATA             SIGRF    / 1.E-2 /
C=   .                 SIGZE    / 3.E-1 /
C--
C  EM calorimeter resolution ( sigma_E/E at 1 GeV ).
C--
C=   .                 SGEMB0   / 1.0E-2 /
C=   .                 SGEMB    / 1.5E-1 /
C=   .                 SGEME0   / 1.0E-2 /
C=   .                 SGEME    / 1.5E-1 /
C--
C  HD calorimeter resolution ( sigma_E/E at 1 GeV ).
C--
C=   .                 SGHDB0   / 1.0E-2 /
C=   .                 SGHDB    / 4.0E-1 /
C=   .                 SGHDE0   / 1.0E-2 /
C=   .                 SGHDE    / 4.0E-1 /
