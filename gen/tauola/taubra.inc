C--
C  LIST OF BRANCHING RATIOS
C  
C  Normalised to e nu nutau channel
C                  enu   munu   pinu  rhonu   A1nu   Knu    K*nu   pi'
C   DATA JLIST  /    1,     2,     3,     4,     5,     6,     7,
C   DATA GAMPRT /1.000,0.9730,0.6054,1.2432,0.8432,0.0432,O.O811,0.616
C
C  Multipion decays
C    Conventions of particles names
C  
C                 K-,P-,K+,  K0,P-,KB,  K-,P0,K0
C                  3, 1,-3  , 4, 1,-4  , 3, 2, 4  ,
C                 P0,P0,K-,  K-,P-,P+,  P-,KB,P0
C                  2, 2, 3  , 3, 1,-1  , 1,-4, 2  ,
C                 ET,P-,P0   P-,P0,GM
C                  9, 1, 2  , 1, 2, 8
C
C    Outgoing multiplicity and flavors of multi-pion /multi-K modes    
C
C      DIMENSION NOPIK(6,NMODE),NPIK(NMODE)
C      DATA   NPIK  /                4,                    4,  
C     1                              5,                    5,
C     2                              6,                    6,
C     3                              3,                    3,            
C     4                              3,                    3,            
C     5                              3,                    3,            
C     6                              3,                    3,  
C     7                              2                         /         
C      DATA  NOPIK / -1,-1, 1, 2, 0, 0,     2, 2, 2,-1, 0, 0,  
C     1              -1,-1, 1, 2, 2, 0,    -1,-1,-1, 1, 1, 0,  
C     2              -1,-1,-1, 1, 1, 2,    -1,-1, 1, 2, 2, 2, 
C     3              -3,-1, 3, 0, 0, 0,    -4,-1, 4, 0, 0, 0,  
C     4              -3, 2,-4, 0, 0, 0,     2, 2,-3, 0, 0, 0,  
C     5              -3,-1, 1, 0, 0, 0,    -1, 4, 2, 0, 0, 0,  
C     6               9,-1, 2, 0, 0, 0,    -1, 2, 8, 0, 0, 0,
C     7              -3, 4, 0, 0, 0, 0                         /
C--
      COMMON / TAUBRA / GAMPRT(30), JLIST(30), NCHAN
      REAL   *4         GAMPRT
      INTEGER*4		JLIST, NCHAN
