
      SUBROUTINE INITDK
C ----------------------------------------------------------------------
C     INITIALISATION OF TAU DECAY PARAMETERS  and routines
C
C     called by : KORALZ
C ----------------------------------------------------------------------
C
#include "decpar.inc"
#include "parmas.inc"
#include "taubra.inc"
#include "taukle.inc"
#include "decomp.inc"
C
      REAL*4 PI
C
C LIST OF BRANCHING RATIOS
#ifdef AM
normalised to e nu nutau channel
#endif 
#ifdef AM
                 enu   munu   pinu  rhonu   A1nu   Knu    K*nu   pi'
#endif 
#ifdef AM
  DATA JLIST  /    1,     2,     3,     4,     5,     6,     7,
#endif 
#ifdef AM
  DATA GAMPRT /1.000,0.9730,0.6054,1.2432,0.8432,0.0432,O.O811,0.616
#endif 
CAM
#ifdef AM
 multipion decays
#endif 
C
C    conventions of particles names
C                 K-,P-,K+,  K0,P-,KB,  K-,P0,K0
C                  3, 1,-3  , 4, 1,-4  , 3, 2, 4  ,
C                 P0,P0,K-,  K-,P-,P+,  P-,KB,P0
C                  2, 2, 3  , 3, 1,-1  , 1,-4, 2  ,
C                 ET,P-,P0   P-,P0,GM
C                  9, 1, 2  , 1, 2, 8
C
      DIMENSION NOPIK(6,NMODE),NPIK(NMODE)
#ifdef AM
  outgoing multiplicity and flavors of multi-pion /multi-K modes    
#endif 
      DATA   NPIK  /                4,                    4,  
     1                              5,                    5,
     2                              6,                    6,
     3                              3,                    3,            
     4                              3,                    3,            
     5                              3,                    3,            
     6                              3,                    3,  
     7                              2                         /         
      DATA  NOPIK / -1,-1, 1, 2, 0, 0,     2, 2, 2,-1, 0, 0,  
     1              -1,-1, 1, 2, 2, 0,    -1,-1,-1, 1, 1, 0,  
     2              -1,-1,-1, 1, 1, 2,    -1,-1, 1, 2, 2, 2, 
     3              -3,-1, 3, 0, 0, 0,    -4,-1, 4, 0, 0, 0,  
     4              -3, 2,-4, 0, 0, 0,     2, 2,-3, 0, 0, 0,  
     5              -3,-1, 1, 0, 0, 0,    -1, 4, 2, 0, 0, 0,  
     6               9,-1, 2, 0, 0, 0,    -1, 2, 8, 0, 0, 0,
     7              -3, 4, 0, 0, 0, 0                         /
C LIST OF BRANCHING RATIOS
      NCHAN = NMODE + 7
      DO 1 I = 1,30
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ. 1) GAMPRT(I) = 1.0000
        IF(I.EQ. 2) GAMPRT(I) = 0.9696
        IF(I.EQ. 3) GAMPRT(I) = 0.6058
        IF(I.EQ. 4) GAMPRT(I) = 1.3274
        IF(I.EQ. 5) GAMPRT(I) = 0.7194
        IF(I.EQ. 6) GAMPRT(I) = 0.0398
        IF(I.EQ. 7) GAMPRT(I) = 0.07015
        IF(I.EQ. 8) GAMPRT(I) = 0.0840
        IF(I.EQ. 9) GAMPRT(I) = 0.0172
        IF(I.EQ.10) GAMPRT(I) = 0.0634
        IF(I.EQ.11) GAMPRT(I) = 0.0289
        IF(I.EQ.12) GAMPRT(I) = 0.00422
        IF(I.EQ.13) GAMPRT(I) = 0.00419
        IF(I.EQ.14) GAMPRT(I) = 0.00606
        IF(I.EQ.15) GAMPRT(I) = 0.00560
        IF(I.EQ.16) GAMPRT(I) = 0.000516
        IF(I.EQ.17) GAMPRT(I) = 0.00612
        IF(I.EQ.18) GAMPRT(I) = 0.0315
        IF(I.EQ.19) GAMPRT(I) = 0.0319
        IF(I.EQ.20) GAMPRT(I) = 0.0109
        IF(I.EQ.21) GAMPRT(I) = 0.00317
        IF(I.EQ.22) GAMPRT(I) = 0.0181
        IF(I.EQ. 8) NAMES(I-7)='  TAU-  --> 2PI-,  PI0,  PI+   '
        IF(I.EQ. 9) NAMES(I-7)='  TAU-  --> 3PI0,        PI-   '
        IF(I.EQ.10) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 2PI0   '
        IF(I.EQ.11) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,        '
        IF(I.EQ.12) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,  PI0   '
        IF(I.EQ.13) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 3PI0   '
        IF(I.EQ.14) NAMES(I-7)='  TAU-  -->  K-, PI-,  K+      '
        IF(I.EQ.15) NAMES(I-7)='  TAU-  -->  K0, PI-, K0B      '
        IF(I.EQ.16) NAMES(I-7)='  TAU-  -->  K-,  K0, PI0      '
        IF(I.EQ.17) NAMES(I-7)='  TAU-  --> PI0, PI0,  K-      '
        IF(I.EQ.18) NAMES(I-7)='  TAU-  -->  K-, PI-, PI+      '
        IF(I.EQ.19) NAMES(I-7)='  TAU-  --> PI-, K0B, PI0      '
        IF(I.EQ.20) NAMES(I-7)='  TAU-  --> ETA, PI-, PI0      '
        IF(I.EQ.21) NAMES(I-7)='  TAU-  --> PI-, PI0, GAM      '
        IF(I.EQ.22) NAMES(I-7)='  TAU-  -->  K-,  K0           '
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   1  CONTINUE
      DO I=1,NMODE
        MULPIK(I)=NPIK(I)
        DO J=1,MULPIK(I)
         IDFFIN(J,I)=NOPIK(J,I)
        ENDDO
      ENDDO
C
C
C --- COEFFICIENTS TO FIX RATIO OF:
C --- A1 3CHARGED/ A1 1CHARGED 2 NEUTRALS MATRIX ELEMENTS (MASLESS LIM.)
C --- PROBABILITY OF K0 TO BE KS
C --- PROBABILITY OF K0B TO BE KS
C --- RATIO OF COEFFICIENTS FOR K*--> K0 PI-
C --- ALL COEFFICENTS SHOULD BE IN THE RANGE (0.0,1.0)
C --- THEY MEANING IS PROBABILITY OF THE FIRST CHOICE ONLY IF ONE
C --- NEGLECTS MASS-PHASE SPACE EFFECTS
      BRA1=0.5
      BRK0=0.5
      BRK0B=0.5
      BRKS=0.6667
C
C --- remaining constants
      PI =4.*ATAN(1.)
      GFERMI = 1.16637E-5
      CCABIB = 0.975
      GV     = 1.0
      GA     =-1.0
C ZW 13.04.89 HERE WAS AN ERROR
      SCABIB = SQRT(1.-CCABIB**2)
      GAMEL  = GFERMI**2*AMTAU**5/(192*PI**3)
C
C      CALL DEXAY(-1)
C
      RETURN
      END
