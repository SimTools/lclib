C
      REAL*4  CLSPAR, CLSSH_ANM, CLSSH_ALM
      COMMON /SMRCLP/ CLSPAR(5,4), CLSSH_ANM(2,2), CLSSH_ALM(2,2)
      
C
C  /SMRCLP/ Parameter for Clustering.
C           CLSPAR(i,1) ; For Barrel EMC
C                 (i,2) ; For EndCap EMC
C                 (i,3) ; For Barrel HDC
C                 (i,4) ; For EndCap HDC
C*
C=    DATA (CLSPAR(I,1),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.3 /
C=    DATA (CLSPAR(I,2),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.3 /
C=    DATA (CLSPAR(I,3),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.25 /
C=    DATA (CLSPAR(I,4),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.25 /
C
C Parameters to define lateral spread of shower in cal.
C  Initialize a lateral shower shape function:
C     f(x) = ANM(1,i)*EXP(-!x!/ALM(1,i)) + ANM(2,i)*EXP(-!x!/ALM(2,i))
C                   core part                 peripheral part
C  where i = (1,2) = (EM,HD).
C=    DATA (CLSSH_ANM(I,1),I=1,2)/ 0.055424, 0.395883/
C=    DATA (CLSSH_ALM(I,1),I=1,2)/ 0.45,  1.20 /
C=    DATA (CLSSH_ANM(I,2),I=1,2)/ 0.0877193, 0.043860/
C=    DATA (CLSSH_ALM(I,2),I=1,2)/ 2.20,  7.00/
