C**********************************************************************
C* 
C*  ----------------------------====================
C*  Subroutine UFGEOT( NPAR, X, FITANS, DITANS, NRET)
C*  ----------------------------====================
C* 
C*(Function)
C*   Called from PGFTRK to unpack fit results.
C* 
C*(Input)
C*   NTRK    ; # of tracks.
C*   X(i)    ; Final fit parameters.
C* 
C*(Output)
C*   FITANS  ; Array to store fit results.
C*   DITANS  ; Same as FITANS with REAL*8 variable.
C*   NRET    ; When failed to invert error matrix.
C* 
C*(Author)
C*  A. Miyamoto  24-Jun-1987
C* 
C**********************************************************************
C* 
      SUBROUTINE UFGEOT( NPAR, X, FITANS, DITANS, NRET)
C  
      REAL*4  FITANS(16,0:*)
      REAL*8  DITANS( 8,0:*)
      REAL*8  X(NPAR)
C  
#include "ufgeoc.inc"
#include "ufgeop.inc"
C  
C Local variables.
C  
      LDTADR(I,J) = I*(I-1)/2 + J
C  
C =====< Entry Point >=================================================
C  
      NRET = 0
C  
C ---------------------------------------------------------------------
C (2) Loop by Trak to copy track parameter to /PGFBUF/
C ---------------------------------------------------------------------
C  
      DO 200 ITRK = 0, NUMTRK
        IP              = 3*ITRK
        FITANS(1,ITRK)  = X(IP+1)
        FITANS(2,ITRK)  = X(IP+2)
        FITANS(3,ITRK)  = X(IP+3)
        IF( ITRK .EQ. 0 ) THEN
          FITANS(4,ITRK) = -1.
        ELSE
          FITANS(4,ITRK) = TRKPAR(9,ITRK)
        ENDIF
   
C (2.1) Save Error matrix.
   
        IK  =  2
        DO 210   I = 1, 3
          DO 220 J = I, 3
            IK  = IK + 1
            DITANS(IK, ITRK) = DD(IP+I, IP+J)
220       CONTINUE
210     CONTINUE
   
200   CONTINUE
C  
C ---------------------------------------------------------------------
C (5) Return to Caller.
C ---------------------------------------------------------------------
C  
      RETURN
      END
