C**********************************************************************
C* 
C*  ----------------------------------
C*  Subroutine UTRKIV( TRKPAR, DRKPAR)
C*  ----------------------------------
C* 
C*(Function)
C*   Change charge sign of the track parameter.
C* 
C*(Input)
C*   TRKPAR  ; Track parameter with TPC;Track parameter format.
C*             ( R*4 format)
C*   DRKPAR  ; same as TRKPAR, but R*8 format.
C* 
C*(Output)
C*   TRKPAR  ; Change inverted track parameter.
C*   DRKPAR  ;
C* 
C*(Author)
C*  A. Miyamoto   6-Jul-1987
C* 
C**********************************************************************
C* 
      SUBROUTINE UTRKIV( TRKPAR, DRKPAR )
C  
      REAL*4  TRKPAR(*)
      REAL*8  DRKPAR(*)
C  
C Local variables.
C  
      REAL*4  RSIGN(5)/-1., 1., -1., 1., -1./
      REAL*8  DSIGN(5:19)/1.D0, -1.D0,  1.D0, -1.D0,  1.D0,
     >                           1.D0, -1.D0,  1.D0, -1.D0,
     >                                  1.D0, -1.D0,  1.D0,
     >                                         1.D0, -1.D0,
     >                                                1.D0 /
#include "unumcn.inc"
      EXTERNAL UCONST
C  
C =====< Entry Point >=================================================
C  
#ifdef HIUXF77
      CALL UCONST
#endif
C ---------------------------------------------------------------------
C (1) Convert track parameter.
C ---------------------------------------------------------------------
C  
      DO 100 I = 1, 5
        TRKPAR(I) = RSIGN(I)*TRKPAR(I)
100   CONTINUE
   
      TRKPAR(2) = MOD( TRKPAR(2) + x2PID, x2PID ) - xPID
   
      DO 200 I = 5, 19
        DRKPAR(I) = DSIGN(I)*DRKPAR(I)
200   CONTINUE
   
      RETURN
      END
