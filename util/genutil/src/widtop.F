CPLOT
#ifdef PLOT
       DIMENSION WIDTH(0:3)
#endif 
#ifdef PLOT
      DATA      IU/21/
#endif 
#ifdef PLOT
C
#endif 
#ifdef PLOT
      AMT = 130.
#endif 
#ifdef PLOT
      ALPHA = 1./128.
#endif 
#ifdef PLOT
      SIN2TW = 0.230
#endif 
CPLOT
#ifdef PLOT
      WRITE(IU,*) 'SET FONT DUPLEX'
#endif 
#ifdef PLOT
      WRITE(IU,*) 'SET LIMIT X FROM 80. TO 300.'
#endif 
#ifdef PLOT
      WRITE(IU,*) 'SET LIMIT Y FROM 0.  TO 10.'
#endif 
CPLOT
#ifdef PLOT
      DO 200 IT = 0, 3
#endif 
#ifdef PLOT
        WRITE(IU,*) 'SET ORDER X Y'
#endif 
#ifdef PLOT
        AMIN = 90.0
#endif 
#ifdef PLOT
        ASTP =  2.0
#endif 
#ifdef PLOT
        DO 100 I = 0, 100
#endif 
#ifdef PLOT
          AMT = AMIN + ASTP*FLOAT(I)
#endif 
#ifdef PLOT
          CALL WIDTOP( AMT, ALPHA, SIN2TW, WIDTH )
#endif 
#ifdef PLOT
          WRITE(IU,*) AMT, WIDTH(IT)
#endif 
#ifdef PLOT
100     CONTINUE
#endif 
#ifdef PLOT
        WRITE(IU,*) ' JOIN SOLID '
#endif 
#ifdef PLOT
200   CONTINUE
#endif 
#ifdef PLOT
C
#endif 
#ifdef PLOT
      STOP
#endif 
#ifdef PLOT
      END
#endif 
C***********************************************************************
C*
C*  --------------------------------
C*   Subroutine WIDTOP( AMT, ALPHA, SIN2TW, WIDTH )
C*  --------------------------------
C*(Function)
C*   Calculate decay width of
C*             t --> W + f  (f=b,s,d)
C*   This routine assumes the masses and KM matrix as follows.
C*     Mass : W=80GeV, b=5GeV, s=0.5GeV, d=0.3GeV
C*     KM Matrix : Utb = 0.999,  Uts=0.043, Utd=0.010
C*(Input)
C*   AMT   ; Mass of TOP quark.
C*   ALPHA ; Alpha
C*   SIN2TW; Weignberg angle.
C*(Output)
C*   WIDTH(0:3)  ; Decay width
C*           (0) = Total
C*           (1) = Wd
C*           (2) = Ws
C*           (3) = Wb
C*(Author)
C*   A. Miyamoto  10-Aug-1990   Original version.
C*
C***********************************************************************
C
      SUBROUTINE WIDTOP( AMT, ALPHA, SIN2TW, WIDTH)
C
      DIMENSION WIDTH(0:3)
      DIMENSION AMAS(0:3), UTF(3)
      DATA  AMAS/ 80.0, 0.3, 0.5, 5.0 /
      DATA  UTF /0.010, 0.043, 0.999/
C
      BETABS(A, X1, X2)=1. - 2.*(X1*X1+X2*X2)/(A*A)
     >                     + ((X1-X2)*(X1+X2)/(A*A))**2
C
 
C
C =====< Entry Point >==================================================
C
      WIDTH(0) = 0
      FACT     = ALPHA/(8.*SIN2TW)
      DO 100 I = 1, 3
        XM = (AMT-AMAS(0))*(AMT+AMAS(0))
     >     + (AMAS(I)-AMAS(0))*(AMAS(I)+AMAS(0))
     >     + ((AMT-AMAS(I))*(AMT+AMAS(I))/AMAS(0))**2
        WIDTH(I) = FACT
     >           * UTF(I)**2
     >           * SQRT( BETABS(AMT, AMAS(I), AMAS(0) ) )
     >           * XM
     >           / (2.*AMT)
        WIDTH(0) = WIDTH(0) + WIDTH(I)
100   CONTINUE
C
      RETURN
      END
