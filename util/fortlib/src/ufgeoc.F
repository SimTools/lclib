C**********************************************************************
C* 
C*  -----------------------------------------------
C*  Subroutine UFGEOC( NTRK, LNxTRK, TRKINI, VTXINI,
C*                     CHIS, FITANS, NRET)
C*  -----------------------------------------------
C* 
C*(Function)
C*   Do geometrical constraint fit to the Track parameter.
C* 
C*(Input)
C*   NTRK    ; # of track.
C*   LNxTRK  ; Length of track parameter in I*4 unit.
C*             LNxTRK must be even number.
C*   TRKINI  ; Initial track parameter with TPC;Track_Parameter format.
C*   VTXINI  ; Initial vertex position.
C*             VTXINI(3) = ( Vx, Vy, Vz )
C* 
C*(Output)
C*   CHIS    ; Chi-square of Geofit.
C*   FITANS  ; R*4, R*8 mixed array of fit results, Should be decalred
C*            as REAL*4  FITANS(16, 0:NTRK) in the calling routine.
C*            Error matrix are saved in FITANS(5:14,0:NTRK) by REAL*8
C*            format.  To use Error matrix, declare as follows.
C*               REAL*4  FITANS(16,0:NTRK)
C*               REAL*8  DITANS( 8,0:NTRK)
C*               EQUIVALENCE (FITANS(1,0), DITANS(1,0))
C* 
C*       FITANS(1,i) = Vx (i=0) or Phi0(i^=0)
C*             (2,i) = Vy (i=0) or K   (i^=0)
C*             (3,i) = Vz (i=0) or Tanl(i^=0)
C*             (4,0) = -1, indicatting fitted vertex parameter.
C*                i) = Chi-square contribution of i-th track.
C*       DITANS(3,i) = Error matrix (1,1)  (k,l)
C*             (4,i) =              (2,1)     k=l=1  for Vx or Phi0
C*             (5,i) =              (3,1)        =2  for Vy orK
C*             (6,i) =              (2,2)        =3  for Vz of Tanl
C*             (7,i) =              (3,2)
C*             (8,i) =              (3,3)
C* 
C*   NRET  > 0 ; # of try, when fit converges
C*         =-1 ; Fit does not converge.
C*         =-2 ; Error during fit.
C*         =-3 ; Invalid error matrix.
C*         =-4 ; NTRK .GT. MFxTRK (50)
C* 
C*(Author)
C*  A. Miyamoto  24-Jun-1987
C* 
C**********************************************************************
C* 
      SUBROUTINE UFGEOC( NTRK,   LNxTRK, TRKINI, VTXINI,
     >                   CHISQR, FITANS, NRET)
C  
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4  TRKINI(LNxTRK, NTRK), VTXINI(3)
      REAL*4  FITANS(16,0:NTRK)
      REAL*4  CHISQR
C  
#include "ufgeoc.inc"
#include "ufgeop.inc"
C  
C Local variables.
C  
      REAL*8     A(MFxPAR), DA(MFxPAR), AS(MFxPAR)
      REAL*8     CHI2,  DCHI2(MFxPAR)
      REAL*8     CHI2S, DCHI2S(MFxPAR)
C      REAL*8     PCHI2(MFxTRK)
C      REAL*8     PCHI2S(MFxTRK)
      REAL*8     D2CHI2(MFxPAR, MFxPAR)
      REAL*8     D2KI2S(MFxPAR, MFxPAR)
      EXTERNAL   UFGEOB, PRBLK1
C  
C =====< Entry Point >=================================================
C  
#ifdef HIUXF77
      CALL PRBLK1
      CALL UFGEOB
#endif
C ---------------------------------------------------------------------
C (1) Parameter check.
C ---------------------------------------------------------------------
C  
      NRET = -4
      IF( NTRK .GT. MFxTRK ) RETURN
      IF( NTRK .LT. 2      ) RETURN
      IF( MOD(LNxTRK,2) .EQ. 1 ) THEN
         PRINT *,'%Error..UFGEOC  LNxTRK was ',LNxTRK,' ..not even.'
         CALL ERRTRA
         STOP
      ELSEIF( LNxTRK .LT. 38 ) THEN
         PRINT *,'%Error..UFGEOC  LNxTRK was ',LNxTRK,
     >           ' This should be greater than 38.'
         CALL ERRTRA
         STOP
      ENDIF
C  
C ---------------------------------------------------------------------
C (2) Set initial parameter to the common /PGFBUF/
C ---------------------------------------------------------------------
C  
      CALL UFGEOI(NTRK, LNxTRK, TRKINI, TRKINI, VTXINI, A, IRET)
      NRET = -3
      IF( IRET .LT. 0 ) RETURN
C  
C ---------------------------------------------------------------------
C (3) Reset loop control parameters
C ---------------------------------------------------------------------
C  
      NPAR  = 3 + 3*NTRK
      NTRY  = 0
      EPSL  = 1.D-12
      CHI2S = 1.D10
C  
C ---------------------------------------------------------------------
C (4) Start Initalization loop.
C ---------------------------------------------------------------------
C  
400   CONTINUE
      NTRY  = NTRY + 1
C  
C (4.1) CLEAR CHI2, PCHI2(i), @CHI2/@A, @@CHI2/@A@A
C     CHI2        = Chi**2
C     PCHI2(Itrk) = Chi**2 contribution from Itrk-th track.
C     DCHI2(i)    = @Chi**2/@A(i)
C     D2CHI2(i,j) = @@Chi**2/@A(i)@A(j)
C     DA(i)       = Correction to A
C  
      CALL UFGEOK(NPAR, MFxPAR, A, CHI2, DCHI2, D2CHI2)
C  
C ------------------------------------------------------------------
C (5) Look at chi-square change
C ------------------------------------------------------------------
C  
      IF( DABS(CHI2S-CHI2) .LT. CHITST ) GO TO 8000
C  
C (5.1) Chi-square increase.  Multiply EPSL by 1.D1 and try again
C  
      IF( CHI2 .GT. CHI2S ) THEN
        EPSL  = EPSL*1.D2
        DO 500 I = 1, NPAR
          A(I)     = AS(I)
          DCHI2(I) = DCHI2S(I)
          DO  50 J = 1, NPAR
            D2CHI2(I,J) = D2KI2S(I,J)
            DD    (I,J) = D2CHI2(I,J)
50        CONTINUE
          DD(I,I)  = (1.D0 + EPSL)*DD(I,I)
500     CONTINUE
C  
C (5.2) Chi-square decreases.  Accept this step and modify EPSL by 1.D-2
C  
      ELSE
        EPSL  = EPSL*1.D-2
        CHI2S = CHI2
        DO 520 I = 1, NPAR
          AS(I)     = A(I)
          DCHI2S(I) = DCHI2(I)
          DO  52 J = 1, NPAR
             D2KI2S(I,J) = D2CHI2(I,J)
             DD    (I,J) = D2KI2S(I,J)
52        CONTINUE
          DD(I,I)   = (1.D0 + EPSL)*DD(I,I)
520     CONTINUE
      ENDIF
C  
C ----------------------------------------------------------------------
C (6) Invert DD and get next step.
C ----------------------------------------------------------------------
C  
C--
C  Invert DD and get next step.
C--
      CALL UMATIN(MFxPAR,NPAR,DD,DET,IRT)
      IF ( IRT.LT.0 ) THEN
         PRINT *, ' >>>>>> Error in UFGEOC >>>>>>'
         PRINT *, '   Failed to invert D2CHI2 ; IRT =',IRT
         NTRY = - NTRY
         RETURN
      ENDIF
      DO 600 I = 1, NPAR
         DA(I) = 0.0
         DO 610 J = 1, NPAR
            DA(I) = DA(I) + DD(I,J)*DCHI2(J)
610      CONTINUE
         A(I)  = A(I)  - DA(I)
600   CONTINUE
C  
C--
C  Continue iteration if NTRY < MAXTRY.
C--
      IF ( EPSL .GT. 1.0D60 )                      GO TO 700
      IF ( NTRY .LT. MAXTRY )                      GO TO 400
C--
C  Fit did not converge. Recall the best fit.
C--
700   CONTINUE
      PRINT *, ' UFGEOC : did not converge.'
      CHI2 = CHI2S
      DO 750 I = 1, NPAR
         A(I) = AS(I)
         DO 75 J = 1, NPAR
            D2CHI2(I,J) = D2KI2S(I,J)
75       CONTINUE
750   CONTINUE
C--
C  End iteration loop. Now calculate vertex error matrix.
C--
8000  CONTINUE
      DO 850 I = 1, NPAR
         DO 85 J = 1, NPAR
            DD(I,J) = D2CHI2(I,J)
85       CONTINUE
850   CONTINUE
      CALL UMATIN(MFxPAR,NPAR,DD,DET,IRT)
      IF ( IRT.LT.0 ) THEN
         PRINT *, ' >>>>>> Error in UFGEOC >>>>>>'
         PRINT *, '   Failed to invert D2CHI2 ; IRT =',IRT
         NTRY = - NTRY
         RETURN
      ENDIF
C--
C  Fill output arguments.
C--
      CALL UFGEOT( NPAR, A, FITANS, FITANS, IRET)
      CHISQR = CHI2
      NRET   = NTRY
C--
C  That's it.
C--
9000  RETURN
      END
