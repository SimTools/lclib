CC********************************************************************CC
C*                                                                    *C
C*==================================================================  *C
C*  Subroutine UFTVTX(NTRK,LNxHLX,HELX,XVST,                          *C
C*                                   XV,CHI24,PCHI24,ERVT,PHI,NTRY)   *C
C*===================================-----------------------------==  *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates vertex point consists of NTRK tracks.                *C
C*    The #tracks should not exceed MXxPAR-3. At presetn MXxPAR is    *C
C*    set at 53 corresponding to 50 tracks. User who needs more       *C
C*    can extend MXxPAR.                                              *C
C* (Inputs)                                                           *C
C*      NTRK        : (I*4) ; # tracks.                               *C
C*      LNxHLX      : (I*4) ; length of helix parameter ( > 37 ).     *C
C*      HELX(*,ITK) : (R*4) ; helix parameter vector. 1-st 38 words   *C
C*                            should have the same format as that of  *C
C*                            Production:TPC;Track_Parameter.         *C
C*      XVST(*)     : (R*4) ; start vertex.                           *C
C* (Outputs)                                                          *C
C*      XV  (*)     : (R*4) ; vertex.                                 *C
C*      CHI24       : (R*4) ; chi**2.                                 *C
C*      PCHI24(*)   : (R*4) ; chi**2 contribution of each track.      *C
C*      ERVT(3,3)   : (R*4) ; vertex error matrix.                    *C
C*      PHI (ITK)   : (R*4) ; deflection angle at vertex.             *C
C*      NTRY        : (I*4) ; #trials.                                *C
C* (Relation)                                                         *C
C*    Calls : UFCLSP, UVZERO, UCPSER, UMATIN, UVCOPY.                 *C
C* (Update Record)                                                    *C
C*    6/26/87  K.Fujii        Original version.                       *C
C*    7/03/87  K.Fujii        Fixed a bug in error matrix calculation.*C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UFTVTX(NTRK,LNxHLX,HELX,XVST,
     .                           XV,CHI24,PCHI24,ERVT,PHI,NTRY)
   
      IMPLICIT   REAL*8 ( A-H, O-Z )
   
#include "unumcn.inc"
#include "prfeld.inc"
   
      INTEGER*4  NTRK
      REAL   *4  HELX(LNxHLX,NTRK),XVST(3),XV(3),CHI24,PCHI24(NTRK),
     .           ERVT(3,3),PHI(NTRK)
   
      PARAMETER ( MXxPAR = 53 )
      REAL   *8  RS(3),TNV(3),TNVP(3),
     .           EPS(3,3),DKI2S(MXxPAR),
     .           D2KI2S(MXxPAR,MXxPAR),AS(MXxPAR),
     .           DCHI2(MXxPAR),D2CHI2(MXxPAR,MXxPAR),
     .           A(MXxPAR),DA(MXxPAR),DD(MXxPAR,MXxPAR),
     .           PCHI2(MXxPAR-3),PCHI2S(MXxPAR-3),
     .           CSFI0(MXxPAR-3),SNFI0(MXxPAR-3)
   
      REAL   *4  X0(3),TV(3),DIST
   
      DATA NTRYMX / 100 /  CHITST /  1.D-2 /
   
      EXTERNAL    UCONST, PRBLK1
C  
C  
C========< Entry Point >================================================
C  
#ifdef HIUXF77
      CALL PRBLK1
      CALL UCONST
#endif
C  
C--
C  Reset # trials.
C--
      NTRY = 0
C--
C  Check # parameters.
C--
      NPAR = 3 + NTRK
      IF ( NPAR.GT.MXxPAR ) THEN
         PRINT *, ' UFTVTX; NPAR > MXxPAR , NPAR, MXxPAR =',NPAR,MXxPAR
         CHI24 = 1.E10
         RETURN
      ENDIF
C--
C  Initialize loop control paramters.
C--
      EPSL  = 1.D-12
      CHI2S = 1.D10
C--
C  Set initial parameters.
C     A(  1) ; vertex X.
C      (  2) ; vertex Y.
C      (  3) ; vertex Z.
C      (I+3) ; R * deflection angle of I-th track.
C--
      DO 10 I = 1, 3
         A (I) = XVST(I)
10    CONTINUE
      DO 15 ITRK = 1, NTRK
         CALL UFCLSP(HELX(1,ITRK),XVST,X0,TV,DIST,PHI(ITRK))
         R           = PTOR/HELX(3,ITRK)
         A(3+ITRK)   = PHI(ITRK)*R
         FI0         = HELX(2,ITRK)
         FI0         = MOD(FI0+10.D0*x2PID,x2PID)
         CSF         = DCOS(FI0)
         SNF         = DSQRT(MAX(0.D0,1.D0-CSF*CSF))
         IF ( FI0.GT.xPID ) SNF = - SNF
         CSFI0(ITRK) = CSF
         SNFI0(ITRK) = SNF
15    CONTINUE
C--
C  Start iteration.
C--
1     NTRY = NTRY + 1
C--
C  Clear CHI2, PCHI2(i), @CHI2/@A, @@CHI2/@A@A,
C     CHI2        = Chi**2
C     PCHI2 (ITK) = Chi**2 contribution from ITK-th track
C     DCHI2 ( I ) = @Chi**2/@A(I)
C     D2CHI2(I,J) = @@Chi**2/@A(I)@A(J)
C     DA    ( I ) = correction to A(I)
C--
      CHI2 = 0.D0
      CALL UVZERO(2*NTRK,PCHI2)
      CALL UVZERO(2*NPAR,DCHI2)
      CALL UVZERO(2*MXxPAR*MXxPAR,D2CHI2)
      CALL UVZERO(2*NPAR,DA)
C--
C  Loop over tracks to accumulate chi**2 and its derivatives.
C--
      DO 5000 ITRK = 1, NTRK
C--
C  Get residual and tangent verctors at FI.
C     RS (I) = XV(I)-X(I)
C     TNV(I) = @X(I)/@(R*phi)
C--
         IPR    = 3 + ITRK
         FI0    = HELX(2,ITRK)
         TNL    = HELX(5,ITRK)
         R      = PTOR/HELX(3,ITRK)
         FI     = A(IPR)/R
         CSF0   = CSFI0(ITRK)
         SNF0   = SNFI0(ITRK)
         PSI    = MOD(FI+FI0+10.D0*x2PID,x2PID)
         CSF    = DCOS(PSI)
         SNF    = DSQRT(MAX(0.D0,1.D0-CSF*CSF))
         IF ( PSI.GT.xPID ) SNF = - SNF
         RS(1)  = HELX(6,ITRK) + HELX(1,ITRK)*CSF0 + R*(CSF0-CSF)
         RS(2)  = HELX(7,ITRK) + HELX(1,ITRK)*SNF0 + R*(SNF0-SNF)
         RS(3)  = HELX(8,ITRK) + HELX(4,ITRK)      - R*TNL*FI
         TNV(1) =  SNF
         TNV(2) = -CSF
         TNV(3) = -TNL
         DO 200 I = 1, 3
            RS(I) = A(I) - RS(I)
200      CONTINUE
C--
C  Calculate position error matrix at X0.
C--
         CALL UCPSER(SNGL(FI),HELX(1,ITRK),EPS)
C--
C  Invert position error matrix EPS in DD.
C--
         DO 300 I = 1, 3
            DO 30 J = 1, 3
               DD(I,J) = EPS(I,J)
30          CONTINUE
300      CONTINUE
         CALL UMATIN(MXxPAR,3,DD,DET,IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, ' >>>>>> Error in UFTVTX >>>>>>'
            PRINT *, '   Failed to invert EPS ; IRT =',IRT
            NTRY = - NTRY
            RETURN
         ENDIF
C--
C  Modify TNV with EPS**-1 as metrix tensor and save result as TNVP.
C--
         DO 400 I = 1, 3
            TNVP(I) = 0.D0
            DO 40 J = 1, 3
               TNVP(I) = TNVP(I) + DD(I,J)*TNV(J)
40          CONTINUE
400      CONTINUE
C--
C  Accumulate chi**2, @chi**2/@A, @@chi**2/@A@A.
C--
         DO 500 I = 1, 3
            DCHI2 (    IPR) = DCHI2(IPR) - TNVP(I)*RS(I)
            D2CHI2(I  ,IPR) = - TNVP(I)
            D2CHI2(IPR,I  ) = - TNVP(I)
            D2CHI2(IPR,IPR) = D2CHI2(IPR,IPR) + TNV(I)*TNVP(I)
            DO 50 J = 1, 3
               PCHI2(ITRK)   = PCHI2(ITRK) + RS(I)*DD(I,J)*RS(J)
               DCHI2(I)      = DCHI2(I)    + DD(I,J)*RS(J)
               D2CHI2(I,J)   = D2CHI2(I,J) + DD(I,J)
50          CONTINUE
500      CONTINUE
         CHI2 = CHI2 + PCHI2(ITRK)
5000  CONTINUE
C--
C  Look at Chi**2 change.
C--
      IF ( DABS(CHI2S-CHI2).LT.CHITST )          GO TO 8000
C--
C  Chi**2 increased. Multiply EPSL by 1.D1 and try again.
C--
      IF ( CHI2.GT.CHI2S ) THEN
         EPSL  = EPSL*1.D1
         DO 600 I = 1, NPAR
            A    (I) = AS(I)
            DCHI2(I) = DKI2S(I)
            DO 60 J = 1, NPAR
               D2CHI2(I,J) = D2KI2S(I,J)
               DD    (I,J) = D2CHI2(I,J)
60          CONTINUE
            DD(I,I) = ( 1.D0 + EPSL )*DD(I,I)
600      CONTINUE
C--
C  Chi**2 decreased. Accept this step and modify EPSL by 1.D-2.
C--
      ELSE
         EPSL  = EPSL*1.D-2
         CHI2S = CHI2
         CALL UVCOPY(2*NTRK,PCHI2,PCHI2S)
         DO 650 I = 1, NPAR
            AS   (I) = A(I)
            DKI2S(I) = DCHI2(I)
            DO 65 J = 1, NPAR
               D2KI2S(I,J) = D2CHI2(I,J)
               DD    (I,J) = D2KI2S(I,J)
65          CONTINUE
            DD(I,I) = ( 1.D0 + EPSL )*DD(I,I)
650      CONTINUE
      ENDIF
C--
C  Invert DD and get next step.
C--
      CALL UMATIN(MXxPAR,NPAR,DD,DET,IRT)
      IF ( IRT.LT.0 ) THEN
         PRINT *, ' >>>>>> Error in UFTVTX >>>>>>'
         PRINT *, '   Failed to invert D2CHI2 ; IRT =',IRT
         NTRY = - NTRY
         RETURN
      ENDIF
      DSTEP = 0.D0
      DO 700 I = 1, NPAR
         DO 70 J = 1, NPAR
            DA(I) = DA(I) + DD(I,J)*DCHI2(J)
70       CONTINUE
         DSTEP = DSTEP + DA(I)*DA(I)
         A(I)  = A(I)  - DA(I)
700   CONTINUE
C--
C  Continue iteration if NTRY < NTRYMX.
C--
      IF ( NTRY.LT.NTRYMX )                      GO TO 1
C--
C  Fit did not converge. Recall the best fit.
C--
      PRINT *, ' UFTVTX : did not converge.'
      CHI2 = CHI2S
      CALL UVCOPY(2*NTRK,PCHI2S,PCHI2)
      DO 750 I = 1, NPAR
         A(I) = AS(I)
         DO 75 J = 1, NPAR
            D2CHI2(I,J) = D2KI2S(I,J)
75       CONTINUE
750   CONTINUE
C--
C  End iteration loop. Now calculate vertex error matrix.
C--
8000  DO 850 I = 1, NPAR
         DO 85 J = 1, NPAR
            DD(I,J) = D2CHI2(I,J)
85       CONTINUE
850   CONTINUE
      CALL UMATIN(MXxPAR,NPAR,DD,DET,IRT)
      IF ( IRT.LT.0 ) THEN
         PRINT *, ' >>>>>> Error in UFTVTX >>>>>>'
         PRINT *, '   Failed to invert D2CHI2 ; IRT =',IRT
         NTRY = - NTRY
         RETURN
      ENDIF
C--
C  Fill output arguments.
C--
      DO 870 I = 1, 3
         XV(I) = A(I)
         DO 87 J = 1, 3
            ERVT(I,J) = DD(I,J)
87       CONTINUE
870   CONTINUE
      CHI24 = CHI2
      DO 900 ITRK = 1, NTRK
         PCHI24(ITRK) = PCHI2(ITRK)
         R            = PTOR/HELX(3,ITRK)
         PHI(ITRK)    = A(3+ITRK)/R
900   CONTINUE
C--
C  That's it.
C--
9000  RETURN
      END
