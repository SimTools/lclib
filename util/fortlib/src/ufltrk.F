CC********************************************************************CC
C*                                                                    *C
C*================================------------------------===         *C
C*  Subroutine UFLTRK(NPT,W,D,AST,A,ERRMAT,CHI2,PCHI2,NTRY)           *C
C*================================------------------------===         *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Fit straight line track in R-Phi space.                         *C
C* (Inputs)                                                           *C
C*      NPT         : (I*4) ; # hit points.                           *C
C*      W  (1,IPT)  : (R*4) ; X coordinate of wire position.          *C
C*         (2,IPT)          ; Y coordinate of wire position.          *C
C*      D  (1,IPT)  : (R*4) ; drift length.                           *C
C*         (2,IPT)          ; error of drift length.                  *C
C*      AST(1)      : (R*4) ; DR    --+-- initial values.             *C
C*         (2)              ; Phi0  --+                               *C
C*         (3)              ; X coordinate of the pivot.              *C
C*         (4)              ; Y coordinate of the pivot.              *C
C* (Outputs)                                                          *C
C*      A  (1)      : (R*4) ; DR    --+-- fit results.                *C
C*         (2)              ; Phi0  --+                               *C
C*         (3)              ; X coordinate of the pivot.              *C
C*         (4)              ; Y coordinate of the pivot.              *C
C*      CHI2        : (R*4) ; chi**2.                                 *C
C*      PCHI2(IPT)  : (R*4) ; chi**2 contribution of each hit.        *C
C*      NTRY        : (I*4) ; return flag.                            *C
C*                  :   > 0 ; # iterations.                           *C
C*                  :  =< 0 ; error.                                  *C
C* (Relation)                                                         *C
C*    Calls : UVZERO, UMATIN, UVCOPY.                                 *C
C* (Update Record)                                                    *C
C*    7/13/87  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UFLTRK(NPT,W,D,AST,A,ERRMAT,CHI2,PCHI2,NTRY)
   
      IMPLICIT   REAL*8 ( A-H, O-Z )
   
      INTEGER*4      NPT, NTRY
      REAL   *4      W(2,NPT), D(2,NPT), AST(4), A(4), CHI2, PCHI2(NPT)
      REAL   *8      ERRMAT(3)
   
      PARAMETER    ( MXxNPT = 100, NPAR = 2, MPAR = NPAR*(NPAR+1)/2,
     .               NPAR2  = 2*NPAR, MPAR2 = 2*MPAR )
      INTEGER*4     LR(MXxNPT)
      REAL   *4     CHI2S, PKI2S(MXxNPT), AS(NPAR), XWX0S(MXxNPT),
     .              YWY0S(MXxNPT), SIGRS(MXxNPT), CHITST
      REAL   *8     DA   (NPAR), DXDA(NPAR), DCHI2(NPAR),
     .              D2CHI2(MPAR), DKI2S(NPAR), D2KI2S(MPAR),
     .              DD(NPAR*NPAR), EE(NPAR,NPAR)
      EQUIVALENCE  ( DD, EE )
   
C>>>>>>> Loop control parameters >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      DATA NTRYMX / 100 /  CHITST /  1.E-3 /
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  
C  
C========< Entry Point >================================================
C  
C  
C--
C  Reset # trials.
C--
      NTRY = 0
C--
C  Check # parameters.
C--
      IF ( NPT.GT.MXxNPT ) THEN
         PRINT *, ' UFLTRK; NPT > MXxNPT : NPT, MXxNPT =',NPT ,MXxNPT
         CHI2 = 1.E10
         RETURN
      ELSE IF ( NPT.LT.3 ) THEN
         PRINT *, ' UFLTRK; 3 > NPT = ', NPT
         CHI2 = 1.E10
         RETURN
      ENDIF
C--
C  Initialize loop control paramters.
C--
      EPSL  = 1.D-6
      CHI2S = 1.E10
C--
C  Set initial parameters.
C--
      CALL UVCOPY(4,AST,A)
      X0 = A(3)
      Y0 = A(4)
      DO 10 IPT = 1, NPT
         XWX0S(IPT) = W(1,IPT) - X0
         YWY0S(IPT) = W(2,IPT) - Y0
         SIGRS(IPT) = 1./(D(2,IPT)*D(2,IPT))
10    CONTINUE
C--
C  Start iteration.
C--
1     NTRY = NTRY + 1
C--
C  Clear CHI2, PCHI2(i), @CHI2/@A, @@CHI2/@A@A.
C     CHI2        = Chi**2
C     PCHI2 (IPT) = Chi**2 contribution from IPT-th data point.
C     DCHI2 ( I ) = 0.5 * @Chi**2/@A(I)
C     D2CHI2( IJ) = 0.5 * @@Chi**2/@A(I)@A(J)
C--
      CHI2 = 0.E0
      CALL UVZERO(NPT  ,PCHI2 )
      CALL UVZERO(NPAR2,DCHI2 )
      CALL UVZERO(MPAR2,D2CHI2)
C--
C  Loop over data points to accumulate chi**2 and its derivatives.
C--
      DO 2000 IPT = 1, NPT
C--
C  Get X and its tangent matrix and metric tensor at A.
C     X          = expected drift time.
C     DXDA(IPAR) = @X/@A(IPAR).
C     LR  (IPT)  = left-right flag.
C--
         DR      = A(1)
         FI0     = A(2)
         CSF0    = DCOS(FI0)
         SNF0    = DSIN(FI0)
         XWX0    = XWX0S(IPT)
         YWY0    = YWY0S(IPT)
         X       = XWX0*CSF0 + YWY0*SNF0 - DR
         IF ( X.GE.0.D0 ) THEN
            LR(IPT) =  1
         ELSE
            LR(IPT) = -1
         ENDIF
         DXDA(1) = - 1.D0
         DXDA(2) = - XWX0*SNF0 + YWY0*CSF0
C--
C  Galculate residual vector.
C     DELD  = LR(IPT)*D(1,IPT) - X
C     SIGR  = Inverse sigma(D)**2.
C--
         DELD = LR(IPT)*D(1,IPT) - X
         SIGR = SIGRS(IPT)
C--
C  Accumulate chi**2.
C--
         PCHI2(IPT) = DELD*SIGR*DELD
         CHI2       = CHI2 + PCHI2(IPT)
C--
C  Accumulate @chi**2/@A, @@chi**2/@A@A.
C--
         IPJP = 0
         DO 200 JPAR = 1, NPAR
            DCHI2 (JPAR) = DCHI2(JPAR) - DELD*SIGR*DXDA(JPAR)
            DO 20 IPAR = 1, JPAR
               IPJP = IPJP + 1
               D2CHI2(IPJP) = D2CHI2(IPJP)
     .                      + DXDA(IPAR)*SIGR*DXDA(JPAR)
20          CONTINUE
200      CONTINUE
2000  CONTINUE
C--
C  Look at Chi**2 change.
C--
      IF ( ABS(CHI2S-CHI2).LT.CHITST )           GO TO 5000
C--
C  Chi**2 increased. Multiply EPSL by 1.D1 and try again.
C--
      IF ( CHI2.GT.CHI2S ) THEN
         EPSL  = EPSL*1.D2
         CALL UVCOPY(NPAR ,AS    ,A     )
         CALL UVCOPY(NPAR2,DKI2S ,DCHI2 )
         CALL UVCOPY(MPAR2,D2KI2S,D2CHI2)
C--
C  Chi**2 decreased. Accept this step and modify EPSL by 1.D-1.
C  Save Chi**2, Chi**2 contribution of each point, parameter vector,
C  @Chi**2/@A, and @@Chi**2/@A@A.
C--
      ELSE
         EPSL  = EPSL*1.D-2
         CHI2S = CHI2
         CALL UVCOPY(NPT  ,PCHI2 ,PKI2S )
         CALL UVCOPY(NPAR ,A     ,AS    )
         CALL UVCOPY(NPAR2,DCHI2 ,DKI2S )
         CALL UVCOPY(MPAR2,D2CHI2,D2KI2S)
      ENDIF
C--
C  Modify diagonal elements and save results in DD. This is necessary
C  to avoid divergence of the fit. Large EPSL forces correction vector
C  to point counter gradient direction with small step length.
C--
      CALL UVCOPY(MPAR2,D2CHI2,DD)
      IPJP = 0
      DO 300 JPAR = 1, NPAR
         IPJP     = IPJP + JPAR
         DD(IPJP) = ( 1.D0 + EPSL )*DD(IPJP)
300   CONTINUE
C--
C  Solve DCHI2 + DD*(-DA) = 0 to get next step -DA.
C--
      CALL UVCOPY(NPAR2,DCHI2,DA)
      CALL DLSX(DD,NPAR,DA,0.D0,1,IRT)
      IF ( IRT.LT.0 ) THEN
         PRINT *, ' >>>>>> Error in UFTVTX >>>>>>'
         PRINT *, '   DLSX retruns IRT =',IRT
         NTRY = - NTRY
         RETURN
      ENDIF
      DO 400 IPAR = 1, NPAR
         A(IPAR)  = A(IPAR)  - DA(IPAR)
400   CONTINUE
C--
C  Continue iteration if NTRY < NTRYMX.
C--
      IF ( NTRY.LT.NTRYMX )                      GO TO 1
C--
C  Fit did not converge. Recall the best fit.
C--
      PRINT *, ' UFLTRK : did not converge.'
      CHI2 = CHI2S
      CALL UVCOPY(NPT  ,PKI2S ,PCHI2 )
      CALL UVCOPY(NPAR ,AS    ,A     )
      CALL UVCOPY(MPAR2,D2KI2S,D2CHI2)
C--
C  End iteration loop. Now calculate vertex error matrix.
C--
5000  IPJP = 0
      DO 600 JPAR = 1, NPAR
         DO 60 IPAR = 1, JPAR
            IPJP          = IPJP + 1
            EE(IPAR,JPAR) = D2CHI2(IPJP)
            EE(JPAR,IPAR) = EE(IPAR,JPAR)
60       CONTINUE
600   CONTINUE
      CALL UMATIN(NPAR,NPAR,EE,DET,IRT)
      IF ( IRT.LT.0 ) THEN
         PRINT *, ' >>>>>> Error in UFLTRK >>>>>>'
         PRINT *, '   Failed to invert D2CHI2 ; IRT =',IRT
         NTRY = - NTRY
         RETURN
      ENDIF
      N = 0
      DO 900 JPAR = 1, NPAR
         DO 90 IPAR = JPAR, NPAR
            N         = N + 1
            ERRMAT(N) = EE(IPAR,JPAR)
90       CONTINUE
900   CONTINUE
C--
C  That's it.
C--
      RETURN
      END
