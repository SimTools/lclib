CC********************************************************************CC
C*                                                                    *C
C*===========================================----------------===      *C
C*  Subroutine UTRKAV(NTRK,LNxHLX,HELX,HELXAV,CHI2,PCHI2,IRET)        *C
C*=====================================----------------------===      *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Convolutes multi-track segments into a single helx.             *C
C* (Inputs)                                                           *C
C*      NTRK        : (I*4) ; # tracks.                               *C
C*      LNxHLX      : (I*4) ; length of helix parameter ( > 37 ).     *C
C*      HELX(*,ITK) : (R*4) ; helix parameter vector. 1-st 38 words   *C
C*                            should have the same format as that of  *C
C*                            Production:TPC;Track_Parameter.         *C
C*      HELXAV(3  ) : (R*4) ; charge of the track. ( +/-1.)           *C
C*            (6-8) : (R*4) ; new pivot position.                     *C
C* (Outputs)                                                          *C
C*      HELXAV(*)   : (R*4) ; averaged helix parameter ( = 38 ).      *C
C*      CHI2        : (R*4) ; chi**2.                                 *C
C*      PCHI2 (*)   : (R*4) ; chi**2 contribution of each track.      *C
C*      IRET        : (I*4) ; return flag.                            *C
C* (Relation)                                                         *C
C*    Calls : UTRKMV, UVZERO, UMATIN, UVCOPY.                         *C
C* (Update Record)                                                    *C
C*    7/08/87  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UTRKAV(NTRK,LNxHLX,HELX,HELXAV,CHI2,PCHI2,IRET)
   
      IMPLICIT   REAL*8 ( A-H, O-Z )
   
#include "unumcn.inc"
#include "prfeld.inc"
      INTEGER*4      NTRK, IRET
      REAL   *4      HELX(LNxHLX,NTRK), HELXAV(38), CHI2, PCHI2(NTRK)
   
      PARAMETER      ( MXxTRK = 100 )
      INTEGER*4      IFLIP(15)
      REAL   *8      RS(5), EPS(5,5), EA(5), EE(5,5), ERRMAT(15),
     .               EPSS(5,5,MXxTRK)
      REAL   *4      HELXWK(38), HELXIN(5,MXxTRK), CHRG
      EQUIVALENCE   (HELXWK(9),ERRMAT(1))
      DATA  IFLIP  / 1,-1, 1,-1, 1,  1,-1, 1,-1,  1,-1, 1,  1,-1,  1 /
   
      EXTERNAL       UCONST, PRBLK1
C  
C  
C========< Entry Point >================================================
C  
#ifdef HIUXF77
      CALL PRBLK1
      CALL UCONST
#endif
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Check #tracks.
C--
      IF ( NTRK.GT.MXxTRK ) THEN
         PRINT *, ' MXxTRK < NTRK = ', NTRK
         IRET = -99
         RETURN
      ENDIF
C--
C  Check charge of the track.
C--
      CHRG = HELXAV(3)
C--
C  Clear EE and EA.
C--
      CALL UVZERO(50  ,EE   )
      CALL UVZERO(10  ,EA   )
C--
C  Loop over tracks to accumulate chi**2.
C--
      DO 3000 ITRK = 1, NTRK
C--
C  Convert helx vectors.
C--
         CALL UVCOPY(38,HELX(1,ITRK),HELXWK)
         IF ( CHRG*HELXWK(3).LT.0. ) THEN
            HELXWK(1) = - HELXWK(1)
            HELXWK(2) =   HELXWK(2) + 3.*xPI
            HELXWK(2) =   MOD(HELXWK(2),x2PI)
            IF ( HELXWK(2).GT.xPI ) HELXWK(2) = HELXWK(2) - x2PI
            HELXWK(3) = - HELXWK(3)
            HELXWK(5) = - HELXWK(5)
            DO 100 N = 1, 15
               ERRMAT(N) = IFLIP(N)*ERRMAT(N)
100         CONTINUE
         ENDIF
         CALL UTRKMV(38,HELXWK,HELXAV(6),HELXWK)
C--
C  Save this converted helix parameters to HELXIN.
C--
         CALL UVCOPY(5,HELXWK,HELXIN(1,ITRK))
C--
C  Invert converted helix error matrix.
C--
         N = 0
         DO 200 J = 1, 5
            DO 20 I = J, 5
               N        = N + 1
               EPS(I,J) = ERRMAT(N)
               EPS(J,I) = EPS(I,J)
20          CONTINUE
200      CONTINUE
         CALL UMATIN(5,5,EPS,DET,IRT)
         IF ( IRT.LT.0 ) THEN
            PRINT *, ' >>>>>> Error in UTRKAV >>>>>>'
            PRINT *, '   Failed to invert EPS ; IRT =',IRT
            IRET = - 1
            RETURN
         ENDIF
C--
C  Save this inverted error matrix in EPSS.
C--
         CALL UVCOPY(50,EPS,EPSS(1,1,ITRK))
C--
C  Accumulate EE and EA.
C--
         DO 300 J = 1, 5
            DO 30 I = 1, 5
               EA(J  ) = EA(J  ) + EPS(J,I)*HELXWK(I)
               EE(I,J) = EE(I,J) + EPS(I,J)
30          CONTINUE
300      CONTINUE
3000  CONTINUE
C--
C  Solve  EE*A = EA to get new helix parameter.
C--
      CALL UMATIN(5,5,EE,DET,IRT)
      IF ( IRT.LT.0 ) THEN
         PRINT *, ' >>>>>> Error in UTRKAV >>>>>>'
         PRINT *, '   Failed to invert EE     ; IRT =',IRT
         IRET = - 2
         RETURN
      ENDIF
      DO 400 I = 1, 5
         HELXAV(I) = 0.E0
         DO 40 J = 1, 5
            HELXAV(I) = HELXAV(I) + EE(I,J)*EA(J)
40       CONTINUE
400   CONTINUE
C--
C  Calculate Chi**2.
C     CHI2        = Chi**2
C     PCHI2 (ITK) = Chi**2 contribution from ITK-th track
C--
      CHI2 = 0.E0
      CALL UVZERO(NTRK,PCHI2)
      DO 6000 ITRK = 1, NTRK
         DO 500 I = 1, 5
            RS(I) = HELXIN(I,ITRK) - HELXAV(I)
500      CONTINUE
         DO 600 J = 1, 5
            DO 60 I = 1, 5
               PCHI2(ITRK) = PCHI2(ITRK) + RS(I)*EPSS(I,J,ITRK)*RS(J)
60          CONTINUE
600      CONTINUE
         CHI2 = CHI2 + PCHI2(ITRK)
6000  CONTINUE
C--
C  Fill output array.
C--
      IJ = 0
      DO 700 J = 1, 5
         DO 70 I = J, 5
            IJ         = IJ + 1
            ERRMAT(IJ) = EE(I,J)
70       CONTINUE
700   CONTINUE
      CALL UVCOPY(30,ERRMAT,HELXAV(9))
C--
C  That's it.
C--
9000  RETURN
      END
