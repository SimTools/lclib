CC********************************************************************CC
C*                                                                    *C
C*==================================-----------==                     *C
C*  Subroutine UFTVTK(LNxTRK,TRAKIN,TRAKOT,IRET)                      *C
C*==================================-----------==                     *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Refit input trakc with IP constraint.                           *C
C* (Inputs)                                                           *C
C*      LNxTRK      : (I*4) ; length of track parameter ( > 37 ).     *C
C*      TRAKIN(*)   : (R*4) ; helix parameter vector. 1-st 38 words   *C
C*                            should have the same format as that of  *C
C*                            11-th to 48-th words of                 *C
C*                            Production:CDC;Track_Parameter.         *C
C* (Outputs)                                                          *C
C*      TRAKOT(*)   : (R*4) ; helix parameter vector with IP          *C
C*                            constraint. Stored error infomation is  *C
C*                            now the lower triangle of 3x3 matrix    *C
C*                            corresponding to (phi_0,kappa,tanl).    *C
C*                            First 8 words has the same format as    *C
C*                            TRAKIN though drho, dz are always zero. *C
C* (Relation)                                                         *C
C*    Calls : UVZERO, UVCOPY, UMATIN, UTRKMV.                         *C
C* (Update Record)                                                    *C
C*    5/22/92  K.Fujii        Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
 
      SUBROUTINE UFTVTK(LNxTRK,TRAKIN,TRAKOT,IRET)
 
      INTEGER*4      IRET
      REAL   *4      TRAKIN(LNxTRK), TRAKOT(LNxTRK)
      PARAMETER      ( LNxHLX = 38 )
      REAL   *4      HELXIN(LNxHLX), XP(3), HELXOT(LNxHLX)
      REAL   *8      EE(5,5), EEP(15), DADB(5,3), EEB(3,3), B(3)
      DATA NCALL     /0/
C
C========< Entry Point >================================================
C
C--
C  Initialization.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         CALL UVZERO(30,DADB)
         DADB(2,1) = 1
         DADB(3,2) = 1
         DADB(5,3) = 1
      ENDIF
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Store track informations to HELXIN.
C--
      CALL UVCOPY(30,TRAKIN(9),EEP)
      N = 0
      DO 100 J = 1, 5
         DO 10 I = 1, J
            N       = N + 1
            EE(I,J) = EEP(N)
            EE(J,I) = EE (I,J)
10       CONTINUE
100   CONTINUE
C--
      N = 0
      DO 200 J = 1, 5
         DO 20 I = J, 5
            N      = N + 1
            EEP(N) = EE(I,J)
20       CONTINUE
200   CONTINUE
C--
      CALL UVCOPY(8,TRAKIN(1),HELXIN(1))
      CALL UVCOPY(30,EEP(1),HELXIN(9))
C--
C  Move pivot to IP.
C--
      XP(1) = 0
      XP(2) = 0
      XP(3) = 0
      CALL UTRKMV(LNxHLX,HELXIN,XP,HELXOT)
C--
C  Symmetrize error matrix.
C--
      CALL UVCOPY(30,HELXOT(9),EEP)
      N = 0
      DO 300 J = 1, 5
         DO 30 I = J, 5
            N       = N + 1
            EE(I,J) = EEP(N)
            EE(J,I) = EE (I,J)
30       CONTINUE
300   CONTINUE
C--
C  Invert old error matrix.
C--
      CALL UMATIN(5,5,EE,DET,IRT)
      IF ( IRT.NE.0 ) THEN
         IRET = IRT
         RETURN
      ENDIF
C--
C  Calculate new error matrix.
C--
      DO 4000 J = 1, 3
         DO 400 I = 1, J
            EEB(I,J) = 0
            DO 40 L = 1, 5
               DO 4 K = 1, 5
                  EEB(I,J) = EEB(I,J) + DADB(K,I)*EE(K,L)*DADB(L,J)
4              CONTINUE
40          CONTINUE
            EEB(J,I) = EEB(I,J)
400      CONTINUE
4000  CONTINUE
C--
      CALL UMATIN(3,3,EEB,DET,IRT)
      IF ( IRT.NE.0 ) THEN
         IRET = IRT
         RETURN
      ENDIF
C--
C  Calculate new track parameter.
C--
      DO 5000 I = 1, 3
         B(I) = 0
         DO 500 L = 1, 3
            DO 50 K = 1, 5
               DO 5 J = 1, 5
                  B(I) = B(I) + EEB(I,J)*DADB(K,J)*EE(K,L)*HELXOT(L)
5              CONTINUE
50          CONTINUE
500      CONTINUE
5000  CONTINUE
C--
C  Store new track parameter in TRAKOT.
C--
      TRAKOT(1) = 0
      TRAKOT(2) = B(1)
      TRAKOT(3) = B(2)
      TRAKOT(4) = 0
      TRAKOT(5) = B(3)
      TRAKOT(6) = 0
      TRAKOT(7) = 0
      TRAKOT(8) = 0
C--
      N = 0
      DO 600 J = 1, 3
         DO 60 I = 1, J
            N      = N + 1
            EEP(N) = EEB(I,J)
60       CONTINUE
600   CONTINUE
      CALL UVCOPY(12,EEP,TRAKOT(9))
C--
C  That's it.
C--
      RETURN
      END
