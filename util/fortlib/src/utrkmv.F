CC********************************************************************CC
C*                                                                    *C
C*=====================================------===                      *C
C*  Subroutine UTRKMV(LNxHLX,HELXIN,XP,HELXOT)                        *C
C*=====================================------===                      *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Transforms helix parameters and their error matrix to those     *C
C*    of a new pivot XP.                                              *C
C* (Inputs)                                                           *C
C*      LNxHLX      : (I*4) ; length of helix parameter ( > 37 ).     *C
C*      HELXIN(*)   : (R*4) ; helix parameter vector. 1-st 38 words   *C
C*                            should have the same format as that of  *C
C*                            Production:TPC;Track_Parameter.         *C
C*      XP    (*)   : (R*4) ; new pivot position.                     *C
C* (Outputs)                                                          *C
C*      HELXOT(*)   : (R*4) ; helix parameter vector at XP.           *C
C* (Relation)                                                         *C
C*    Calls : no subroutines.                                         *C
C* (Update Record)                                                    *C
C*    7/06/87  K.Fujii        Original version.                       *C
C*    6/25/89  K.Fujii        Restrict ABS(phi) < pi.                 *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UTRKMV(LNxHLX,HELXIN,XP,HELXOT)
   
      IMPLICIT   REAL*8 ( A-H, O-Z )
   
#include "unumcn.inc"
#include "prfeld.inc"
      INTEGER*4      LNxHLX
      REAL   *4      HELXIN(LNxHLX), XP(3), HELXOT(LNxHLX)
      REAL   *8      DAPDA(5,5), EE(5,5), EEP(15)
   
      EXTERNAL       UCONST, PRBLK1
C  
C========< Entry Point >================================================
C  
C--
C  Check parameter length.
C--
      IF ( LNxHLX.LT.38 ) THEN
         PRINT *, ' >>>>> ERROR in UTRKMV >>>>>'
         PRINT *, '    38 > LNxHLX = ', LNxHLX
         RETURN
      ENDIF
C--
C  Define local variables for helix parameters.
C--
      DR   = HELXIN(1)
      FI0  = HELXIN(2)
      CPA  = HELXIN(3)
      DZ   = HELXIN(4)
      TNL  = HELXIN(5)
      X0   = HELXIN(6)
      Y0   = HELXIN(7)
      Z0   = HELXIN(8)
      XV   = XP(1)
      YV   = XP(2)
      ZV   = XP(3)
C--
C  Transform helix parameters.
C--
      R    = PTOR/CPA
      RDR  = R + DR
      FIP  = MOD(FI0+2.D0*x2PID,x2PID)
      CSF0 = DCOS(FIP)
      SNF0 = DSQRT(MAX(0.D0,(1.D0-CSF0)*(1.D0+CSF0)))
      IF ( FIP.GT.xPID ) SNF0 = - SNF0
      XC   = X0 + RDR*CSF0
      YC   = Y0 + RDR*SNF0
      CSF  = (XC-XV)/R
      SNF  = (YC-YV)/R
      ANRM = 1.D0/DSQRT(CSF*CSF+SNF*SNF)
      CSF  = ANRM*CSF
      SNF  = ANRM*SNF
      CSFD = CSF*CSF0 + SNF*SNF0
      SNFD = SNF*CSF0 - CSF*SNF0
      FIP  = DATAN2(SNF,CSF)
      FID  = MOD(FIP-FI0+4*x2PID,x2PID)
      IF ( FID.GT.xPID ) FID = FID - x2PID
      DRP  = (X0+DR*CSF0+R*(CSF0-CSF)-XV)*CSF
     .     + (Y0+DR*SNF0+R*(SNF0-SNF)-YV)*SNF
      DZP  = Z0 + DZ    - R*TNL*FID - ZV
C--
C  Calculate @AP/@A.
C     AP = ( DRP, FIP, CPA, DZP, TNL )
C     A  = ( DR , FI0, CPA, DZ , TNL )
C--
      RDRPR = 1.D0/(R+DRP)
      RCPAR = R/CPA
   
      CALL UVZERO(50,DAPDA)
   
      DAPDA(1,1) =   CSFD
      DAPDA(1,2) =   RDR*SNFD
      DAPDA(1,3) =   RCPAR*(1.D0-CSFD)
   
      DAPDA(2,1) = - RDRPR*SNFD
      DAPDA(2,2) =   RDR*RDRPR*CSFD
      DAPDA(2,3) =   RCPAR*RDRPR*SNFD
   
      DAPDA(3,3) =   1.D0
   
      DAPDA(4,1) =   R*RDRPR*TNL*SNFD
      DAPDA(4,2) =   R*TNL*(1.D0-RDR*RDRPR*CSFD)
      DAPDA(4,3) =   RCPAR*TNL*(FID-R*RDRPR*SNFD)
      DAPDA(4,4) =   1.D0
      DAPDA(4,5) = - R*FID
   
      DAPDA(5,5) =   1.D0
C--
C  Copy error matrix to EEP and symmetrize it into EE.
C--
      CALL UVCOPY(30,HELXIN(9),EEP)
      N = 0
      DO 100 J = 1, 5
         DO 10 I = J, 5
            N       = N + 1
            EE(I,J) = EEP(N)
            EE(J,I) = EE (I,J)
10       CONTINUE
100   CONTINUE
C--
C  Transform error matrix EEP to that of XP.
C--
      N = 0
      DO 2000 J = 1, 5
         DO 200 I = J, 5
            N      = N + 1
            EEP(N) = 0.D0
            DO 20 K = 1, 5
               DO 2 L = 1, 5
                  EEP(N) = EEP(N) + DAPDA(I,K)*EE(K,L)*DAPDA(J,L)
2              CONTINUE
20          CONTINUE
200      CONTINUE
2000  CONTINUE
C--
C  Fill HELXOT array.
C--
      HELXOT(1) = DRP
      HELXOT(2) = FIP
      HELXOT(3) = CPA
      HELXOT(4) = DZP
      HELXOT(5) = TNL
      HELXOT(6) = XV
      HELXOT(7) = YV
      HELXOT(8) = ZV
      CALL UVCOPY(30,EEP,HELXOT(9))
C--
C  That's it.
C--
      RETURN
      END
