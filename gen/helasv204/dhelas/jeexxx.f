      SUBROUTINE JEEXXX(EB,EF,SHLF,CHLF,PHI,NHB,NHF,NSF , JEE)
C
C This subroutine computes an off-shell photon wavefunction emitted from
C the electron or positron beam, with a special care for the small angle
C region.  The momenta are measured in the laboratory frame, where the
C e- (e+) beam is along the positive (negative) z axis.
C
C INPUT:
C       real    EB             : energy (GeV)    of beam  e-/e+
C       real    EF             : energy (GeV)    of final e-/e+
C       real    SHLF           : sin(theta/2)    of final e-/e+
C       real    CHLF           : cos(theta/2)    of final e-/e+
C       real    PHI            : azimuthal angle of final e-/e+
C       integer NHB  = -1 or 1 : helicity        of beam  e-/e+
C       integer NHF  = -1 or 1 : helicity        of final e-/e+
C       integer NSF  = -1 or 1 : +1 for electron, -1 for positron
C
C OUTPUT:
C       complex JEE(6)         : off-shell photon          J^mu(<e|A|e>)
C
      COMPLEX*16 JEE(6),COEFF
      REAL*8  CS(2),EB,EF,SHLF,CHLF,PHI,ME,ALPHA,GAL,HI,SF,SFH,X,ME2,Q2,
     &        RFP,RFM,SNP,CSP,RXC,C,S
      INTEGER*4 NHB,NHF,NSF
C
      PARAMETER (ME=0.51099906D-3)
      PARAMETER (ALPHA=1.D0/128.D0)
      PARAMETER (FOURPI=4.D0*3.14159265358979323846D0)
      GAL  =DSQRT(ALPHA*FOURPI)
C
      HI =DBLE(NHB)
      SF =DBLE(NSF)
      SFH=DBLE(NHB*NSF)
      CS((3+NSF)/2)=SHLF
      CS((3-NSF)/2)=CHLF
C CS(1)=CHLF and CS(2)=SHLF for electron
C CS(1)=SHLF and CS(2)=CHLF for positron
      X=EF/EB
      ME2=ME**2
      Q2=-4.D0*CS(2)**2*(EF*EB-ME2)
     &   +SF*(1.D0-X)**2/X*(SHLF+CHLF)*(SHLF-CHLF)*ME2
      RFP=DBLE(1+NSF)
      RFM=DBLE(1-NSF)
      SNP=DSIN(PHI)
      CSP=DCOS(PHI)
C
      IF (NHB.EQ.NHF) THEN
         RXC=2.D0*X/(1.D0-X)*CS(1)**2
         COEFF= GAL*2.D0*EB*DSQRT(X)*CS(2)/Q2
     &         *(DCMPLX( RFP )-RFM*DCMPLX( CSP ,-SNP*HI ))*0.5D0
         JEE(1) =  DCMPLX( 0.D0 )
         JEE(2) =  COEFF*DCMPLX( (1.D0+RXC)*CSP ,-SFH*SNP )
         JEE(3) =  COEFF*DCMPLX( (1.D0+RXC)*SNP , SFH*CSP )
         JEE(4) =  COEFF*(-SF*RXC/CS(1)*CS(2))
      ELSE
         COEFF= GAL*ME/Q2/DSQRT(X)
     &         *(DCMPLX( RFP )+RFM*DCMPLX( CSP , SNP*HI ))*0.5D0*HI
         JEE(1) = -COEFF*(1.D0+X)*CS(2)*DCMPLX( CSP , SFH*SNP )
         JEE(2) =  COEFF*(1.D0-X)*CS(1)
         JEE(3) =  JEE(2)*DCMPLX( 0.D0 , SFH )
         JEE(4) =  JEE(1)*SF*(1.D0-X)/(1.D0+X)
      ENDIF
C
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.D0*CHLF*SHLF
C
      JEE(5) = -EB*DCMPLX( 1.D0-X , SF-X*C )
      JEE(6) =  EB*X*S*DCMPLX( CSP , SNP )
C
      RETURN
      END
