      SUBROUTINE EAOXXX(EB,EA,SHLF,CHLF,PHI,NHE,NHA , EAO)
C
C This subroutine computes an off-shell positron wavefunction after
C emitting a photon from the positron beam, with a special care for the
C small angle region.  The momenta are measured in the laboratory frame,
C where the e+ beam is along the negative z axis.
C
C INPUT:
C       real    EB             : energy (GeV)    of beam  e+
C       real    EA             : energy (GeV)    of final photon
C       real    SHLF           : sin(theta/2)    of final photon
C       real    CHLF           : cos(theta/2)    of final photon
C       real    PHI            : azimuthal angle of final photon
C       integer NHE  = -1 or 1 : helicity        of beam  e+
C       integer NHA  = -1 or 1 : helicity        of final photon
C
C OUTPUT:
C       complex EAO(6)         : off-shell positron             <e,A,e'|
C
      COMPLEX*16 EAO(6),PHS
      REAL*8  EB,EA,SHLF,CHLF,PHI,ME,ALPHA,GAL,RNHE,X,C,S,D,COEFF,
     &        XNNP,XNNM,SNP,CSP
      INTEGER*4 NHE,NHA,NN
C
      PARAMETER (ME=0.51099906D-3)
      PARAMETER (ALPHA=1.D0/128.D0)
      PARAMETER (FOURPI=4.D0*3.14159265358979323846D0)
      GAL  =DSQRT(ALPHA*FOURPI)
C
      NN=NHA*NHE
      RNHE=DBLE(NHE)
      X=EA/EB
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.D0*CHLF*SHLF
      D=-1.D0/(EA*EB*(4.D0*CHLF**2-(ME/EB)**2*C))
      COEFF=DBLE(NN)*GAL*DSQRT(EB)*D
      XNNP=X*DBLE(1+NN)
      XNNM=X*DBLE(1-NN)
      SNP=DSIN(PHI)
      CSP=DCOS(PHI)
      PHS=DCMPLX( CSP ,-RNHE*SNP )
C
      EAO((5-3*NHE)/2) =              COEFF*ME*S*(1.D0+XNNP*0.5D0)
      EAO((5-NHE)/2)   = RNHE*XNNP    *COEFF*ME*SHLF**2*PHS
      EAO((5+NHE)/2)   =              COEFF*EB*S*(-2.D0+XNNM)
      EAO((5+3*NHE)/2) = DBLE(NHA-NHE)*COEFF*EB*X*CHLF**2*PHS*2.D0
C
      EAO(5) = EB*DCMPLX( X-1.D0 , X*C+1.D0 )
      EAO(6) = EB*X*S*DCMPLX( CSP , SNP )
C
      RETURN
      END
