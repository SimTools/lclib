      SUBROUTINE EAIXXX(EB,EA,SHLF,CHLF,PHI,NHE,NHA , EAI)
C
C This subroutine computes an off-shell electron wavefunction after
C emitting a photon from the electron beam, with a special care for the
C small angle region.  The momenta are measured in the laboratory frame,
C where the e- beam is along the positive z axis.
C
C INPUT:
C       real    EB             : energy (GeV)    of beam  e-
C       real    EA             : energy (GeV)    of final photon
C       real    SHLF           : sin(theta/2)    of final photon
C       real    CHLF           : cos(theta/2)    of final photon
C       real    PHI            : azimuthal angle of final photon
C       integer NHE  = -1 or 1 : helicity        of beam  e-
C       integer NHA  = -1 or 1 : helicity        of final photon
C
C OUTPUT:
C       complex EAI(6)         : off-shell electron             |e',A,e>
C
      COMPLEX*16 EAI(6),PHS
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
      D=-1.D0/(EA*EB*(4.D0*SHLF**2+(ME/EB)**2*C))
      COEFF=-REAL(NN)*GAL*SQRT(EB)*D
      XNNP=X*REAL(1+NN)
      XNNM=X*REAL(1-NN)
      SNP=DSIN(PHI)
      CSP=DCOS(PHI)
      PHS=DCMPLX( CSP , RNHE*SNP )
C
      EAI((5-3*NHE)/2) = -RNHE*COEFF*ME*S*(1.D0+XNNP*0.5D0)
      EAI((5-NHE)/2)   =  XNNP*COEFF*ME*CHLF**2*PHS
      EAI((5+NHE)/2)   =  RNHE*COEFF*EB*S*(-2.D0+XNNM)
      EAI((5+3*NHE)/2) =  XNNM*COEFF*EB*SHLF**2*PHS*2.D0
C
      EAI(5) =  EB*DCMPLX( 1.D0-X , 1.D0-X*C )
      EAI(6) = -EB*X*S*DCMPLX( CSP , SNP )
C
      RETURN
      END
