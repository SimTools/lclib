      SUBROUTINE OXXXXX(P,FMASS,NHEL,NSF , FO)
C
C This subroutine computes a fermion wavefunction with the flowing-OUT
C fermion number.
C
C INPUT:
C       real    P(0:3)         : four-momentum of fermion
C       real    FMASS          : mass          of fermion
C       integer NHEL = -1 or 1 : helicity      of fermion
C       integer NSF  = -1 or 1 : +1 for particle, -1 for anti-particle
C
C OUTPUT:
C       complex FO(6)          : fermion wavefunction               <FO|
C
      COMPLEX*16 FO(6),CHI(2)
      REAL*8  P(0:3),SF(2),SFOMEG(2),OMEGA(2),FMASS,PP,PP3,SQP0P3,SQM
      INTEGER*4 NHEL,NSF,NH,IP,IM
C
      FO(5) = DCMPLX(P(0),P(3))*NSF
      FO(6) = DCMPLX(P(1),P(2))*NSF
C
      NH=NHEL*NSF
C
      IF (FMASS.EQ.0.D0) GOTO 10
C
      PP=DMIN1(P(0),DSQRT(P(1)**2+P(2)**2+P(3)**2))
C
      IF (PP.EQ.0.D0) GOTO 20
C
      PP=DMIN1(P(0),DSQRT(P(1)**2+P(2)**2+P(3)**2))
      SF(1)=DBLE(1+NSF+(1-NSF)*NH)*.5
      SF(2)=DBLE(1+NSF-(1-NSF)*NH)*.5
      OMEGA(1)=DSQRT(P(0)+PP)
      OMEGA(2)=FMASS/OMEGA(1)
      IP=(3+NH)/2
      IM=(3-NH)/2
      SFOMEG(1)=SF(1)*OMEGA(IP)
      SFOMEG(2)=SF(2)*OMEGA(IM)
      PP3=DMAX1(PP+P(3),0.D0)
      CHI(1)=DCMPLX( DSQRT(PP3*.5/PP) )
      IF (PP3.EQ.0.D0) THEN
         CHI(2)=DCMPLX(-NH )
      ELSE
         CHI(2)=DCMPLX( NH*P(1) , -P(2) )/DSQRT(2.D0*PP*PP3)
      ENDIF
C
      FO(1) = SFOMEG(2)*CHI(IM)
      FO(2) = SFOMEG(2)*CHI(IP)
      FO(3) = SFOMEG(1)*CHI(IM)
      FO(4) = SFOMEG(1)*CHI(IP)
C
      RETURN
C
  10  SQP0P3=DSQRT(DMAX1(P(0)+P(3),0.D0))*NSF
      CHI(1)=DCMPLX( SQP0P3 )
      IF (SQP0P3.EQ.0.D0) THEN
         CHI(2)=DCMPLX(-NHEL )*DSQRT(2.D0*P(0))
      ELSE
         CHI(2)=DCMPLX( NH*P(1), -P(2) )/SQP0P3
      ENDIF
      IF (NH.EQ.1) THEN
         FO(1) = CHI(1)
         FO(2) = CHI(2)
         FO(3) = DCMPLX( 0.D0 )
         FO(4) = DCMPLX( 0.D0 )
      ELSE
         FO(1) = DCMPLX( 0.D0 )
         FO(2) = DCMPLX( 0.D0 )
         FO(3) = CHI(2)
         FO(4) = CHI(1)
      ENDIF
      RETURN
C
  20  SQM=DSQRT(FMASS)
      IP=-((1+NH)/2)
      IM=  (1-NH)/2
C
      FO(1) = IM     * SQM
      FO(2) = IP*NSF * SQM
      FO(3) = IM*NSF * SQM
      FO(4) = IP     * SQM
C
      RETURN
      END
