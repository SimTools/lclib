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
      COMPLEX FO(6),CHI(2)
      REAL    P(0:3),SF(2),SFOMEG(2),OMEGA(2),FMASS,PP,PP3,SQP0P3,SQM
      INTEGER NHEL,NSF,NH,IP,IM
C
      FO(5) = CMPLX(P(0),P(3))*NSF
      FO(6) = CMPLX(P(1),P(2))*NSF
C
      NH=NHEL*NSF
C
      IF (FMASS.EQ.0.) GOTO 10
C
      PP=MIN(P(0),SQRT(P(1)**2+P(2)**2+P(3)**2))
C
      IF (PP.EQ.0.) GOTO 20
C
      PP=MIN(P(0),SQRT(P(1)**2+P(2)**2+P(3)**2))
      SF(1)=REAL(1+NSF+(1-NSF)*NH)*.5
      SF(2)=REAL(1+NSF-(1-NSF)*NH)*.5
      OMEGA(1)=SQRT(P(0)+PP)
      OMEGA(2)=FMASS/OMEGA(1)
      IP=(3+NH)/2
      IM=(3-NH)/2
      SFOMEG(1)=SF(1)*OMEGA(IP)
      SFOMEG(2)=SF(2)*OMEGA(IM)
      PP3=MAX(PP+P(3),0.)
      CHI(1)=CMPLX( SQRT(PP3*.5/PP) )
      IF (PP3.EQ.0.) THEN
         CHI(2)=CMPLX(-NH )
      ELSE
         CHI(2)=CMPLX( NH*P(1) , -P(2) )/SQRT(2.*PP*PP3)
      ENDIF
C
      FO(1) = SFOMEG(2)*CHI(IM)
      FO(2) = SFOMEG(2)*CHI(IP)
      FO(3) = SFOMEG(1)*CHI(IM)
      FO(4) = SFOMEG(1)*CHI(IP)
C
      RETURN
C
  10  SQP0P3=SQRT(MAX(P(0)+P(3),0.))*NSF
      CHI(1)=CMPLX( SQP0P3 )
      IF (SQP0P3.EQ.0.) THEN
         CHI(2)=CMPLX(-NHEL )*SQRT(2.*P(0))
      ELSE
         CHI(2)=CMPLX( NH*P(1), -P(2) )/SQP0P3
      ENDIF
      IF (NH.EQ.1) THEN
         FO(1) = CHI(1)
         FO(2) = CHI(2)
         FO(3) = CMPLX( 0. )
         FO(4) = CMPLX( 0. )
      ELSE
         FO(1) = CMPLX( 0. )
         FO(2) = CMPLX( 0. )
         FO(3) = CHI(2)
         FO(4) = CHI(1)
      ENDIF
      RETURN
C
  20  SQM=SQRT(FMASS)
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
