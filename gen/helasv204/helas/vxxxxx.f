      SUBROUTINE VXXXXX(P,VMASS,NHEL,NSV , VC)
C
C This subroutine computes a VECTOR wavefunction.                       
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum of vector boson          
C       real    VMASS          : mass          of vector boson          
C       integer NHEL = -1, 0, 1: helicity      of vector boson          
C                                (0 is forbidden if VMASS=0.0)          
C       integer NSV  = -1 or 1 : +1 for final, -1 for initial           
C                                                                       
C OUTPUT:                                                               
C       complex VC(6)          : vector wavefunction       epsilon^mu(V)
C
      COMPLEX VC(6)
      REAL    P(0:3),VMASS,HEL,HEL0,PT,PT2,PP,PZPT,EMP,SQH
      INTEGER NHEL,NSV,NSVAHL
C
      SQH=SQRT(.5)
      HEL=REAL(NHEL)
      NSVAHL=NSV*ABS(HEL)
      PT2=P(1)**2+P(2)**2
      PP=MIN(P(0),SQRT(PT2+P(3)**2))
      PT=MIN(PP,SQRT(PT2))
C
      VC(5) = CMPLX(P(0),P(3))*NSV
      VC(6) = CMPLX(P(1),P(2))*NSV
C
      IF (VMASS.EQ.0.) GOTO 10
C
      HEL0=1.0-ABS(HEL)
C
      IF (PP.EQ.0.) GOTO 20
C
      EMP=P(0)/(VMASS*PP)
        VC(1) = CMPLX( HEL0*PP/VMASS )
        VC(4) = CMPLX( HEL0*P(3)*EMP+HEL*PT/PP*SQH )
      IF (PT.NE.0.) THEN
         PZPT=P(3)/(PP*PT)*SQH*HEL
        VC(2) = CMPLX( HEL0*P(1)*EMP-P(1)*PZPT , -NSVAHL*P(2)/PT*SQH )
        VC(3) = CMPLX( HEL0*P(2)*EMP-P(2)*PZPT ,  NSVAHL*P(1)/PT*SQH )
      ELSE
        VC(2) = CMPLX(-HEL*SQH )
        VC(3) = CMPLX( 0. , NSVAHL*SIGN(SQH,P(3)) )
      ENDIF
      RETURN
C
  10  PP=P(0)
      PT=SQRT(P(1)**2+P(2)**2)
        VC(1) = CMPLX( 0. )
        VC(4) = CMPLX( HEL*PT/PP*SQH )
      IF (PT.NE.0.) THEN
         PZPT=P(3)/(PP*PT)*SQH*HEL
        VC(2) = CMPLX(-P(1)*PZPT , -NSV*P(2)/PT*SQH )
        VC(3) = CMPLX(-P(2)*PZPT ,  NSV*P(1)/PT*SQH )
      ELSE
        VC(2) = CMPLX(-HEL*SQH )
        VC(3) = CMPLX( 0. , NSV*SIGN(SQH,P(3)) )
      ENDIF
      RETURN
C
  20  VC(1) = CMPLX( 0. )
      VC(2) = CMPLX(-HEL*SQH )
      VC(3) = CMPLX( 0. , NSVAHL*SQH )
      VC(4) = CMPLX( HEL0 )
C
      RETURN
      END
