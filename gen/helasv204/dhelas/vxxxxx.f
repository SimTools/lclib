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
      COMPLEX*16 VC(6)
      REAL*8     P(0:3),VMASS,HEL,HEL0,PT,PT2,PP,PZPT,EMP,SQH
      INTEGER*4  NHEL,NSV,NSVAHL
C
      SQH=DSQRT(0.5D0)
      HEL=DBLE(NHEL)
      NSVAHL=NSV*ABS(HEL)
      PT2=P(1)**2+P(2)**2
      PP=DMIN1(P(0),DSQRT(PT2+P(3)**2))
      PT=DMIN1(PP,DSQRT(PT2))
C
      VC(5) = DCMPLX(P(0),P(3))*NSV
      VC(6) = DCMPLX(P(1),P(2))*NSV
C
      IF (VMASS.EQ.0.D0) GOTO 10
C
      HEL0=1.0-ABS(HEL)
C
      IF (PP.EQ.0.D0) GOTO 20
C
      EMP=P(0)/(VMASS*PP)
        VC(1) = DCMPLX( HEL0*PP/VMASS )
        VC(4) = DCMPLX( HEL0*P(3)*EMP+HEL*PT/PP*SQH )
      IF (PT.NE.0.D0) THEN
         PZPT=P(3)/(PP*PT)*SQH*HEL
        VC(2) = DCMPLX( HEL0*P(1)*EMP-P(1)*PZPT , -NSVAHL*P(2)/PT*SQH )
        VC(3) = DCMPLX( HEL0*P(2)*EMP-P(2)*PZPT ,  NSVAHL*P(1)/PT*SQH )
      ELSE
        VC(2) = DCMPLX(-HEL*SQH )
        VC(3) = DCMPLX( 0.D0 , NSVAHL*DSIGN(SQH,P(3)) )
      ENDIF
      RETURN
C
  10  PP=P(0)
      PT=DSQRT(P(1)**2+P(2)**2)
        VC(1) = DCMPLX( 0.D0 )
        VC(4) = DCMPLX( HEL*PT/PP*SQH )
      IF (PT.NE.0.D0) THEN
         PZPT=P(3)/(PP*PT)*SQH*HEL
        VC(2) = DCMPLX(-P(1)*PZPT , -NSV*P(2)/PT*SQH )
        VC(3) = DCMPLX(-P(2)*PZPT ,  NSV*P(1)/PT*SQH )
      ELSE
        VC(2) = DCMPLX(-HEL*SQH )
        VC(3) = DCMPLX( 0.D0 , NSV*DSIGN(SQH,P(3)) )
      ENDIF
      RETURN
C
  20  VC(1) = DCMPLX( 0.D0 )
      VC(2) = DCMPLX(-HEL*SQH )
      VC(3) = DCMPLX( 0.D0 , NSVAHL*SQH )
      VC(4) = DCMPLX( HEL0 )
C
      RETURN
      END
