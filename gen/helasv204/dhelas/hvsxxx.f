      SUBROUTINE HVSXXX(VC,SC,G,SMASS,SWIDTH , HVS)
C
C This subroutine computes an off-shell scalar current from the vector-
C scalar-scalar coupling.  The coupling is absent in the minimal SM in
C unitary gauge.
C
C INPUT:
C       complex VC(6)          : input vector                          V
C       complex SC(3)          : input scalar                          S
C       real    G              : coupling constant (S charge)
C       real    SMASS          : mass  of OUTPUT scalar S'
C       real    SWIDTH         : width of OUTPUT scalar S'
C
C Examples of the coupling constant G for SUSY particles are as follows:
C   -----------------------------------------------------------
C   |    S1    | (Q,I3) of S1  ||   V=A   |   V=Z   |   V=W   |
C   -----------------------------------------------------------
C   | nu~_L    | (  0  , +1/2) ||   ---   |  GZN(1) |  GWF(1) |
C   | e~_L     | ( -1  , -1/2) ||  GAL(1) |  GZL(1) |  GWF(1) |
C   | u~_L     | (+2/3 , +1/2) ||  GAU(1) |  GZU(1) |  GWF(1) |
C   | d~_L     | (-1/3 , -1/2) ||  GAD(1) |  GZD(1) |  GWF(1) |
C   -----------------------------------------------------------
C   | e~_R-bar | ( +1  ,  0  ) || -GAL(2) | -GZL(2) | -GWF(2) |
C   | u~_R-bar | (-2/3 ,  0  ) || -GAU(2) | -GZU(2) | -GWF(2) |
C   | d~_R-bar | (+1/3 ,  0  ) || -GAD(2) | -GZD(2) | -GWF(2) |
C   -----------------------------------------------------------
C where the SC charge is defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex HVS(3)         : scalar current                J(S':V,S)
C
      COMPLEX*16 VC(6),SC(3),HVS(3),DG,QVV,QPV
      REAL*8     QV(0:3),QP(0:3),QA(0:3),G,SMASS,SWIDTH,Q2
C
      HVS(2) = VC(5)+SC(2)
      HVS(3) = VC(6)+SC(3)
C
      QV(0)=DBLE(  VC(2))
      QV(1)=DBLE(  VC(3))
      QV(2)=DIMAG( VC(3))
      QV(3)=DIMAG( VC(2))
      QP(0)=DBLE(  SC(2))
      QP(1)=DBLE(  SC(3))
      QP(2)=DIMAG( SC(3))
      QP(3)=DIMAG( SC(2))
      QA(0)=DBLE( HVS(2))
      QA(1)=DBLE( HVS(3))
      QA(2)=DIMAG(HVS(3))
      QA(3)=DIMAG(HVS(2))
      Q2=QA(0)**2-(QA(1)**2+QA(2)**2+QA(3)**2)
C
      DG=-G/DCMPLX( Q2-SMASS**2 , DMAX1(DSIGN( SMASS*SWIDTH ,Q2),0.D0) )
      QVV=QV(0)*VC(1)-QV(1)*VC(2)-QV(2)*VC(3)-QV(3)*VC(4)
      QPV=QP(0)*VC(1)-QP(1)*VC(2)-QP(2)*VC(3)-QP(3)*VC(4)
C
      HVS(1) = DG*(2.D0*QPV+QVV)*SC(1)
C
      RETURN
      END
