C  *********************************************************************
C  *********************************************************************
C  ***                                                               ***
C  ***                                                               ***
C  ***   HHH    HH   EEEEEEEEE   LL              AA        SSSSS     ***
C  ***   HHH    HH   EE          LL            AA  AA    SS     SS   ***
C  ***   HHH    HH   EE          LL           AA    AA   SS          ***
C  ***   HHHHHHHHH   EEEEEEE     LL          AAA    AA     SSSSS     ***
C  ***   HHH    HH   EE          LL          AAAAAAAAA          SS   ***
C  ***   HHH    HH   EE          LL          AAA    AA   SS     SS   ***
C  ***   HHH    HH   EEEEEEEEE   LLLLLLLLL   AAA    AA     SSSSS     ***
C  ***                                                               ***
C  ***     CCCCC     HHH    HH   EEEEEEEEE     CCCCC     KKK    KK   ***
C  ***   CCC    CC   HHH    HH   EE          CCC    CC   KKK  KK     ***
C  ***   CCC         HHH    HH   EE          CCC         KKKKK       ***
C  ***   CCC         HHHHHHHHH   EEEEEEE     CCC         KKK KK      ***
C  ***   CCC         HHH    HH   EE          CCC         KKK  KK     ***
C  ***   CCC    CC   HHH    HH   EE          CCC    CC   KKK   KK    ***
C  ***     CCCCC     HHH    HH   EEEEEEEEE     CCCCC     KKK    KK   ***
C  ***                                                               ***
C  ***               coded by H. Murayama & I. Watanabe              ***
C  ***                          ver.  2.3                            ***
C  ***                       12th  Apr.  1992                        ***
C  ***                                                               ***
C  *** For the formalism and notations, see the following reference: ***
C  ***           H. Murayama, I. Watanabe and K. Hagiwara            ***
C  ***           "HELAS: HELicity Amplitude Subroutines              ***
C  ***               for Feynman diagram evaluation"                 ***
C  ***               KEK Report 91-11, December 1991                 ***
C  ***                                                               ***
C  *********************************************************************
C  *********************************************************************
C                                                                       
C  The subroutines are named as follows.                                
C                                                                       
C  External Lines:                                                      
C   | f >                     : fermion (flow-IN)            ==>  IXXXXX
C   < f |                     : fermion (flow-OUT)           ==>  OXXXXX
C   epsilon^mu , epsilon*^mu  : vector boson (initial,final) ==>  VXXXXX
C   S                         : scalar boson (initial,final) ==>  SXXXXX
C                                                                       
C  Vertices:                                                            
C   < f' V f >            : amplitude        of   FFV vertex ==>  IOVXXX
C   | f' V f >            : flow-in  fermion from FFV vertex ==>  FVIXXX
C   < f V f' |            : flow-out fermion from FFV vertex ==>  FVOXXX
C   J^mu(< f' | V | f >)  : vector   current from FFV vertex ==>  JIOXXX
C                         : W3       current from FFV vertex ==>  J3XXXX
C   < f' S f >            : amplitude        of   FFS vertex ==>  IOSXXX
C   | f' S f >            : flow-in  fermion from FFS vertex ==>  FSIXXX
C   < f S f' |            : flow-out fermion from FFS vertex ==>  FSOXXX
C   J(< f' | S | f >)     : scalar   current from FFS vertex ==>  HIOXXX
C   Gamma(V1,V2,V3)         : amplitude      of   VVV vertex ==>  VVVXXX
C   J^mu(V':V1,V2)          : vector current from VVV vertex ==>  JVVXXX
C   Gamma(V1,V2,S)          : amplitude      of   VVS vertex ==>  VVSXXX
C   J^mu(V':V,S)            : vector current from VVS vertex ==>  JVSXXX
C   J(S:V1,V2)              : scalar current from VVS vertex ==>  HVVXXX
C   Gamma(V,S1,S2)          : amplitude      of   VSS vertex ==>  VSSXXX
C   J^mu(V:S1,S2)           : vector current from VSS vertex ==>  JSSXXX
C   J(S':V,S)               : scalar current from VSS vertex ==>  HVSXXX
C   Gamma(S1,S2,S3)         : amplitude      of   SSS vertex ==>  SSSXXX
C   J(S':S1,S2)             : scalar current from SSS vertex ==>  HSSXXX
C   Gamma(WM,WP,WM,WP) : amplitude of   4-point W+/W- vertex ==>  WWWWXX
C   J^mu(W':W1,W2,W3)  : W current from 4-point W+/W- vertex ==>  JWWWXX
C   Gamma(WM,W3,WP,W3) : amplitude of   4-point W/W3  vertex ==>  W3W3XX
C   J^mu(W':W1,W2,W3)  : W current from 4-point W/W3  vertex ==>  JW3WXX
C   Gamma(V1,V2,S1,S2)   : amplitude        of   VVSS vertex ==>  VVSSXX
C   J^mu(V':V,S1,S2)     : vector current   from VVSS vertex ==>  JVSSXX
C   J(S':V1,V2,S)        : scalar current   from VVSS vertex ==>  HVVSXX
C   Gamma(S1,S2,S3,S4)   : amplitude        of   SSSS vertex ==>  SSSSXX
C   J(S':S1,S2,S3)       : scalar current   from SSSS vertex ==>  HSSSXX
C                                                                       
C  Special Vertices:                                                    
C   | e' A e- >               : initial electron with photon ==>  EAIXXX
C   < e+ A e' |               : initial positron with photon ==>  EAOXXX
C   J^mu(< e+ | A | e->)      : t-channel photon from e-/e+  ==>  JEEXXX
C                                                                       
C  Utilities for Momentum Manipulations:                                
C   P^mu(energy,mass,costh,phi)          : set up 4-momentum ==>  MOMNTX
C   P1^mu & P2^mu   : set up two 4-momenta in 1 2 rest frame ==>  MOM2CX
C   P_boosted                  : Lorentz boost of 4-momentum ==>  BOOSTX
C   P_rotated                       : rotation of 4-momentum ==>  ROTXXX
C                                                                       
C  Standard Model Coupling Constants:                                   
C   for VVV,VVVV vertices                                    ==>  COUP1X
C   for FFV vertices                                         ==>  COUP2X
C   for VVS,SSS,VVSS,SSSS vertices                           ==>  COUP3X
C   for FFS vertices                                         ==>  COUP4X
C                                                                       
C **********************************************************************
C
      SUBROUTINE IXXXXX(P,FMASS,NHEL,NSF , FI)
C
C This subroutine computes a fermion wavefunction with the flowing-IN   
C fermion number.                                                       
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum of fermion               
C       real    FMASS          : mass          of fermion               
C       integer NHEL = -1 or 1 : helicity      of fermion               
C       integer NSF  = -1 or 1 : +1 for particle, -1 for anti-particle  
C                                                                       
C OUTPUT:                                                               
C       complex FI(6)          : fermion wavefunction               |FI>
C
      COMPLEX FI(6),CHI(2)
      REAL    P(0:3),SF(2),SFOMEG(2),OMEGA(2),FMASS,PP,PP3,SQP0P3,SQM
      INTEGER NHEL,NSF,IP,IM,NH
         REAL P2
C
C .........CHECK........................................................
          PP=SQRT(P(1)**2+P(2)**2+P(3)**2)
          IF (ABS(P(0))+PP.EQ.0.)
     &    WRITE(99,*) ' HELAS-ERROR : P(0:3) in IXXXXX is zero momentum'
          IF (P(0).LE.0.) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : P(0:3) in IXXXXX has non-positive energy'
          WRITE(1,*) '               P(0) = ',P(0)
          ENDIF
          P2=(P(0)-PP)*(P(0)+PP)
          IF (ABS(P2-FMASS**2).GT.P(0)**2*2.E-5) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : P(0:3) in IXXXXX has inappropriate mass'
          WRITE(1,*)
     &    '               P**2 = ',P2,' : FMASS**2 = ',FMASS**2
          ENDIF
          IF (ABS(NHEL).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHEL in IXXXXX is not -1,1'
          WRITE(1,*) '               NHEL = ',NHEL
          ENDIF
          IF (ABS(NSF).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NSF in IXXXXX is not -1,1'
          WRITE(1,*) '               NSF = ',NSF
          ENDIF
C .........CHECK........................................................
C
      FI(5) = CMPLX(P(0),P(3))*NSF
      FI(6) = CMPLX(P(1),P(2))*NSF
C
      NH=NHEL*NSF
C
      IF (FMASS.EQ.0.) GOTO 10
C
      PP=MIN(P(0),SQRT(P(1)**2+P(2)**2+P(3)**2))
C
      IF (PP.EQ.0.) GOTO 20
C
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
         CHI(2)=CMPLX( NH*P(1) , P(2) )/SQRT(2.*PP*PP3)
      ENDIF
C
      FI(1) = SFOMEG(1)*CHI(IM)
      FI(2) = SFOMEG(1)*CHI(IP)
      FI(3) = SFOMEG(2)*CHI(IM)
      FI(4) = SFOMEG(2)*CHI(IP)
C
      RETURN
C
  10  SQP0P3=SQRT(MAX(P(0)+P(3),0.))*NSF
      CHI(1)=CMPLX( SQP0P3 )
      IF (SQP0P3.EQ.0.) THEN
         CHI(2)=CMPLX(-NHEL )*SQRT(2.*P(0))
      ELSE
         CHI(2)=CMPLX( NH*P(1), P(2) )/SQP0P3
      ENDIF
      IF (NH.EQ.1) THEN
         FI(1) = CMPLX( 0. )
         FI(2) = CMPLX( 0. )
         FI(3) = CHI(1)
         FI(4) = CHI(2)
      ELSE
         FI(1) = CHI(2)
         FI(2) = CHI(1)
         FI(3) = CMPLX( 0. )
         FI(4) = CMPLX( 0. )
      ENDIF
      RETURN
C
  20  SQM=SQRT(FMASS)
      IP=(1+NH)/2
      IM=(1-NH)/2
C
      FI(1) = IP     * SQM
      FI(2) = IM*NSF * SQM
      FI(3) = IP*NSF * SQM
      FI(4) = IM     * SQM
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
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
         REAL P2
C
C .........CHECK........................................................
          PP=SQRT(P(1)**2+P(2)**2+P(3)**2)
          IF (ABS(P(0))+PP.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : P(0:3) in OXXXXX is zero momentum'
          IF (P(0).LE.0.) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : P(0:3) in OXXXXX has non-positive energy'
          WRITE(1,*) '               P(0) = ',P(0)
          ENDIF
          P2=(P(0)-PP)*(P(0)+PP)
          IF (ABS(P2-FMASS**2).GT.P(0)**2*2.E-5) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : P(0:3) in OXXXXX has inappropriate mass'
          WRITE(1,*)
     &    '               P**2 = ',P2,' : FMASS**2 = ',FMASS**2
          ENDIF
          IF (ABS(NHEL).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHEL in OXXXXX is not -1,1'
          WRITE(1,*) '               NHEL = ',NHEL
          ENDIF
          IF (ABS(NSF).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NSF in OXXXXX is not -1,1'
          WRITE(1,*) '               NSF = ',NSF
          ENDIF
C .........CHECK........................................................
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
C
C ----------------------------------------------------------------------
C
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
         REAL P2
C
C .........CHECK........................................................
          PP=SQRT(P(1)**2+P(2)**2+P(3)**2)
          IF (ABS(P(0))+PP.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : P(0:3) in VXXXXX is zero momentum'
          IF (P(0).LE.0.) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : P(0:3) in VXXXXX has non-positive energy'
          WRITE(1,*) '               P(0) = ',P(0)
          ENDIF
          P2=(P(0)+PP)*(P(0)-PP)
          IF (ABS(P2-VMASS**2).GT.P(0)**2*2.E-5) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : P(0:3) in VXXXXX has inappropriate mass'
          WRITE(1,*)
     &    '               P**2 = ',P2,' : VMASS**2 = ',VMASS**2
          ENDIF
          IF (VMASS.NE.0.) THEN
          IF (ABS(NHEL).GT.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHEL in VXXXXX is not -1,0,1'
          WRITE(1,*) '               NHEL = ',NHEL
          ENDIF
          ELSE
          IF (ABS(NHEL).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHEL in VXXXXX is not -1,1'
          WRITE(1,*) '               NHEL = ',NHEL
          ENDIF
          ENDIF
          IF (ABS(NSV).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NSV in VMXXXX is not -1,1'
          WRITE(1,*) '               NSV = ',NSV
          ENDIF
C .........CHECK........................................................
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
C
C .........CHECK........................................................
C NHEL=4 option for scalar polarization
      IF(NHEL.EQ.4) THEN
        IF(VMASS.EQ.0.) THEN
          VC(1) = 1.
          VC(2) = P(1)/P(0)
          VC(3) = P(2)/P(0)
          VC(4) = P(3)/P(0)
        ELSE
          VC(1) = P(0)/VMASS
          VC(2) = P(1)/VMASS
          VC(3) = P(2)/VMASS
          VC(4) = P(3)/VMASS
        ENDIF
        RETURN
      ENDIF
C .........CHECK........................................................
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
C
C ----------------------------------------------------------------------
C
      SUBROUTINE SXXXXX(P,NSS , SC)
C
C This subroutine computes a complex SCALAR wavefunction.               
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum of scalar boson          
C       integer NSS  = -1 or 1 : +1 for final, -1 for initial           
C                                                                       
C OUTPUT:                                                               
C       complex SC(3)          : scalar wavefunction                   S
C
      COMPLEX SC(3)
      REAL    P(0:3)
      INTEGER NSS
         REAL P2
C
C .........CHECK........................................................
          IF (ABS(P(0))+ABS(P(1))+ABS(P(2))+ABS(P(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : P(0:3) in SXXXXX is zero momentum'
          IF (P(0).LE.0.) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : P(0:3) in SXXXXX has non-positive energy'
          WRITE(1,*) '               P(0) = ',P(0)
          ENDIF
          P2=P(0)**2-(P(1)**2+P(2)**2+P(3)**2)
          IF (P2.LT.-P(0)**2*2.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : P(0:3) in SXXXXX is spacelike'
          WRITE(1,*) '               P**2 = ',P2
          ENDIF
          IF (ABS(NSS).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NSS in SXXXXX is not -1,1'
          WRITE(1,*) '               NSS = ',NSS
          ENDIF
C .........CHECK........................................................
C
      SC(1) = CMPLX( 1.0 )
      SC(2) = CMPLX(P(0),P(3))*NSS
      SC(3) = CMPLX(P(1),P(2))*NSS
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE IOVXXX(FI,FO,VC,G , VERTEX)
C
C This subroutine computes an amplitude of the fermion-fermion-vector   
C coupling.                                                             
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex VC(6)          : input    vector                      V 
C       real    G(2)           : coupling constants                  GVF
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude                     <FO|V|FI>
C
      COMPLEX FI(6),FO(6),VC(6),VERTEX
      REAL    G(2)
         REAL P0,P1,P2,P3,Q0,Q1,Q2,Q3,R0,R1,R2,R3,PM
C
C .........CHECK........................................................
          P0=-REAL( FI(5))
          P1=-REAL( FI(6))
          P2=-AIMAG(FI(6))
          P3=-AIMAG(FI(5))
          Q0=REAL( FO(5))
          Q1=REAL( FO(6))
          Q2=AIMAG(FO(6))
          Q3=AIMAG(FO(5))
          R0=REAL( VC(5))
          R1=REAL( VC(6))
          R2=AIMAG(VC(6))
          R3=AIMAG(VC(5))
          IF (ABS(FI(1))+ABS(FI(2))+ABS(FI(3))+ABS(FI(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FI(6) in IOVXXX is zero spinor'
          IF (ABS(FI(5))+ABS(FI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FI(6) in IOVXXX has zero momentum'
          IF (ABS(FO(1))+ABS(FO(2))+ABS(FO(3))+ABS(FO(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FO(6) in IOVXXX is zero spinor'
          IF (ABS(FO(5))+ABS(FO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FO(6) in IOVXXX has zero momentum'
          IF (ABS(VC(1))+ABS(VC(2))+ABS(VC(3))+ABS(VC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : VC(6) in IOVXXX is zero vector'
          IF (ABS(VC(5))+ABS(VC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : VC(6) in IOVXXX has zero momentum'
          PM=MAX( ABS(P0),ABS(Q0),ABS(R0),ABS(P1),ABS(Q1),ABS(R1),
     &            ABS(P2),ABS(Q2),ABS(R2),ABS(P3),ABS(Q3),ABS(R3) )
          IF (( ABS(-FI(5)+FO(5)+VC(5))+ABS(-FI(6)+FO(6)+VC(6)))
     &        .GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : FI(6),FO(6),VC(6) in IOVXXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (ABS(G(1))+ABS(G(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G(2) in IOVXXX is zero coupling'
C .........CHECK........................................................
C
      VERTEX =  G(1)*( (FO(3)*FI(1)+FO(4)*FI(2))*VC(1)
     &                +(FO(3)*FI(2)+FO(4)*FI(1))*VC(2)
     &                -(FO(3)*FI(2)-FO(4)*FI(1))*VC(3)*CMPLX(0.,1.)
     &                +(FO(3)*FI(1)-FO(4)*FI(2))*VC(4)             )
      IF (G(2).NE.0.) VERTEX = VERTEX
     &        + G(2)*( (FO(1)*FI(3)+FO(2)*FI(4))*VC(1)
     &                -(FO(1)*FI(4)+FO(2)*FI(3))*VC(2)
     &                +(FO(1)*FI(4)-FO(2)*FI(3))*VC(3)*CMPLX(0.,1.)
     &                -(FO(1)*FI(3)-FO(2)*FI(4))*VC(4)             )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FVIXXX(FI,VC,G,FMASS,FWIDTH , FVI)
C
C This subroutine computes an off-shell fermion wavefunction from a     
C flowing-IN external fermion and a vector boson.                       
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex VC(6)          : input    vector                      V 
C       real    G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'             
C       real    FWIDTH         : width of OUTPUT fermion F'             
C                                                                       
C OUTPUT:                                                               
C       complex FVI(6)         : off-shell fermion             |F',V,FI>
C
      COMPLEX FI(6),VC(6),FVI(6),SL1,SL2,SR1,SR2,D,CI
      REAL    G(2),PF(0:3),FMASS,FWIDTH,PF2
C
C .........CHECK........................................................
          IF (ABS(FI(1))+ABS(FI(2))+ABS(FI(3))+ABS(FI(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FI(6) in FVIXXX is zero spinor'
          IF (ABS(FI(5))+ABS(FI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FI(6) in FVIXXX has zero momentum'
          IF (ABS(VC(1))+ABS(VC(2))+ABS(VC(3))+ABS(VC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : VC(6) in FVIXXX is zero vector'
          IF (ABS(VC(5))+ABS(VC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : VC(6) in FVIXXX has zero momentum'
          IF (ABS(G(1))+ABS(G(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G(2) in FVIXXX is zero coupling'
          IF (FMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FMASS in FVIXXX is negative'
          WRITE(1,*) '               FMASS = ',FMASS
          ENDIF
          IF (FWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FWIDTH in FVIXXX is negative'
          WRITE(1,*) '               FWIDTH = ',FWIDTH
          ENDIF
C .........CHECK........................................................
C
      FVI(5) = FI(5)-VC(5)
      FVI(6) = FI(6)-VC(6)
C
      PF(0)=REAL( FVI(5))
      PF(1)=REAL( FVI(6))
      PF(2)=AIMAG(FVI(6))
      PF(3)=AIMAG(FVI(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
C .........CHECK........................................................
          IF (ABS(FVI(5))+ABS(FVI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FVI(6) in FVIXXX has zero momentum'
          IF ((FWIDTH.EQ.0.).AND.(PF2.EQ.FMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : FVI(6) in FVIXXX is on FMASS pole'
          WRITE(1,*) '               P     = ',PF(0),PF(1),PF(2),PF(3)
          WRITE(1,*) '               abs(P)= ',SQRT(ABS(PF2))
          FVI(1)=CMPLX(0.)
          FVI(2)=CMPLX(0.)
          FVI(3)=CMPLX(0.)
          FVI(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FI(1)
     &    +(VC(2)-CI*VC(3))*FI(2)
      SL2= (VC(2)+CI*VC(3))*FI(1)
     &    +(VC(1)-   VC(4))*FI(2)
C
      IF (G(2).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FI(3)
     &    -(VC(2)-CI*VC(3))*FI(4)
      SR2=-(VC(2)+CI*VC(3))*FI(3)
     &    +(VC(1)+   VC(4))*FI(4)
C
      FVI(1) = ( G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVI(2) = ( G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
      FVI(3) = ( G(2)*((PF(0)+PF(3))*SR1 +CONJG(FVI(6))*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVI(4) = ( G(2)*(       FVI(6)*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
C
      RETURN          
C
  10  FVI(1) = G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)*D
      FVI(2) = G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)*D
      FVI(3) = G(1)*FMASS*SL1*D
      FVI(4) = G(1)*FMASS*SL2*D
C
      RETURN          
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FVOXXX(FO,VC,G,FMASS,FWIDTH , FVO)
C
C This subroutine computes an off-shell fermion wavefunction from a     
C flowing-OUT external fermion and a vector boson.                      
C                                                                       
C INPUT:                                                                
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex VC(6)          : input    vector                      V 
C       real    G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'             
C       real    FWIDTH         : width of OUTPUT fermion F'             
C                                                                       
C OUTPUT:                                                               
C       complex FVO(6)         : off-shell fermion             <FO,V,F'|
C
      COMPLEX FO(6),VC(6),FVO(6),SL1,SL2,SR1,SR2,D,CI
      REAL    G(2),PF(0:3),FMASS,FWIDTH,PF2
C
C .........CHECK........................................................
          IF (ABS(FO(1))+ABS(FO(2))+ABS(FO(3))+ABS(FO(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FO(6) in FVOXXX is zero spinor'
          IF (ABS(FO(5))+ABS(FO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FO(6) in FVOXXX has zero momentum'
          IF (ABS(VC(1))+ABS(VC(2))+ABS(VC(3))+ABS(VC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : VC(6) in FVOXXX is zero vector'
          IF (ABS(VC(5))+ABS(VC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : VC(6) in FVOXXX has zero momentum'
          IF (ABS(G(1))+ABS(G(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G(2) in FVOXXX is zero coupling'
          IF (FMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FMASS in FVOXXX is negative'
          WRITE(1,*) '               FMASS = ',FMASS
          ENDIF
          IF (FWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FWIDTH in FVOXXX is negative'
          WRITE(1,*) '               FWIDTH = ',FWIDTH
          ENDIF
C .........CHECK........................................................
C
      FVO(5) = FO(5)+VC(5)
      FVO(6) = FO(6)+VC(6)
C
      PF(0)=REAL( FVO(5))
      PF(1)=REAL( FVO(6))
      PF(2)=AIMAG(FVO(6))
      PF(3)=AIMAG(FVO(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
C .........CHECK........................................................
          IF (ABS(FVO(5))+ABS(FVO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FVO(6) in FVOXXX has zero momentum'
          IF ((FWIDTH.EQ.0.).AND.(PF2.EQ.FMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : FVO(6) in FVOXXX is on FMASS pole'
          WRITE(1,*) '               P     = ',PF(0),PF(1),PF(2),PF(3)
          WRITE(1,*) '               abs(P)= ',SQRT(ABS(PF2))
          FVO(1)=CMPLX(0.)
          FVO(2)=CMPLX(0.)
          FVO(3)=CMPLX(0.)
          FVO(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FO(3)
     &    +(VC(2)+CI*VC(3))*FO(4)
      SL2= (VC(2)-CI*VC(3))*FO(3)
     &    +(VC(1)-   VC(4))*FO(4)
C
      IF (G(2).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FO(1)
     &    -(VC(2)+CI*VC(3))*FO(2)
      SR2=-(VC(2)-CI*VC(3))*FO(1)
     &    +(VC(1)+   VC(4))*FO(2)
C
      FVO(1) = ( G(2)*( (PF(0)+PF(3))*SR1        +FVO(6)*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVO(2) = ( G(2)*( CONJG(FVO(6))*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
      FVO(3) = ( G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVO(4) = ( G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
C
      RETURN          
C
  10  FVO(1) = G(1)*FMASS*SL1*D
      FVO(2) = G(1)*FMASS*SL2*D
      FVO(3) = G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)*D
      FVO(4) = G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)*D
C
      RETURN          
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JIOXXX(FI,FO,G,VMASS,VWIDTH , JIO)
C
C This subroutine computes an off-shell vector current from an external 
C fermion pair.  The vector boson propagator is given in Feynman gauge  
C for a massless vector and in unitary gauge for a massive vector.      
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       real    G(2)           : coupling constants                  GVF
C       real    VMASS          : mass  of OUTPUT vector V               
C       real    VWIDTH         : width of OUTPUT vector V               
C                                                                       
C OUTPUT:                                                               
C       complex JIO(6)         : vector current          J^mu(<FO|V|FI>)
C
      COMPLEX FI(6),FO(6),JIO(6),C0,C1,C2,C3,CS,D
      REAL    G(2),Q(0:3),VMASS,VWIDTH,Q2,VM2,DD
C
C .........CHECK........................................................
          IF (ABS(FI(1))+ABS(FI(2))+ABS(FI(3))+ABS(FI(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FI(6) in JIOXXX is zero spinor'
          IF (ABS(FI(5))+ABS(FI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FI(6) in JIOXXX has zero momentum'
          IF (ABS(FO(1))+ABS(FO(2))+ABS(FO(3))+ABS(FO(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FO(6) in JIOXXX is zero spinor'
          IF (ABS(FO(5))+ABS(FO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FO(6) in JIOXXX has zero momentum'
          IF (ABS(G(1))+ABS(G(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G(2) in JIOXXX is zero coupling'
          IF (VMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VMASS in JIOXXX is negative'
          WRITE(1,*) '               VMASS = ',VMASS
          ENDIF
          IF (VWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VWIDTH in JIOXXX is negative'
          WRITE(1,*) '               VWIDTH = ',VWIDTH
          ENDIF
C .........CHECK........................................................
C
      JIO(5) = FO(5)-FI(5)
      JIO(6) = FO(6)-FI(6)
C
      Q(0)=REAL( JIO(5))
      Q(1)=REAL( JIO(6))
      Q(2)=AIMAG(JIO(6))
      Q(3)=AIMAG(JIO(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
C .........CHECK........................................................
          IF (ABS(JIO(5))+ABS(JIO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : JIO(6) in JIOXXX has zero momentum'
          IF ((VWIDTH.EQ.0.).AND.(Q2.EQ.VM2)) THEN
          WRITE(1,*) ' HELAS-ERROR : JIO(6) in JIOXXX is on VMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          JIO(1)=CMPLX(0.)
          JIO(2)=CMPLX(0.)
          JIO(3)=CMPLX(0.)
          JIO(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      IF (VMASS.EQ.0.) GOTO 50
C
      D=1./CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above D.
C      D=1./CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      IF (G(2).EQ.0.) GOTO 10
C
      C0=  G(1)*( FO(3)*FI(1)+FO(4)*FI(2))
     &    +G(2)*( FO(1)*FI(3)+FO(2)*FI(4))
      C1= -G(1)*( FO(3)*FI(2)+FO(4)*FI(1))
     &    +G(2)*( FO(1)*FI(4)+FO(2)*FI(3))
      C2=( G(1)*( FO(3)*FI(2)-FO(4)*FI(1)) 
     &    +G(2)*(-FO(1)*FI(4)+FO(2)*FI(3)))*CMPLX(0.,1.)
      C3=  G(1)*(-FO(3)*FI(1)+FO(4)*FI(2))
     &    +G(2)*( FO(1)*FI(3)-FO(2)*FI(4))
      CS=(Q(0)*C0-Q(1)*C1-Q(2)*C2-Q(3)*C3)/VM2
C
      JIO(1) = (C0-CS*Q(0))*D
      JIO(2) = (C1-CS*Q(1))*D
      JIO(3) = (C2-CS*Q(2))*D
      JIO(4) = (C3-CS*Q(3))*D
C
      RETURN
C
  10  D=D*G(1)
      C0=  FO(3)*FI(1)+FO(4)*FI(2)
      C1= -FO(3)*FI(2)-FO(4)*FI(1)
      C2=( FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,1.)
      C3= -FO(3)*FI(1)+FO(4)*FI(2)
      CS=(Q(0)*C0-Q(1)*C1-Q(2)*C2-Q(3)*C3)/VM2
C
      JIO(1) = (C0-CS*Q(0))*D
      JIO(2) = (C1-CS*Q(1))*D
      JIO(3) = (C2-CS*Q(2))*D
      JIO(4) = (C3-CS*Q(3))*D
C
      RETURN
C
  50  CONTINUE
C
      DD=1./Q2
C
      IF (G(2).EQ.0.) GOTO 60
C
      JIO(1) = ( G(1)*( FO(3)*FI(1)+FO(4)*FI(2))
     &          +G(2)*( FO(1)*FI(3)+FO(2)*FI(4)) )*DD
      JIO(2) = (-G(1)*( FO(3)*FI(2)+FO(4)*FI(1))
     &          +G(2)*( FO(1)*FI(4)+FO(2)*FI(3)) )*DD
      JIO(3) = ( G(1)*( FO(3)*FI(2)-FO(4)*FI(1))
     &          +G(2)*(-FO(1)*FI(4)+FO(2)*FI(3)) )*CMPLX(0.,DD)
      JIO(4) = ( G(1)*(-FO(3)*FI(1)+FO(4)*FI(2))
     &          +G(2)*( FO(1)*FI(3)-FO(2)*FI(4)) )*DD
C
      RETURN
C
  60  DD=DD*G(1)
C
      JIO(1) =  ( FO(3)*FI(1)+FO(4)*FI(2))*DD
      JIO(2) = -( FO(3)*FI(2)+FO(4)*FI(1))*DD
      JIO(3) =  ( FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,DD)
      JIO(4) =  (-FO(3)*FI(1)+FO(4)*FI(2))*DD
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE J3XXXX(FI,FO,GAF,GZF,ZMASS,ZWIDTH , J3)
C
C This subroutine computes the sum of photon and Z currents with the    
C suitable weights ( J(W3) = cos(theta_W) J(Z) + sin(theta_W) J(A) ).   
C The output J3 is useful as an input of VVVXXX, JVVXXX or W3W3XX.      
C The photon propagator is given in Feynman gauge, and the Z propagator 
C is given in unitary gauge.                                            
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       real    GAF(2)         : FI couplings with A                 GAF
C       real    GZF(2)         : FI couplings with Z                 GZF
C       real    ZMASS          : mass  of Z                             
C       real    ZWIDTH         : width of Z                             
C                                                                       
C OUTPUT:                                                               
C       complex J3(6)          : W3 current             J^mu(<FO|W3|FI>)
C
      COMPLEX FI(6),FO(6),J3(6),
     &        C0L,C1L,C2L,C3L,CSL,C0R,C1R,C2R,C3R,CSR,DZ,DDIF
      REAL    GAF(2),GZF(2),Q(0:3),ZMASS,ZWIDTH,ZM2,ZMW,Q2,DA,WW,
     &        CW,SW,GN,GZ3L,GA3L
C
C .........CHECK........................................................
          IF (ABS(FI(1))+ABS(FI(2))+ABS(FI(3))+ABS(FI(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FI(6) in J3XXXX is zero spinor'
          IF (ABS(FI(5))+ABS(FI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FI(6) in J3XXXX has zero momentum'
          IF (ABS(FO(1))+ABS(FO(2))+ABS(FO(3))+ABS(FO(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FO(6) in J3XXXX is zero spinor'
          IF (ABS(FO(5))+ABS(FO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FO(6) in J3XXXX has zero momentum'
          IF (ABS(GAF(1))+ABS(GAF(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GAF(2) in J3XXXX is zero coupling'
          IF (ABS(GZF(1))+ABS(GZF(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GZF(2) in J3XXXX is zero coupling'
          IF (GAF(1).NE.GAF(2)) THEN
          WRITE(1,*)
     &    ' HELAS-warn  : GAF(2) in J3XXXX is non-standard coupling'
          WRITE(1,*) '               GAF(2) = ( ',GAF(1),GAF(2),' )'
          ENDIF
          IF ((GZF(1)*GZF(2).GT.0.).OR.(ABS(GZF(1)).LE.ABS(GZF(2))))
     &    THEN
          WRITE(1,*)
     &    ' HELAS-warn  : GZF(2) in J3XXXX is non-standard coupling'
          WRITE(1,*) '               GZF(2) = ( ',GZF(1),GZF(2),' )'
          ENDIF
          IF (ZMASS.LE.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : ZMASS in J3XXXX is not positive'
          WRITE(1,*) '               ZMASS = ',ZMASS
          ENDIF
          IF (ZWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : ZWIDTH in J3XXXX is negative'
          WRITE(1,*) '               ZWIDTH = ',ZWIDTH
          ENDIF
C .........CHECK........................................................
C
      J3(5) = FO(5)-FI(5)
      J3(6) = FO(6)-FI(6)
C
      Q(0)=-REAL( J3(5))
      Q(1)=-REAL( J3(6))
      Q(2)=-AIMAG(J3(6))
      Q(3)=-AIMAG(J3(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      ZM2=ZMASS**2
      ZMW=ZMASS*ZWIDTH
C
C .........CHECK........................................................
          IF (ABS(J3(5))+ABS(J3(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : J3(6) in J3XXXX has zero momentum'
          IF (Q2.EQ.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : J3(6) in J3XXXX is on photon pole'
          WRITE(1,*) '               Q = ',Q(0),Q(1),Q(2),Q(3)
          J3(1)=CMPLX(0.)
          J3(2)=CMPLX(0.)
          J3(3)=CMPLX(0.)
          J3(4)=CMPLX(0.)
          RETURN
          ENDIF
          IF ((ZWIDTH.EQ.0.).AND.(Q2.EQ.ZM2)) THEN
          WRITE(1,*) ' HELAS-ERROR : J3(6) in J3XXXX is on Z pole'
          WRITE(1,*) '               Q        = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q**2)= ',SQRT(ABS(Q2))
          J3(1)=CMPLX(0.)
          J3(2)=CMPLX(0.)
          J3(3)=CMPLX(0.)
          J3(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DA=1./Q2
      WW=MAX(SIGN( ZMW ,Q2),0.)
      DZ=1./CMPLX( Q2-ZM2 , WW )
      DDIF=CMPLX( -ZM2 , WW )*DA*DZ
C DDIF is the difference : DDIF=DA-DZ
C  For the running width, use below instead of the above WW,DZ and DDIF.
C      WW=MAX( ZWIDTH*Q2/ZMASS ,0.)
C      DZ=1./CMPLX( Q2-ZM2 , WW )
C      DDIF=CMPLX( -ZM2 , WW )*DA*DZ
C
      CW=1./SQRT(1.+(GZF(2)/GAF(2))**2)
      SW=SQRT((1.-CW)*(1.+CW))
      GN=GAF(2)*SW
      GZ3L=GZF(1)*CW
      GA3L=GAF(1)*SW
      C0L=  FO(3)*FI(1)+FO(4)*FI(2)
      C0R=  FO(1)*FI(3)+FO(2)*FI(4)
      C1L=-(FO(3)*FI(2)+FO(4)*FI(1))
      C1R=  FO(1)*FI(4)+FO(2)*FI(3)
      C2L= (FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,1.)
      C2R=(-FO(1)*FI(4)+FO(2)*FI(3))*CMPLX(0.,1.)
      C3L= -FO(3)*FI(1)+FO(4)*FI(2)
      C3R=  FO(1)*FI(3)-FO(2)*FI(4)
      CSL=(Q(0)*C0L-Q(1)*C1L-Q(2)*C2L-Q(3)*C3L)/ZM2
      CSR=(Q(0)*C0R-Q(1)*C1R-Q(2)*C2R-Q(3)*C3R)/ZM2
C
      J3(1) =  GZ3L*DZ*(C0L-CSL*Q(0))+GA3L*C0L*DA
     &       + GN*(C0R*DDIF+CSR*Q(0)*DZ)
      J3(2) =  GZ3L*DZ*(C1L-CSL*Q(1))+GA3L*C1L*DA
     &       + GN*(C1R*DDIF+CSR*Q(1)*DZ)
      J3(3) =  GZ3L*DZ*(C2L-CSL*Q(2))+GA3L*C2L*DA
     &       + GN*(C2R*DDIF+CSR*Q(2)*DZ)
      J3(4) =  GZ3L*DZ*(C3L-CSL*Q(3))+GA3L*C3L*DA
     &       + GN*(C3R*DDIF+CSR*Q(3)*DZ)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE IOSXXX(FI,FO,SC,GC , VERTEX)
C
C This subroutine computes an amplitude of the fermion-fermion-scalar   
C coupling.                                                             
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex SC(3)          : input    scalar                      S 
C       complex GC(2)          : coupling constants                 GCHF
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude                     <FO|S|FI>
C
      COMPLEX FI(6),FO(6),SC(3),GC(2),VERTEX
         REAL P0,P1,P2,P3,Q0,Q1,Q2,Q3,R0,R1,R2,R3,PM
C
C .........CHECK........................................................
          P0=-REAL( FI(5))
          P1=-REAL( FI(6))
          P2=-AIMAG(FI(6))
          P3=-AIMAG(FI(5))
          Q0=REAL( FO(5))
          Q1=REAL( FO(6))
          Q2=AIMAG(FO(6))
          Q3=AIMAG(FO(5))
          R0=REAL( SC(2))
          R1=REAL( SC(3))
          R2=AIMAG(SC(3))
          R3=AIMAG(SC(2))
          IF (ABS(FI(1))+ABS(FI(2))+ABS(FI(3))+ABS(FI(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FI(6) in IOSXXX is zero spinor'
          IF (ABS(FI(5))+ABS(FI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FI(6) in IOSXXX has zero momentum'
          IF (ABS(FO(1))+ABS(FO(2))+ABS(FO(3))+ABS(FO(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FO(6) in IOSXXX is zero spinor'
          IF (ABS(FO(5))+ABS(FO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FO(6) in IOSXXX has zero momentum'
          IF (ABS(SC(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : SC(3) in IOSXXX is zero scalar'
          IF (ABS(SC(2))+ABS(SC(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : SC(3) in IOSXXX has zero momentum'
          PM=MAX( ABS(P0),ABS(Q0),ABS(R0),ABS(P1),ABS(Q1),ABS(R1),
     &            ABS(P2),ABS(Q2),ABS(R2),ABS(P3),ABS(Q3),ABS(R3) )
          IF (( ABS(-FI(5)+FO(5)+SC(2))+ABS(-FI(6)+FO(6)+SC(3)))
     &        .GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : FI(6),FO(6),SC(3) in IOSXXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (ABS(GC(1))+ABS(GC(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GC(2) in IOSXXX is zero coupling'
C .........CHECK........................................................
C
      VERTEX = SC(1)*( GC(1)*(FI(1)*FO(1)+FI(2)*FO(2))
     &                +GC(2)*(FI(3)*FO(3)+FI(4)*FO(4)) )
C
      RETURN          
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FSIXXX(FI,SC,GC,FMASS,FWIDTH , FSI)
C
C This subroutine computes an off-shell fermion wavefunction from a     
C flowing-IN external fermion and a vector boson.                       
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex SC(3)          : input    scalar                      S 
C       complex GC(2)          : coupling constants                 GCHF
C       real    FMASS          : mass  of OUTPUT fermion F'             
C       real    FWIDTH         : width of OUTPUT fermion F'             
C                                                                       
C OUTPUT:                                                               
C       complex FSI(6)         : off-shell fermion             |F',S,FI>
C
      COMPLEX FI(6),SC(3),FSI(6),GC(2),SL1,SL2,SR1,SR2,DS
      REAL    PF(0:3),FMASS,FWIDTH,PF2,P0P3,P0M3
C
C .........CHECK........................................................
          IF (ABS(FI(1))+ABS(FI(2))+ABS(FI(3))+ABS(FI(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FI(6) in FSIXXX is zero spinor'
          IF (ABS(FI(5))+ABS(FI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FI(6) in FSIXXX has zero momentum'
          IF (ABS(SC(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : SC(3) in FSIXXX is zero scalar'
          IF (ABS(SC(2))+ABS(SC(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : SC(3) in FSIXXX has zero momentum'
          IF (ABS(GC(1))+ABS(GC(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GC(2) in FSIXXX is zero coupling'
          IF (FMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FMASS in FSIXXX is negative'
          WRITE(1,*) '               FMASS = ',FMASS
          ENDIF
          IF (FWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FWIDTH in FSIXXX is negative'
          WRITE(1,*) '               FWIDTH = ',FWIDTH
          ENDIF
C .........CHECK........................................................
C
      FSI(5) = FI(5)-SC(2)
      FSI(6) = FI(6)-SC(3)
C
      PF(0)=REAL( FSI(5))
      PF(1)=REAL( FSI(6))
      PF(2)=AIMAG(FSI(6))
      PF(3)=AIMAG(FSI(5))
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
C .........CHECK........................................................
          IF (ABS(FSI(5))+ABS(FSI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FSI(6) in FSIXXX has zero momentum'
          IF ((FWIDTH.EQ.0.).AND.(PF2.EQ.FMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : FSI(6) in FSIXXX is on FMASS pole'
          WRITE(1,*) '               P     = ',PF(0),PF(1),PF(2),PF(3)
          WRITE(1,*) '               abs(P)= ',SQRT(ABS(PF2))
          FSI(1)=CMPLX(0.)
          FSI(2)=CMPLX(0.)
          FSI(3)=CMPLX(0.)
          FSI(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DS=-SC(1)/CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      P0P3=PF(0)+PF(3)
      P0M3=PF(0)-PF(3)
      SL1=GC(1)*(P0P3*FI(1)+CONJG(FSI(6))*FI(2))
      SL2=GC(1)*(P0M3*FI(2)      +FSI(6) *FI(1))
      SR1=GC(2)*(P0M3*FI(3)-CONJG(FSI(6))*FI(4))
      SR2=GC(2)*(P0P3*FI(4)      -FSI(6) *FI(3))
C
      FSI(1) = ( GC(1)*FMASS*FI(1) + SR1 )*DS
      FSI(2) = ( GC(1)*FMASS*FI(2) + SR2 )*DS
      FSI(3) = ( GC(2)*FMASS*FI(3) + SL1 )*DS
      FSI(4) = ( GC(2)*FMASS*FI(4) + SL2 )*DS
C
      RETURN          
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FSOXXX(FO,SC,GC,FMASS,FWIDTH , FSO)
C
C This subroutine computes an off-shell fermion wavefunction from a     
C flowing-OUT external fermion and a vector boson.                      
C                                                                       
C INPUT:                                                                
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex SC(6)          : input    scalar                      S 
C       complex GC(2)          : coupling constants                 GCHF
C       real    FMASS          : mass  of OUTPUT fermion F'             
C       real    FWIDTH         : width of OUTPUT fermion F'             
C                                                                       
C OUTPUT:                                                               
C       complex FSO(6)         : off-shell fermion             <FO,S,F'|
C
      COMPLEX FO(6),SC(6),FSO(6),GC(2),SL1,SL2,SR1,SR2,DS
      REAL    PF(0:3),FMASS,FWIDTH,PF2,P0P3,P0M3
C
C .........CHECK........................................................
          IF (ABS(FO(1))+ABS(FO(2))+ABS(FO(3))+ABS(FO(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FO(6) in FSOXXX is zero spinor'
          IF (ABS(FO(5))+ABS(FO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FO(6) in FSOXXX has zero momentum'
          IF (ABS(SC(1))+ABS(SC(2))+ABS(SC(3))+ABS(SC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : SC(6) in FSOXXX is zero scalar'
          IF (ABS(SC(5))+ABS(SC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : SC(6) in FSOXXX has zero momentum'
          IF (ABS(GC(1))+ABS(GC(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GC(2) in FSOXXX is zero coupling'
          IF (FMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FMASS in FSOXXX is negative'
          WRITE(1,*) '               FMASS = ',FMASS
          ENDIF
          IF (FWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FWIDTH in FSOXXX is negative'
          WRITE(1,*) '               FWIDTH = ',FWIDTH
          ENDIF
C .........CHECK........................................................
C
      FSO(5) = FO(5)+SC(2)
      FSO(6) = FO(6)+SC(3)
C
      PF(0)=REAL( FSO(5))
      PF(1)=REAL( FSO(6))
      PF(2)=AIMAG(FSO(6))
      PF(3)=AIMAG(FSO(5))
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
C .........CHECK........................................................
          IF (ABS(FSO(5))+ABS(FSO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FSO(6) in FSOXXX has zero momentum'
          IF ((FWIDTH.EQ.0.).AND.(PF2.EQ.FMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : FSO(6) in FSOXXX is on FMASS pole'
          WRITE(1,*) '               P     = ',PF(0),PF(1),PF(2),PF(3)
          WRITE(1,*) '               abs(P)= ',SQRT(ABS(PF2))
          FSO(1)=CMPLX(0.)
          FSO(2)=CMPLX(0.)
          FSO(3)=CMPLX(0.)
          FSO(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DS=-SC(1)/CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      P0P3=PF(0)+PF(3)
      P0M3=PF(0)-PF(3)
      SL1=GC(2)*(P0P3*FO(3)      +FSO(6) *FO(4))
      SL2=GC(2)*(P0M3*FO(4)+CONJG(FSO(6))*FO(3))
      SR1=GC(1)*(P0M3*FO(1)      -FSO(6) *FO(2))
      SR2=GC(1)*(P0P3*FO(2)-CONJG(FSO(6))*FO(1))
C
      FSO(1) = ( GC(1)*FMASS*FO(1) + SL1 )*DS
      FSO(2) = ( GC(1)*FMASS*FO(2) + SL2 )*DS
      FSO(3) = ( GC(2)*FMASS*FO(3) + SR1 )*DS
      FSO(4) = ( GC(2)*FMASS*FO(4) + SR2 )*DS
C
      RETURN          
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HIOXXX(FI,FO,GC,SMASS,SWIDTH , HIO)
C
C This subroutine computes an off-shell scalar current from an external 
C fermion pair.                                                         
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex GC(2)          : coupling constants                 GCHF
C       real    SMASS          : mass  of OUTPUT scalar S               
C       real    SWIDTH         : width of OUTPUT scalar S               
C                                                                       
C OUTPUT:                                                               
C       complex HIO(3)         : scalar current             J(<FI|S|FO>)
C
      COMPLEX FI(6),FO(6),HIO(3),GC(2),DN
      REAL    Q(0:3),SMASS,SWIDTH,Q2
C
C .........CHECK........................................................
          IF (ABS(FI(1))+ABS(FI(2))+ABS(FI(3))+ABS(FI(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FI(6) in HIOXXX is zero spinor'
          IF (ABS(FI(5))+ABS(FI(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FI(6) in HIOXXX has zero momentum'
          IF (ABS(FO(1))+ABS(FO(2))+ABS(FO(3))+ABS(FO(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : FO(6) in HIOXXX is zero spinor'
          IF (ABS(FO(5))+ABS(FO(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : FO(6) in HIOXXX has zero momentum'
          IF (ABS(GC(1))+ABS(GC(2)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GC(2) in HIOXXX is zero coupling'
          IF (SMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SMASS in HIOXXX is negative'
          WRITE(1,*) '               SMASS = ',SMASS
          ENDIF
          IF (SWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SWIDTH in HIOXXX is negative'
          WRITE(1,*) '               SWIDTH = ',SWIDTH
          ENDIF
C .........CHECK........................................................
C
      HIO(2) = FO(5)-FI(5)
      HIO(3) = FO(6)-FI(6)
C
      Q(0)=REAL( HIO(2))
      Q(1)=REAL( HIO(3))
      Q(2)=AIMAG(HIO(3))
      Q(3)=AIMAG(HIO(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
C .........CHECK........................................................
          IF (ABS(HIO(2))+ABS(HIO(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : HIO(3) in HIOXXX has zero momentum'
          IF ((SWIDTH.EQ.0.).AND.(Q2.EQ.SMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : HIO(3) in HIOXXX is on SMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          HIO(1)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DN=-CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HIO(1) = ( GC(1)*(FO(1)*FI(1)+FO(2)*FI(2))
     &          +GC(2)*(FO(3)*FI(3)+FO(4)*FI(4)) )/DN
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VVVXXX(WM,WP,W3,G , VERTEX)
C
C This subroutine computes an amplitude of the three-point coupling of  
C the gauge bosons.                                                     
C                                                                       
C INPUT:                                                                
C       complex WM(6)          : vector               flow-OUT W-       
C       complex WP(6)          : vector               flow-OUT W+       
C       complex W3(6)          : vector               J3 or A    or Z   
C       real    G              : coupling constant    GW or GWWA or GWWZ
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude               Gamma(WM,WP,W3)
C
      COMPLEX WM(6),WP(6),W3(6),VERTEX, 
     &        XV1,XV2,XV3,V12,V23,V31,P12,P13,P21,P23,P31,P32
      REAL    PWM(0:3),PWP(0:3),PW3(0:3),G
         REAL PM
C
      PWM(0)=REAL( WM(5))
      PWM(1)=REAL( WM(6))
      PWM(2)=AIMAG(WM(6))
      PWM(3)=AIMAG(WM(5))
      PWP(0)=REAL( WP(5))
      PWP(1)=REAL( WP(6))
      PWP(2)=AIMAG(WP(6))
      PWP(3)=AIMAG(WP(5))
      PW3(0)=REAL( W3(5))
      PW3(1)=REAL( W3(6))
      PW3(2)=AIMAG(W3(6))
      PW3(3)=AIMAG(W3(5))
C
C .........CHECK........................................................
          IF ( ABS(WM(1))+ABS(WM(2))
     &        +ABS(WM(3))+ABS(WM(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WM(6) in VVVXXX is zero vector'
          IF (ABS(WM(5))+ABS(WM(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WM(6) in VVVXXX has zero momentum'
          IF ( ABS(WP(1))+ABS(WP(2))
     &        +ABS(WP(3))+ABS(WP(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WP(6) in VVVXXX is zero vector'
          IF (ABS(WP(5))+ABS(WP(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WP(6) in VVVXXX has zero momentum'
          IF ( ABS(W3(1))+ABS(W3(2))
     &        +ABS(W3(3))+ABS(W3(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W3(6) in VVVXXX is zero vector'
          IF (ABS(W3(5))+ABS(W3(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W3(6) in VVVXXX has zero momentum'
          PM=MAX( ABS(PWM(0)),ABS(PWP(0)),ABS(PW3(0)),
     &            ABS(PWM(1)),ABS(PWP(1)),ABS(PW3(1)),
     &            ABS(PWM(2)),ABS(PWP(2)),ABS(PW3(2)),
     &            ABS(PWM(3)),ABS(PWP(3)),ABS(PW3(3)) )
          IF (( ABS(WM(5)+WP(5)+W3(5))+ABS(WM(6)+WP(6)+W3(6)))
     &        .GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : WM(6),WP(6),W3(6) in VVVXXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in VVVXXX is zero coupling'
C .........CHECK........................................................
C
      V12=WM(1)*WP(1)-WM(2)*WP(2)-WM(3)*WP(3)-WM(4)*WP(4)
      V23=WP(1)*W3(1)-WP(2)*W3(2)-WP(3)*W3(3)-WP(4)*W3(4)
      V31=W3(1)*WM(1)-W3(2)*WM(2)-W3(3)*WM(3)-W3(4)*WM(4)
      XV1=0.
      XV2=0.
      XV3=0.
      IF (ABS(WM(1)).NE.0.) THEN
      IF (ABS(WM(1)).GE.MAX(ABS(WM(2)),ABS(WM(3)),ABS(WM(4)))*1.E-1)
     &      XV1=PWM(0)/WM(1)
      ENDIF
      IF (ABS(WP(1)).NE.0.) THEN
      IF (ABS(WP(1)).GE.MAX(ABS(WP(2)),ABS(WP(3)),ABS(WP(4)))*1.E-1)
     &      XV2=PWP(0)/WP(1)
      ENDIF
      IF (ABS(W3(1)).NE.0.) THEN
      IF (ABS(W3(1)).GE.MAX(ABS(W3(2)),ABS(W3(3)),ABS(W3(4)))*1.E-1)
     &      XV3=PW3(0)/W3(1)
      ENDIF
      P12= (PWM(0)-XV1*WM(1))*WP(1)-(PWM(1)-XV1*WM(2))*WP(2)
     &    -(PWM(2)-XV1*WM(3))*WP(3)-(PWM(3)-XV1*WM(4))*WP(4)
      P13= (PWM(0)-XV1*WM(1))*W3(1)-(PWM(1)-XV1*WM(2))*W3(2)
     &    -(PWM(2)-XV1*WM(3))*W3(3)-(PWM(3)-XV1*WM(4))*W3(4)
      P21= (PWP(0)-XV2*WP(1))*WM(1)-(PWP(1)-XV2*WP(2))*WM(2)
     &    -(PWP(2)-XV2*WP(3))*WM(3)-(PWP(3)-XV2*WP(4))*WM(4)
      P23= (PWP(0)-XV2*WP(1))*W3(1)-(PWP(1)-XV2*WP(2))*W3(2)
     &    -(PWP(2)-XV2*WP(3))*W3(3)-(PWP(3)-XV2*WP(4))*W3(4)
      P31= (PW3(0)-XV3*W3(1))*WM(1)-(PW3(1)-XV3*W3(2))*WM(2)
     &    -(PW3(2)-XV3*W3(3))*WM(3)-(PW3(3)-XV3*W3(4))*WM(4)
      P32= (PW3(0)-XV3*W3(1))*WP(1)-(PW3(1)-XV3*W3(2))*WP(2)
     &    -(PW3(2)-XV3*W3(3))*WP(3)-(PW3(3)-XV3*W3(4))*WP(4)
C
      VERTEX = -(V12*(P13-P23)+V23*(P21-P31)+V31*(P32-P12))*G
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JVVXXX(V1,V2,G,VMASS,VWIDTH , JVV)
C
C This subroutine computes an off-shell vector current from the three-  
C point gauge boson coupling.  The vector propagator is given in Feynman
C gauge for a massless vector and in unitary gauge for a massive vector.
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       real    G              : coupling constant (see the table below)
C       real    VMASS          : mass  of OUTPUT vector V               
C       real    VWIDTH         : width of OUTPUT vector V               
C                                                                       
C The possible sets of the inputs are as follows:                       
C    ------------------------------------------------------------------ 
C    |   V1   |   V2   |  JVV   |      G       |   VMASS  |  VWIDTH   | 
C    ------------------------------------------------------------------ 
C    |   W-   |   W+   |  A/Z   |  GWWA/GWWZ   | 0./ZMASS | 0./ZWIDTH | 
C    | W3/A/Z |   W-   |  W+    | GW/GWWA/GWWZ |   WMASS  |  WWIDTH   | 
C    |   W+   | W3/A/Z |  W-    | GW/GWWA/GWWZ |   WMASS  |  WWIDTH   | 
C    ------------------------------------------------------------------ 
C where all the bosons are defined by the flowing-OUT quantum number.   
C                                                                       
C OUTPUT:                                                               
C       complex JVV(6)         : vector current            J^mu(V:V1,V2)
C
      COMPLEX V1(6),V2(6),JVV(6),J12(0:3),JS,DG,
     &        SV1,SV2,S11,S12,S21,S22,V12
      REAL    P1(0:3),P2(0:3),Q(0:3),G,VMASS,VWIDTH,GS,S,VM2,M1,M2
C
C .........CHECK........................................................
          IF (ABS(V1(1))+ABS(V1(2))+ABS(V1(3))+ABS(V1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V1(6) in JVVXXX is zero vector'
          IF (ABS(V1(5))+ABS(V1(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V1(6) in JVVXXX has zero momentum'
          IF (ABS(V2(1))+ABS(V2(2))+ABS(V2(3))+ABS(V2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V2(6) in JVVXXX is zero vector'
          IF (ABS(V2(5))+ABS(V2(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V2(6) in JVVXXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in JVVXXX is zero coupling'
          IF (VMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VMASS in JVVXXX is negative'
          WRITE(1,*) '               VMASS = ',VMASS
          ENDIF
          IF (VWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VWIDTH in JVVXXX is negative'
          WRITE(1,*) '               VWIDTH = ',VWIDTH
          ENDIF
C .........CHECK........................................................
C
      JVV(5) = V1(5)+V2(5)
      JVV(6) = V1(6)+V2(6)
C
      P1(0)=REAL( V1(5))
      P1(1)=REAL( V1(6))
      P1(2)=AIMAG(V1(6))
      P1(3)=AIMAG(V1(5))
      P2(0)=REAL( V2(5))
      P2(1)=REAL( V2(6))
      P2(2)=AIMAG(V2(6))
      P2(3)=AIMAG(V2(5))
      Q(0)=-REAL( JVV(5))
      Q(1)=-REAL( JVV(6))
      Q(2)=-AIMAG(JVV(6))
      Q(3)=-AIMAG(JVV(5))
      S=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
C .........CHECK........................................................
          IF (ABS(JVV(5))+ABS(JVV(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : JVV(6) in JVVXXX has zero momentum'
          IF ((VWIDTH.EQ.0.).AND.(S.EQ.VM2)) THEN
          WRITE(1,*) ' HELAS-ERROR : JVV(6) in JVVXXX is on VMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(S))
          JVV(1)=CMPLX(0.)
          JVV(2)=CMPLX(0.)
          JVV(3)=CMPLX(0.)
          JVV(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      V12=V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4)
      SV1= (P2(0)-Q(0))*V1(1) -(P2(1)-Q(1))*V1(2)
     &    -(P2(2)-Q(2))*V1(3) -(P2(3)-Q(3))*V1(4)
      SV2=-(P1(0)-Q(0))*V2(1) +(P1(1)-Q(1))*V2(2)
     &    +(P1(2)-Q(2))*V2(3) +(P1(3)-Q(3))*V2(4)
      J12(0)=(P1(0)-P2(0))*V12 +SV1*V2(1) +SV2*V1(1)
      J12(1)=(P1(1)-P2(1))*V12 +SV1*V2(2) +SV2*V1(2)
      J12(2)=(P1(2)-P2(2))*V12 +SV1*V2(3) +SV2*V1(3)
      J12(3)=(P1(3)-P2(3))*V12 +SV1*V2(4) +SV2*V1(4)
C
      IF (VMASS.EQ.0.) GOTO 10
C
      M1=P1(0)**2-(P1(1)**2+P1(2)**2+P1(3)**2)
      M2=P2(0)**2-(P2(1)**2+P2(2)**2+P2(3)**2)
      S11=P1(0)*V1(1)-P1(1)*V1(2)-P1(2)*V1(3)-P1(3)*V1(4)
      S12=P1(0)*V2(1)-P1(1)*V2(2)-P1(2)*V2(3)-P1(3)*V2(4)
      S21=P2(0)*V1(1)-P2(1)*V1(2)-P2(2)*V1(3)-P2(3)*V1(4)
      S22=P2(0)*V2(1)-P2(1)*V2(2)-P2(2)*V2(3)-P2(3)*V2(4)
      JS=(V12*(-M1+M2) +S11*S12 -S21*S22)/VM2
C
      DG=-G/CMPLX( S-VM2 , MAX(SIGN( VMASS*VWIDTH ,S),0.) )
C  For the running width, use below instead of the above DG.
C      DG=-G/CMPLX( S-VM2 , MAX( VWIDTH*S/VMASS ,0.) )
C
      JVV(1) = DG*(J12(0)-Q(0)*JS)
      JVV(2) = DG*(J12(1)-Q(1)*JS)
      JVV(3) = DG*(J12(2)-Q(2)*JS)
      JVV(4) = DG*(J12(3)-Q(3)*JS)
C
      RETURN
C
  10  GS=-G/S
C
      JVV(1) = GS*J12(0)
      JVV(2) = GS*J12(1)
      JVV(3) = GS*J12(2)
      JVV(4) = GS*J12(3)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VVSXXX(V1,V2,SC,G , VERTEX)
C
C This subroutine computes an amplitude of the vector-vector-scalar     
C coupling.                                                             
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex SC(3)          : input  scalar                        S 
C       real    G              : coupling constant                  GVVH
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude                Gamma(V1,V2,S)
C
      COMPLEX V1(6),V2(6),SC(3),VERTEX
      REAL    G
         REAL P10,P11,P12,P13,P20,P21,P22,P23,Q0,Q1,Q2,Q3,PM
C
C .........CHECK........................................................
          P10=REAL( V1(5))
          P11=REAL( V1(6))
          P12=AIMAG(V1(6))
          P13=AIMAG(V1(5))
          P20=REAL( V2(5))
          P21=REAL( V2(6))
          P22=AIMAG(V2(6))
          P23=AIMAG(V2(5))
          Q0 =REAL( SC(2))
          Q1 =REAL( SC(3))
          Q2 =AIMAG(SC(3))
          Q3 =AIMAG(SC(2))
          IF (ABS(V1(1))+ABS(V1(2))+ABS(V1(3))+ABS(V1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V1(6) in VVSXXX is zero vector'
          IF (ABS(V1(5))+ABS(V1(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V1(6) in VVSXXX has zero momentum'
          IF (ABS(V2(1))+ABS(V2(2))+ABS(V2(3))+ABS(V2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V2(6) in VVSXXX is zero vector'
          IF (ABS(V2(5))+ABS(V2(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V2(6) in VVSXXX has zero momentum'
          IF (ABS(SC(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : SC(3) in VVSXXX is zero scalar'
          IF (ABS(SC(2))+ABS(SC(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : SC(3) in VVSXXX has zero momentum'
          PM=MAX( ABS(P10),ABS(P20),ABS(Q0),ABS(P11),ABS(P21),ABS(Q1),
     &            ABS(P12),ABS(P22),ABS(Q2),ABS(P13),ABS(P23),ABS(Q3) )
          IF (( ABS(V1(5)+V2(5)+SC(2))+ABS(V1(6)+V2(6)+SC(3)))
     &        .GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : V1(6),V2(6),SC(3) in VVSXXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in VVSXXX is zero coupling'
C .........CHECK........................................................
C
      VERTEX = G*SC(1)*(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JVSXXX(VC,SC,G,VMASS,VWIDTH , JVS)
C
C This subroutine computes an off-shell vector current from the vector- 
C vector-scalar coupling.  The vector propagator is given in Feynman    
C gauge for a massless vector and in unitary gauge for a massive vector.
C                                                                       
C INPUT:                                                                
C       complex VC(6)          : input vector                          V
C       complex SC(3)          : input scalar                          S
C       real    G              : coupling constant                  GVVH
C       real    VMASS          : mass  of OUTPUT vector V'              
C       real    VWIDTH         : width of OUTPUT vector V'              
C                                                                       
C OUTPUT:                                                               
C       complex JVS(6)         : vector current             J^mu(V':V,S)
C
      COMPLEX VC(6),SC(3),JVS(6),DG,VK
      REAL    Q(0:3),G,VMASS,VWIDTH,Q2,VM2
C
C .........CHECK........................................................
          IF (ABS(VC(1))+ABS(VC(2))+ABS(VC(3))+ABS(VC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : VC(6) in JVSXXX is zero vector'
          IF (ABS(VC(5))+ABS(VC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : VC(6) in JVSXXX has zero momentum'
          IF (ABS(SC(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : SC(3) in JVSXXX is zero scalar'
          IF (ABS(SC(2))+ABS(SC(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : SC(3) in JVSXXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in JVSXXX is zero coupling'
          IF (VMASS.LE.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VMASS in JVSXXX is not positive'
          WRITE(1,*) '               VMASS = ',VMASS
          ENDIF
          IF (VWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VWIDTH in JVSXXX is negative'
          WRITE(1,*) '               VWIDTH = ',VWIDTH
          ENDIF
C .........CHECK........................................................
C
      JVS(5) = VC(5)+SC(2)
      JVS(6) = VC(6)+SC(3)
C
      Q(0)=REAL( JVS(5))
      Q(1)=REAL( JVS(6))
      Q(2)=AIMAG(JVS(6))
      Q(3)=AIMAG(JVS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
C .........CHECK........................................................
          IF (ABS(JVS(5))+ABS(JVS(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : JVS(6) in JVSXXX has zero momentum'
          IF ((VWIDTH.EQ.0.).AND.(Q2.EQ.VM2)) THEN
          WRITE(1,*) ' HELAS-ERROR : JVS(6) in JVSXXX is on VMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          JVS(1)=CMPLX(0.)
          JVS(2)=CMPLX(0.)
          JVS(3)=CMPLX(0.)
          JVS(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G*SC(1)/CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above DG.
C      DG=G*SC(1)/CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      VK=(-Q(0)*VC(1)+Q(1)*VC(2)+Q(2)*VC(3)+Q(3)*VC(4))/VM2
C
      JVS(1) = DG*(Q(0)*VK+VC(1))
      JVS(2) = DG*(Q(1)*VK+VC(2))
      JVS(3) = DG*(Q(2)*VK+VC(3))
      JVS(4) = DG*(Q(3)*VK+VC(4))
C
      RETURN
C
  10  DG=G*SC(1)/Q2
C
      JVS(1) = DG*VC(1)
      JVS(2) = DG*VC(2)
      JVS(3) = DG*VC(3)
      JVS(4) = DG*VC(4)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HVVXXX(V1,V2,G,SMASS,SWIDTH , HVV)
C
C This subroutine computes an off-shell scalar current from the vector- 
C vector-scalar coupling.                                               
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       real    G              : coupling constant                  GVVH
C       real    SMASS          : mass  of OUTPUT scalar S               
C       real    SWIDTH         : width of OUTPUT scalar S               
C                                                                       
C OUTPUT:                                                               
C       complex HVV(3)         : off-shell scalar current     J(S:V1,V2)
C
      COMPLEX V1(6),V2(6),HVV(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
C .........CHECK........................................................
          IF (ABS(V1(1))+ABS(V1(2))+ABS(V1(3))+ABS(V1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V1(6) in HVVXXX is zero vector'
          IF (ABS(V1(5))+ABS(V1(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V1(6) in HVVXXX has zero momentum'
          IF (ABS(V2(1))+ABS(V2(2))+ABS(V2(3))+ABS(V2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V2(6) in HVVXXX is zero vector'
          IF (ABS(V2(5))+ABS(V2(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V2(6) in HVVXXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in HVVXXX is zero coupling'
          IF (SMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SMASS in HVVXXX is negative'
          WRITE(1,*) '               SMASS = ',SMASS
          ENDIF
          IF (SWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SWIDTH in HVVXXX is negative'
          WRITE(1,*) '               SWIDTH = ',SWIDTH
          ENDIF
C .........CHECK........................................................
C
      HVV(2) = V1(5)+V2(5)
      HVV(3) = V1(6)+V2(6)
C
      Q(0)=REAL( HVV(2))
      Q(1)=REAL( HVV(3))
      Q(2)=AIMAG(HVV(3))
      Q(3)=AIMAG(HVV(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
C .........CHECK........................................................
          IF (ABS(HVV(2))+ABS(HVV(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : HVV(3) in HVVXXX has zero momentum'
          IF ((SWIDTH.EQ.0.).AND.(Q2.EQ.SMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : HVV(3) in HVVXXX is on SMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          HVV(1)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HVV(1) = DG*(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VSSXXX(VC,S1,S2,G , VERTEX)
C
C This subroutine computes an amplitude from the vector-scalar-scalar   
C coupling.  The coupling is absent in the minimal SM in unitary gauge. 
C                                                                       
C       complex VC(6)          : input  vector                        V 
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant (S1 charge)          
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
C where the S1 charge is defined by the flowing-OUT quantum number.     
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude                Gamma(V,S1,S2)
C
      COMPLEX VC(6),S1(3),S2(3),VERTEX
      REAL    P(0:3),G
         REAL P0,P1,P2,P3,Q0,Q1,Q2,Q3,R0,R1,R2,R3,PM
C
C .........CHECK........................................................
          P0=REAL( S1(2))
          P1=REAL( S1(3))
          P2=AIMAG(S1(3))
          P3=AIMAG(S1(2))
          Q0=REAL( S2(2))
          Q1=REAL( S2(3))
          Q2=AIMAG(S2(3))
          Q3=AIMAG(S2(2))
          R0=REAL( VC(5))
          R1=REAL( VC(6))
          R2=AIMAG(VC(6))
          R3=AIMAG(VC(5))
          IF (ABS(VC(1))+ABS(VC(2))+ABS(VC(3))+ABS(VC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : VC(6) in VSSXXX is zero vector'
          IF (ABS(VC(5))+ABS(VC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : VC(6) in VSSXXX has zero momentum'
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in VSSXXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in VSSXXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in VSSXXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in VSSXXX has zero momentum'
          PM=MAX( ABS(P0),ABS(Q0),ABS(R0),ABS(P1),ABS(Q1),ABS(R1),
     &            ABS(P2),ABS(Q2),ABS(R2),ABS(P3),ABS(Q3),ABS(R3) )
          IF (( ABS(VC(5)+S1(2)+S2(2))+ABS(VC(6)+S1(3)+S2(3)))
     &        .GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : VC(6),S1(3),S2(3) in VSSXXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in VSSXXX is zero coupling'
C .........CHECK........................................................
C
      P(0)=REAL( S1(2)-S2(2))
      P(1)=REAL( S1(3)-S2(3))
      P(2)=AIMAG(S1(3)-S2(3))
      P(3)=AIMAG(S1(2)-S2(2))
C
      VERTEX = G*S1(1)*S2(1)
     &        *(VC(1)*P(0)-VC(2)*P(1)-VC(3)*P(2)-VC(4)*P(3))
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JSSXXX(S1,S2,G,VMASS,VWIDTH , JSS)
C
C This subroutine computes an off-shell vector current from the vector- 
C scalar-scalar coupling.  The coupling is absent in the minimal SM in  
C unitary gauge.  The propagator is given in Feynman gauge for a        
C massless vector and in unitary gauge for a massive vector.            
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant (S1 charge)          
C       real    VMASS          : mass  of OUTPUT vector V               
C       real    VWIDTH         : width of OUTPUT vector V               
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
C where the S1 charge is defined by the flowing-OUT quantum number.     
C                                                                       
C OUTPUT:                                                               
C       complex JSS(6)         : vector current            J^mu(V:S1,S2)
C
      COMPLEX S1(3),S2(3),JSS(6),DG,ADG
      REAL    PP(0:3),PA(0:3),Q(0:3),G,VMASS,VWIDTH,Q2,VM2,MP2,MA2,M2D
C
C .........CHECK........................................................
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in JSSXXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in JSSXXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in JSSXXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in JSSXXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in JSSXXX is zero coupling'
          IF (VMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VMASS in JSSXXX is negative'
          WRITE(1,*) '               VMASS = ',VMASS
          ENDIF
          IF (VWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VWIDTH in JSSXXX is negative'
          WRITE(1,*) '               VWIDTH = ',VWIDTH
          ENDIF
C .........CHECK........................................................
C
      JSS(5) = S1(2)+S2(2)
      JSS(6) = S1(3)+S2(3)
C
      Q(0)=REAL( JSS(5))
      Q(1)=REAL( JSS(6))
      Q(2)=AIMAG(JSS(6))
      Q(3)=AIMAG(JSS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
C .........CHECK........................................................
          IF (ABS(JSS(5))+ABS(JSS(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : JSS(6) in JSSXXX has zero momentum'
          IF ((VWIDTH.EQ.0.).AND.(Q2.EQ.VM2)) THEN
          WRITE(1,*) ' HELAS-ERROR : JSS(6) in JSSXXX is on VMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          JSS(1)=CMPLX(0.)
          JSS(2)=CMPLX(0.)
          JSS(3)=CMPLX(0.)
          JSS(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G/CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above DG.
C      DG=G/CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      ADG=DG*S1(1)*S2(1)
C
      PP(0)=REAL( S1(2))
      PP(1)=REAL( S1(3))
      PP(2)=AIMAG(S1(3))
      PP(3)=AIMAG(S1(2))
      PA(0)=REAL( S2(2))
      PA(1)=REAL( S2(3))
      PA(2)=AIMAG(S2(3))
      PA(3)=AIMAG(S2(2))
      MP2=PP(0)**2-(PP(1)**2+PP(2)**2+PP(3)**2)
      MA2=PA(0)**2-(PA(1)**2+PA(2)**2+PA(3)**2)
      M2D=MP2-MA2
C
      JSS(1) = ADG*( (PP(0)-PA(0)) - Q(0)*M2D/VM2)
      JSS(2) = ADG*( (PP(1)-PA(1)) - Q(1)*M2D/VM2)
      JSS(3) = ADG*( (PP(2)-PA(2)) - Q(2)*M2D/VM2)
      JSS(4) = ADG*( (PP(3)-PA(3)) - Q(3)*M2D/VM2)
C
      RETURN
C
  10  ADG=G*S1(1)*S2(1)/Q2
C
      JSS(1) = ADG*REAL( S1(2)-S2(2))
      JSS(2) = ADG*REAL( S1(3)-S2(3))
      JSS(3) = ADG*AIMAG(S1(3)-S2(3))
      JSS(4) = ADG*AIMAG(S1(2)-S2(2))
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
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
      COMPLEX VC(6),SC(3),HVS(3),DG,QVV,QPV
      REAL    QV(0:3),QP(0:3),QA(0:3),G,SMASS,SWIDTH,Q2
C
C .........CHECK........................................................
          IF (ABS(VC(1))+ABS(VC(2))+ABS(VC(3))+ABS(VC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : VC(6) in HVSXXX is zero vector'
          IF (ABS(VC(5))+ABS(VC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : VC(6) in HVSXXX has zero momentum'
          IF (ABS(SC(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : SC(3) in HVSXXX is zero scalar'
          IF (ABS(SC(2))+ABS(SC(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : SC(3) in HVSXXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in HVSXXX is zero coupling'
          IF (SMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SMASS in HVSXXX is negative'
          WRITE(1,*) '               SMASS = ',SMASS
          ENDIF
          IF (SWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SWIDTH in HVSXXX is negative'
          WRITE(1,*) '               SWIDTH = ',SWIDTH
          ENDIF
C .........CHECK........................................................
C
      HVS(2) = VC(5)+SC(2)
      HVS(3) = VC(6)+SC(3)
C
      QV(0)=REAL(  VC(2))
      QV(1)=REAL(  VC(3))
      QV(2)=AIMAG( VC(3))
      QV(3)=AIMAG( VC(2))
      QP(0)=REAL(  SC(2))
      QP(1)=REAL(  SC(3))
      QP(2)=AIMAG( SC(3))
      QP(3)=AIMAG( SC(2))
      QA(0)=REAL( HVS(2))
      QA(1)=REAL( HVS(3))
      QA(2)=AIMAG(HVS(3))
      QA(3)=AIMAG(HVS(2))
      Q2=QA(0)**2-(QA(1)**2+QA(2)**2+QA(3)**2)
C
C .........CHECK........................................................
          IF (ABS(HVS(2))+ABS(HVS(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : HVS(3) in HVSXXX has zero momentum'
          IF ((SWIDTH.EQ.0.).AND.(Q2.EQ.SMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : HVS(3) in HVSXXX is on SMASS pole'
          WRITE(1,*) '               Q     = ',QA(0),QA(1),QA(2),QA(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          HVS(1)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
      QVV=QV(0)*VC(1)-QV(1)*VC(2)-QV(2)*VC(3)-QV(3)*VC(4)
      QPV=QP(0)*VC(1)-QP(1)*VC(2)-QP(2)*VC(3)-QP(3)*VC(4)
C
      HVS(1) = DG*(2.*QPV+QVV)*SC(1)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE SSSXXX(S1,S2,S3,G , VERTEX)
C
C This subroutine computes an amplitude of the three-scalar coupling.   
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       real    G              : coupling constant                  GHHH
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude               Gamma(S1,S2,S3)
C
      COMPLEX S1(3),S2(3),S3(3),VERTEX
      REAL    G
         REAL P0,P1,P2,P3,Q0,Q1,Q2,Q3,R0,R1,R2,R3,PM
C
C .........CHECK........................................................
          P0=REAL( S1(2))
          P1=REAL( S1(3))
          P2=AIMAG(S1(3))
          P3=AIMAG(S1(2))
          Q0=REAL( S2(2))
          Q1=REAL( S2(3))
          Q2=AIMAG(S2(3))
          Q3=AIMAG(S2(2))
          R0=REAL( S3(2))
          R1=REAL( S3(3))
          R2=AIMAG(S3(3))
          R3=AIMAG(S3(2))
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in SSSXXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in SSSXXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in SSSXXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in SSSXXX has zero momentum'
          IF (ABS(S3(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S3(3) in SSSXXX is zero scalar'
          IF (ABS(S3(2))+ABS(S3(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S3(3) in SSSXXX has zero momentum'
          PM=MAX( ABS(P0),ABS(Q0),ABS(R0),ABS(P1),ABS(Q1),ABS(R1),
     &            ABS(P2),ABS(Q2),ABS(R2),ABS(P3),ABS(Q3),ABS(R3) )
          IF (( ABS(S1(2)+S2(2)+S3(2))+ABS(S1(3)+S2(3)+S3(3)))
     &        .GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : S1(3),S2(3),S3(3) in SSSXXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in SSSXXX is zero coupling'
C .........CHECK........................................................
C
      VERTEX = G*S1(1)*S2(1)*S3(1)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HSSXXX(S1,S2,G,SMASS,SWIDTH , HSS)
C
C This subroutine computes an off-shell scalar current from the three-  
C scalar coupling.                                                      
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                  GHHH
C       real    SMASS          : mass  of OUTPUT scalar S'              
C       real    SWIDTH         : width of OUTPUT scalar S'              
C                                                                       
C OUTPUT:                                                               
C       complex HSS(3)         : scalar current              J(S':S1,S2)
C
      COMPLEX S1(3),S2(3),HSS(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
C .........CHECK........................................................
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in HSSXXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in HSSXXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in HSSXXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in HSSXXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in HSSXXX is zero coupling'
          IF (SMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SMASS in HSSXXX is negative'
          WRITE(1,*) '               SMASS = ',SMASS
          ENDIF
          IF (SWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SWIDTH in HSSXXX is negative'
          WRITE(1,*) '               SWIDTH = ',SWIDTH
          ENDIF
C .........CHECK........................................................
C
      HSS(2) = S1(2)+S2(2)
      HSS(3) = S1(3)+S2(3)
C
      Q(0)=REAL( HSS(2))
      Q(1)=REAL( HSS(3))
      Q(2)=AIMAG(HSS(3))
      Q(3)=AIMAG(HSS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
C .........CHECK........................................................
          IF (ABS(HSS(2))+ABS(HSS(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : HSS(3) in HSSXXX has zero momentum'
          IF ((SWIDTH.EQ.0.).AND.(Q2.EQ.SMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : HSS(3) in HSSXXX is on SMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          HSS(1)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HSS(1) = DG*S1(1)*S2(1)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE WWWWXX(WM1,WP1,WM2,WP2,GWWA,GWWZ,ZMASS,ZWIDTH , VERTEX)
C
C This subroutine computes an amplitude of the four-point W-/W+         
C coupling, including the contributions of photon and Z exchanges.  The 
C photon propagator is given in Feynman gauge and the Z propagator is   
C given in unitary gauge.                                               
C                                                                       
C INPUT:                                                                
C       complex WM1(0:3)       : first  flow-OUT W-                  WM1
C       complex WP1(0:3)       : first  flow-OUT W+                  WP1
C       complex WM2(0:3)       : second flow-OUT W-                  WM2
C       complex WP2(0:3)       : second flow-OUT W+                  WP2
C       real    GWWA           : coupling constant of W and A       GWWA
C       real    GWWZ           : coupling constant of W and Z       GWWZ
C       real    ZMASS          : mass  of Z                             
C       real    ZWIDTH         : width of Z                             
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude        Gamma(WM1,WP1,WM2,WP2)
C
      COMPLEX    WM1(6),WP1(6),WM2(6),WP2(6),VERTEX
      COMPLEX*16 DV1(0:3),DV2(0:3),DV3(0:3),DV4(0:3),
     &           J12(0:3),J34(0:3),J14(0:3),J32(0:3),DVERTX,
     &           SV1,SV2,SV3,SV4,TV1,TV2,TV3,TV4,DZS,DZT,
     &           V12,V13,V14,V23,V24,V34,JS12,JS34,JS14,JS32,JS,JT
      REAL       PWM1(0:3),PWP1(0:3),PWM2(0:3),PWP2(0:3),
     &           GWWA,GWWZ,ZMASS,ZWIDTH
      REAL*8     Q(0:3),K(0:3),DP1(0:3),DP2(0:3),DP3(0:3),DP4(0:3),
     &           DGWWA2,DGWWZ2,DGW2,DMZ,DWIDTH,S,T,DAS,DAT
            REAL PM
C
      PWM1(0)=REAL( WM1(5))
      PWM1(1)=REAL( WM1(6))
      PWM1(2)=AIMAG(WM1(6))
      PWM1(3)=AIMAG(WM1(5))
      PWP1(0)=REAL( WP1(5))
      PWP1(1)=REAL( WP1(6))
      PWP1(2)=AIMAG(WP1(6))
      PWP1(3)=AIMAG(WP1(5))
      PWM2(0)=REAL( WM2(5))
      PWM2(1)=REAL( WM2(6))
      PWM2(2)=AIMAG(WM2(6))
      PWM2(3)=AIMAG(WM2(5))
      PWP2(0)=REAL( WP2(5))
      PWP2(1)=REAL( WP2(6))
      PWP2(2)=AIMAG(WP2(6))
      PWP2(3)=AIMAG(WP2(5))
C
C .........CHECK........................................................
          IF ( ABS(WM1(1))+ABS(WM1(2))
     &        +ABS(WM1(3))+ABS(WM1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WM1(6) in WWWWXX is zero vector'
          IF (ABS(WM1(5))+ABS(WM1(5)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WM1(6) in WWWWXX has zero momentum'
          IF ( ABS(WP1(1))+ABS(WP1(2))
     &        +ABS(WP1(3))+ABS(WP1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WP1(6) in WWWWXX is zero vector'
          IF (ABS(WP1(5))+ABS(WP1(5)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WP1(6) in WWWWXX has zero momentum'
          IF ( ABS(WM2(1))+ABS(WM2(2))
     &        +ABS(WM2(3))+ABS(WM2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WM2(6) in WWWWXX is zero vector'
          IF (ABS(WM2(5))+ABS(WM2(5)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WM2(6) in WWWWXX has zero momentum'
          IF ( ABS(WP2(1))+ABS(WP2(2))
     &        +ABS(WP2(3))+ABS(WP2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WP2(6) in WWWWXX is zero vector'
          IF (ABS(WP2(5))+ABS(WP2(5)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WP2(6) in WWWWXX has zero momentum'
          PM=MAX( ABS(PWM1(0)),ABS(PWP1(0)),ABS(PWM2(0)),ABS(PWP2(0)),
     &            ABS(PWM1(1)),ABS(PWP1(1)),ABS(PWM2(1)),ABS(PWP2(1)),
     &            ABS(PWM1(2)),ABS(PWP1(2)),ABS(PWM2(2)),ABS(PWP2(2)),
     &            ABS(PWM1(3)),ABS(PWP1(3)),ABS(PWM2(3)),ABS(PWP2(3)) )
          IF (( ABS(WM1(5)+WP1(5)+WM2(5)+WP2(5))
     &         +ABS(WM1(6)+WP1(6)+WM2(6)+WP2(6))).GE.PM*4.E-5) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : WM1(6),WP1(6),WM2(6),WP2(6) in WWWWXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (GWWA.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GWWA in WWWWXX is zero coupling'
          IF (GWWZ.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GWWZ in WWWWXX is zero coupling'
          IF ((GWWA.LT.0.).OR.(GWWA.GE.GWWZ)) THEN
          WRITE(1,*)
     &  ' HELAS-warn  : GWWA/GWWZ in WWWWXX are non-standard couplings'
          WRITE(1,*) '               GWWA = ',GWWA,'  GWWZ = ',GWWZ
          ENDIF
          IF (ZMASS.LE.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : ZMASS in WWWWXX is not positive'
          WRITE(1,*) '               ZMASS = ',ZMASS
          ENDIF
          IF (ZWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : ZWIDTH in WWWWXX is negative'
          WRITE(1,*) '               ZWIDTH = ',ZWIDTH
          ENDIF
C .........CHECK........................................................
C
      DV1(0)=DCMPLX(WM1(1))
      DV1(1)=DCMPLX(WM1(2))
      DV1(2)=DCMPLX(WM1(3))
      DV1(3)=DCMPLX(WM1(4))
      DP1(0)=DBLE(PWM1(0))
      DP1(1)=DBLE(PWM1(1))
      DP1(2)=DBLE(PWM1(2))
      DP1(3)=DBLE(PWM1(3))
      DV2(0)=DCMPLX(WP1(1))
      DV2(1)=DCMPLX(WP1(2))
      DV2(2)=DCMPLX(WP1(3))
      DV2(3)=DCMPLX(WP1(4))
      DP2(0)=DBLE(PWP1(0))
      DP2(1)=DBLE(PWP1(1))
      DP2(2)=DBLE(PWP1(2))
      DP2(3)=DBLE(PWP1(3))
      DV3(0)=DCMPLX(WM2(1))
      DV3(1)=DCMPLX(WM2(2))
      DV3(2)=DCMPLX(WM2(3))
      DV3(3)=DCMPLX(WM2(4))
      DP3(0)=DBLE(PWM2(0))
      DP3(1)=DBLE(PWM2(1))
      DP3(2)=DBLE(PWM2(2))
      DP3(3)=DBLE(PWM2(3))
      DV4(0)=DCMPLX(WP2(1))
      DV4(1)=DCMPLX(WP2(2))
      DV4(2)=DCMPLX(WP2(3))
      DV4(3)=DCMPLX(WP2(4))
      DP4(0)=DBLE(PWP2(0))
      DP4(1)=DBLE(PWP2(1))
      DP4(2)=DBLE(PWP2(2))
      DP4(3)=DBLE(PWP2(3))
      DGWWA2=DBLE(GWWA)**2
      DGWWZ2=DBLE(GWWZ)**2
      DGW2  =DGWWA2+DGWWZ2
      DMZ   =DBLE(ZMASS)
      DWIDTH=DBLE(ZWIDTH)
C
      V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
      K(0)=DP1(0)+DP4(0)
      K(1)=DP1(1)+DP4(1)
      K(2)=DP1(2)+DP4(2)
      K(3)=DP1(3)+DP4(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      T=K(0)**2-K(1)**2-K(2)**2-K(3)**2
C
      DAS=-1.D0/S
      DAT=-1.D0/T
      DZS=-1.D0/DCMPLX( S-DMZ**2 , DMAX1(DSIGN(DMZ*DWIDTH,S),0.D0) )
      DZT=-1.D0/DCMPLX( T-DMZ**2 , DMAX1(DSIGN(DMZ*DWIDTH,T),0.D0) )
C
      SV1= (DP2(0)+Q(0))*DV1(0) -(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2) -(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0) +(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2) +(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0) -(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2) -(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0) +(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2) +(DP3(3)-Q(3))*DV4(3)
C
      TV1= (DP4(0)+K(0))*DV1(0) -(DP4(1)+K(1))*DV1(1)
     &    -(DP4(2)+K(2))*DV1(2) -(DP4(3)+K(3))*DV1(3)
      TV2=-(DP3(0)-K(0))*DV2(0) +(DP3(1)-K(1))*DV2(1)
     &    +(DP3(2)-K(2))*DV2(2) +(DP3(3)-K(3))*DV2(3)
      TV3= (DP2(0)-K(0))*DV3(0) -(DP2(1)-K(1))*DV3(1)
     &    -(DP2(2)-K(2))*DV3(2) -(DP2(3)-K(3))*DV3(3)
      TV4=-(DP1(0)+K(0))*DV4(0) +(DP1(1)+K(1))*DV4(1)
     &    +(DP1(2)+K(2))*DV4(2) +(DP1(3)+K(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      J14(0)=(DP1(0)-DP4(0))*V14 +TV1*DV4(0) +TV4*DV1(0)
      J14(1)=(DP1(1)-DP4(1))*V14 +TV1*DV4(1) +TV4*DV1(1)
      J14(2)=(DP1(2)-DP4(2))*V14 +TV1*DV4(2) +TV4*DV1(2)
      J14(3)=(DP1(3)-DP4(3))*V14 +TV1*DV4(3) +TV4*DV1(3)
      J32(0)=(DP3(0)-DP2(0))*V23 +TV3*DV2(0) +TV2*DV3(0)
      J32(1)=(DP3(1)-DP2(1))*V23 +TV3*DV2(1) +TV2*DV3(1)
      J32(2)=(DP3(2)-DP2(2))*V23 +TV3*DV2(2) +TV2*DV3(2)
      J32(3)=(DP3(3)-DP2(3))*V23 +TV3*DV2(3) +TV2*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
      JS14=K(0)*J14(0)-K(1)*J14(1)-K(2)*J14(2)-K(3)*J14(3)
      JS32=K(0)*J32(0)-K(1)*J32(1)-K(2)*J32(2)-K(3)*J32(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
      JT=J14(0)*J32(0)-J14(1)*J32(1)-J14(2)*J32(2)-J14(3)*J32(3)
C
      DVERTX = (V12*V34 +V14*V23 -2.D0*V13*V24)*DGW2
     &        +(DZS*DGWWZ2+DAS*DGWWA2)*JS -DZS*DGWWZ2*JS12*JS34/DMZ**2
     &        +(DZT*DGWWZ2+DAT*DGWWA2)*JT -DZT*DGWWZ2*JS14*JS32/DMZ**2
C
      VERTEX = -CMPLX( DVERTX )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JWWWXX(W1,W2,W3,GWWA,GWWZ,ZMASS,ZWIDTH,WMASS,WWIDTH ,
     &                  JWWW)
C
C This subroutine computes an off-shell W+/W- current from the four-    
C point gauge boson coupling, including the contributions of photon and 
C Z exchanges.  The vector propagators for the output W and the internal
C Z bosons are given in unitary gauge, and that of the internal photon  
C is given in Feynman gauge.                                            
C                                                                       
C INPUT:                                                                
C       complex W1(6)          : first  vector                        W1
C       complex W2(6)          : second vector                        W2
C       complex W3(6)          : third  vector                        W3
C       real    GWWA           : coupling constant of W and A       GWWA
C       real    GWWZ           : coupling constant of W and Z       GWWZ
C       real    ZMASS          : mass  of internal Z                    
C       real    ZWIDTH         : width of internal Z                    
C       real    WMASS          : mass  of OUTPUT W                      
C       real    WWIDTH         : width of OUTPUT W                      
C                                                                       
C The possible sets of the inputs are as follows:                       
C   ------------------------------------------------------------------- 
C   |  W1  |  W2  |  W3  |GWWA|GWWZ|ZMASS|ZWIDTH|WMASS|WWIDTH || JWWW | 
C   ------------------------------------------------------------------- 
C   |  W-  |  W+  |  W-  |GWWA|GWWZ|ZMASS|ZWIDTH|WMASS|WWIDTH ||  W+  | 
C   |  W+  |  W-  |  W+  |GWWA|GWWZ|ZMASS|ZWIDTH|WMASS|WWIDTH ||  W-  | 
C   ------------------------------------------------------------------- 
C where all the bosons are defined by the flowing-OUT quantum number.   
C                                                                       
C OUTPUT:                                                               
C       complex JWWW(6)        : W current             J^mu(W':W1,W2,W3)
C
      COMPLEX*8  W1(6),W2(6),W3(6),JWWW(6)
      COMPLEX*16 DW1(0:3),DW2(0:3),DW3(0:3),
     &           JJ(0:3),JS(0:3),JT(0:3),J4(0:3),
     &           JT12(0:3),JT32(0:3),J12(0:3),J32(0:3),
     &           DZS,DZT,DW,W12,W32,W13,P1W2,P2W1,P3W2,P2W3,
     &           JK12,JK32,JSW3,JTW1,P3JS,KSW3,P1JT,KTW1,JQ
      REAL*4     GWWA,GWWZ,ZMASS,ZWIDTH,WMASS,WWIDTH
      REAL*8     P1(0:3),P2(0:3),P3(0:3),Q(0:3),KS(0:3),KT(0:3),
     &           DGWWA2,DGWWZ2,DGW2,DMZ,DWZ,DMW,DWW,MZ2,MW2,Q2,KS2,KT2,
     &           DAS,DAT
C
C .........CHECK........................................................
          IF (ABS(W1(1))+ABS(W1(2))+ABS(W1(3))+ABS(W1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W1(6) in JWWWXX is zero vector'
          IF (ABS(W1(5))+ABS(W1(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W1(6) in JWWWXX has zero momentum'
          IF (ABS(W2(1))+ABS(W2(2))+ABS(W2(3))+ABS(W2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W2(6) in JWWWXX is zero vector'
          IF (ABS(W2(5))+ABS(W2(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W2(6) in JWWWXX has zero momentum'
          IF (ABS(W3(1))+ABS(W3(2))+ABS(W3(3))+ABS(W3(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W3(6) in JWWWXX is zero vector'
          IF (ABS(W3(5))+ABS(W3(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W3(6) in JWWWXX has zero momentum'
          IF (GWWA.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GWWA in JWWWXX is zero coupling'
          IF (GWWZ.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : GWWZ in JWWWXX is zero coupling'
          IF ((GWWA.LT.0.).OR.(GWWA.GE.GWWZ)) THEN
          WRITE(1,*)
     &  ' HELAS-warn  : GWWA/GWWZ in JWWWXX are non-standard couplings'
          WRITE(1,*) '               GWWA = ',GWWA,'  GWWZ = ',GWWZ
          ENDIF
          IF (ZMASS.LE.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : ZMASS in JWWWXX is not positive'
          WRITE(1,*) '               ZMASS = ',ZMASS
          ENDIF
          IF (ZWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : ZWIDTH in JWWWXX is negative'
          WRITE(1,*) '               ZWIDTH = ',ZWIDTH
          ENDIF
          IF (WMASS.LE.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : WMASS in JWWWXX is not positive'
          WRITE(1,*) '               WMASS = ',WMASS
          ENDIF
          IF (WWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : WWIDTH in JWWWXX is negative'
          WRITE(1,*) '               WWIDTH = ',WWIDTH
          ENDIF
C .........CHECK........................................................
C
      JWWW(5) = W1(5)+W2(5)+W3(5)
      JWWW(6) = W1(6)+W2(6)+W3(6)
C
      DW1(0)=DCMPLX(W1(1))
      DW1(1)=DCMPLX(W1(2))
      DW1(2)=DCMPLX(W1(3))
      DW1(3)=DCMPLX(W1(4))
      DW2(0)=DCMPLX(W2(1))
      DW2(1)=DCMPLX(W2(2))
      DW2(2)=DCMPLX(W2(3))
      DW2(3)=DCMPLX(W2(4))
      DW3(0)=DCMPLX(W3(1))
      DW3(1)=DCMPLX(W3(2))
      DW3(2)=DCMPLX(W3(3))
      DW3(3)=DCMPLX(W3(4))
      P1(0)=DBLE(      W1(5))
      P1(1)=DBLE(      W1(6))
      P1(2)=DBLE(AIMAG(W1(6)))
      P1(3)=DBLE(AIMAG(W1(5)))
      P2(0)=DBLE(      W2(5))
      P2(1)=DBLE(      W2(6))
      P2(2)=DBLE(AIMAG(W2(6)))
      P2(3)=DBLE(AIMAG(W2(5)))
      P3(0)=DBLE(      W3(5))
      P3(1)=DBLE(      W3(6))
      P3(2)=DBLE(AIMAG(W3(6)))
      P3(3)=DBLE(AIMAG(W3(5)))
      Q(0)=-(P1(0)+P2(0)+P3(0))
      Q(1)=-(P1(1)+P2(1)+P3(1))
      Q(2)=-(P1(2)+P2(2)+P3(2))
      Q(3)=-(P1(3)+P2(3)+P3(3))
      KS(0)=P1(0)+P2(0)
      KS(1)=P1(1)+P2(1)
      KS(2)=P1(2)+P2(2)
      KS(3)=P1(3)+P2(3)
      KT(0)=P2(0)+P3(0)
      KT(1)=P2(1)+P3(1)
      KT(2)=P2(2)+P3(2)
      KT(3)=P2(3)+P3(3)
      Q2 =Q(0)**2 -(Q(1)**2 +Q(2)**2 +Q(3)**2)
      KS2=KS(0)**2-(KS(1)**2+KS(2)**2+KS(3)**2)
      KT2=KT(0)**2-(KT(1)**2+KT(2)**2+KT(3)**2)
      DGWWA2=DBLE(GWWA)**2
      DGWWZ2=DBLE(GWWZ)**2
      DGW2  =DGWWA2+DGWWZ2
      DMZ=DBLE(ZMASS)
      DWZ=DBLE(ZWIDTH)
      DMW=DBLE(WMASS)
      DWW=DBLE(WWIDTH)
      MZ2=DMZ**2
      MW2=DMW**2
C
C .........CHECK........................................................
          IF (ABS(JWWW(5))+ABS(JWWW(6)).EQ.0.)
     &    WRITE(1,*)
     &    ' HELAS-ERROR : JWWW(6) in JWWWXX has zero momentum'
          IF (KS2.EQ.0.D0) THEN
          WRITE(1,*)
     & ' HELAS-ERROR : JWWW(6) in JWWWXX is on photon pole in s-channel'
          WRITE(1,*) '               k_t     = ',
     &    REAL(KS(0)),REAL(KS(1)),REAL(KS(2)),REAL(KS(3))
          JWWW(1)=CMPLX(0.)
          JWWW(2)=CMPLX(0.)
          JWWW(3)=CMPLX(0.)
          JWWW(4)=CMPLX(0.)
          RETURN
          ENDIF
          IF (KT2.EQ.0.D0) THEN
          WRITE(1,*) 
     & ' HELAS-ERROR : JWWW(6) in JWWWXX is on photon pole in t-channel'
          WRITE(1,*) '               k_t     = ',
     &    REAL(KT(0)),REAL(KT(1)),REAL(KT(2)),REAL(KT(3))
          JWWW(1)=CMPLX(0.)
          JWWW(2)=CMPLX(0.)
          JWWW(3)=CMPLX(0.)
          JWWW(4)=CMPLX(0.)
          RETURN
          ENDIF
          IF (ZWIDTH.EQ.0.) THEN
           IF (KS2.EQ.MZ2) THEN
          WRITE(1,*) 
     &  ' HELAS-ERROR : JWWW(6) in JWWWXX is on ZMASS pole in s-channel'
          WRITE(1,*) '               k_s     = ',
     &    REAL(KS(0)),REAL(KS(1)),REAL(KS(2)),REAL(KS(3))
          WRITE(1,*) '               abs(k_s)= ',SQRT(ABS(REAL(KS2)))
          JWWW(1)=CMPLX(0.)
          JWWW(2)=CMPLX(0.)
          JWWW(3)=CMPLX(0.)
          JWWW(4)=CMPLX(0.)
          RETURN
           ELSE
           IF (KT2.EQ.MZ2) THEN
          WRITE(1,*) 
     &  ' HELAS-ERROR : JWWW(6) in JWWWXX is on ZMASS pole in t-channel'
          WRITE(1,*) '               k_t     = ',
     &    REAL(KT(0)),REAL(KT(1)),REAL(KT(2)),REAL(KT(3))
          WRITE(1,*) '               abs(k_t)= ',SQRT(ABS(REAL(KT2)))
          JWWW(1)=CMPLX(0.)
          JWWW(2)=CMPLX(0.)
          JWWW(3)=CMPLX(0.)
          JWWW(4)=CMPLX(0.)
          RETURN
           ENDIF
           ENDIF
          ENDIF
          IF ((WWIDTH.EQ.0.).AND.(Q2.EQ.MW2)) THEN
          WRITE(1,*) ' HELAS-ERROR : JWWW(6) in JWWWXX is on WMASS pole'
          WRITE(1,*) '               Q     = ',
     &    REAL(Q(0)),REAL(Q(1)),REAL(Q(2)),REAL(Q(3))
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(REAL(Q2)))
          JWWW(1)=CMPLX(0.)
          JWWW(2)=CMPLX(0.)
          JWWW(3)=CMPLX(0.)
          JWWW(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DAS=-DGWWA2/KS2
      DAT=-DGWWA2/KT2
      DZS=-DGWWZ2/DCMPLX( KS2-MZ2 , DMAX1(DSIGN(DMZ*DWZ,KS2),0.D0) )
      DZT=-DGWWZ2/DCMPLX( KT2-MZ2 , DMAX1(DSIGN(DMZ*DWZ,KT2),0.D0) )
      DW =-1.0D0/DCMPLX( Q2 -MW2 , DMAX1(DSIGN(DMW*DWW,Q2 ),0.D0) )
C  For the running width, use below instead of the above DW.
C      DW =-1.0D0/DCMPLX( Q2 -MW2 , DMAX1(DWW*Q2/DMW,0.D0) )
C
      W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
      P3W2= (P3(0)+KT(0))*DW2(0)-(P3(1)+KT(1))*DW2(1)
     &     -(P3(2)+KT(2))*DW2(2)-(P3(3)+KT(3))*DW2(3)
      P2W3= (P2(0)+KT(0))*DW3(0)-(P2(1)+KT(1))*DW3(1)
     &     -(P2(2)+KT(2))*DW3(2)-(P2(3)+KT(3))*DW3(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
      JT32(0)= (P3(0)-P2(0))*W32 + P2W3*DW2(0) - P3W2*DW3(0)
      JT32(1)= (P3(1)-P2(1))*W32 + P2W3*DW2(1) - P3W2*DW3(1)
      JT32(2)= (P3(2)-P2(2))*W32 + P2W3*DW2(2) - P3W2*DW3(2)
      JT32(3)= (P3(3)-P2(3))*W32 + P2W3*DW2(3) - P3W2*DW3(3)
C
      JK12=(JT12(0)*KS(0)-JT12(1)*KS(1)-JT12(2)*KS(2)-JT12(3)*KS(3))/MZ2
      JK32=(JT32(0)*KT(0)-JT32(1)*KT(1)-JT32(2)*KT(2)-JT32(3)*KT(3))/MZ2
C
      J12(0)=JT12(0)*(DAS+DZS)-KS(0)*JK12*DZS
      J12(1)=JT12(1)*(DAS+DZS)-KS(1)*JK12*DZS
      J12(2)=JT12(2)*(DAS+DZS)-KS(2)*JK12*DZS
      J12(3)=JT12(3)*(DAS+DZS)-KS(3)*JK12*DZS
      J32(0)=JT32(0)*(DAT+DZT)-KT(0)*JK32*DZT
      J32(1)=JT32(1)*(DAT+DZT)-KT(1)*JK32*DZT
      J32(2)=JT32(2)*(DAT+DZT)-KT(2)*JK32*DZT
      J32(3)=JT32(3)*(DAT+DZT)-KT(3)*JK32*DZT
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
      JTW1=J32(0)*DW1(0)-J32(1)*DW1(1)-J32(2)*DW1(2)-J32(3)*DW1(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
      P1JT= (P1(0)-Q(0))*J32(0)-(P1(1)-Q(1))*J32(1)
     &     -(P1(2)-Q(2))*J32(2)-(P1(3)-Q(3))*J32(3)
      KTW1= (KT(0)-Q(0))*DW1(0)-(KT(1)-Q(1))*DW1(1)
     &     -(KT(2)-Q(2))*DW1(2)-(KT(3)-Q(3))*DW1(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
      JT(0)= (KT(0)-P1(0))*JTW1 + P1JT*DW1(0) - KTW1*J32(0)
      JT(1)= (KT(1)-P1(1))*JTW1 + P1JT*DW1(1) - KTW1*J32(1)
      JT(2)= (KT(2)-P1(2))*JTW1 + P1JT*DW1(2) - KTW1*J32(2)
      JT(3)= (KT(3)-P1(3))*JTW1 + P1JT*DW1(3) - KTW1*J32(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DGW2*( DW1(0)*W32 + DW3(0)*W12 - 2.D0*DW2(0)*W13 )
      J4(1)=DGW2*( DW1(1)*W32 + DW3(1)*W12 - 2.D0*DW2(1)*W13 )
      J4(2)=DGW2*( DW1(2)*W32 + DW3(2)*W12 - 2.D0*DW2(2)*W13 )
      J4(3)=DGW2*( DW1(3)*W32 + DW3(3)*W12 - 2.D0*DW2(3)*W13 )
C
      JJ(0)=JS(0)+JT(0)+J4(0)
      JJ(1)=JS(1)+JT(1)+J4(1)
      JJ(2)=JS(2)+JT(2)+J4(2)
      JJ(3)=JS(3)+JT(3)+J4(3)
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MW2
C
      JWWW(1) = CMPLX( (JJ(0)-JQ*Q(0))*DW )
      JWWW(2) = CMPLX( (JJ(1)-JQ*Q(1))*DW )
      JWWW(3) = CMPLX( (JJ(2)-JQ*Q(2))*DW )
      JWWW(4) = CMPLX( (JJ(3)-JQ*Q(3))*DW )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE W3W3XX(WM,W31,WP,W32,G31,G32,WMASS,WWIDTH , VERTEX)
C
C This subroutine computes an amplitude of the four-point coupling of   
C the W-, W+ and two W3/Z/A.  The amplitude includes the contributions  
C of W exchange diagrams.  The internal W propagator is given in unitary
C gauge.  If one sets WMASS=0.0, then the gggg vertex is given (see sect
C 2.9.1 of the manual).
C                                                                       
C INPUT:                                                                
C       complex WM(0:3)        : flow-OUT W-                         WM 
C       complex W31(0:3)       : first    W3/Z/A                     W31
C       complex WP(0:3)        : flow-OUT W+                         WP 
C       complex W32(0:3)       : second   W3/Z/A                     W32
C       real    G31            : coupling of W31 with W-/W+             
C       real    G32            : coupling of W32 with W-/W+             
C                                                  (see the table below)
C       real    WMASS          : mass  of W                             
C       real    WWIDTH         : width of W                             
C                                                                       
C The possible sets of the inputs are as follows:                       
C   -------------------------------------------                         
C   |  WM  |  W31 |  WP  |  W32 |  G31 |  G32 |                         
C   -------------------------------------------                         
C   |  W-  |  W3  |  W+  |  W3  |  GW  |  GW  |                         
C   |  W-  |  W3  |  W+  |  Z   |  GW  | GWWZ |                         
C   |  W-  |  W3  |  W+  |  A   |  GW  | GWWA |                         
C   |  W-  |  Z   |  W+  |  Z   | GWWZ | GWWZ |                         
C   |  W-  |  Z   |  W+  |  A   | GWWZ | GWWA |                         
C   |  W-  |  A   |  W+  |  A   | GWWA | GWWA |                         
C   -------------------------------------------                         
C where all the bosons are defined by the flowing-OUT quantum number.   
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude          Gamma(WM,W31,WP,W32)
C
      COMPLEX    WM(6),W31(6),WP(6),W32(6),VERTEX
      COMPLEX*16 DV1(0:3),DV2(0:3),DV3(0:3),DV4(0:3),
     &           J12(0:3),J34(0:3),J14(0:3),J32(0:3),DVERTX,
     &           SV1,SV2,SV3,SV4,TV1,TV2,TV3,TV4,DWS,DWT,
     &           V12,V13,V14,V23,V24,V34,JS12,JS34,JS14,JS32,JS,JT
      REAL       PWM(0:3),PW31(0:3),PWP(0:3),PW32(0:3),
     &           G31,G32,WMASS,WWIDTH
      REAL*8     Q(0:3),K(0:3),DP1(0:3),DP2(0:3),DP3(0:3),DP4(0:3),
     &           DMW,DWIDTH,DM2INV,S,T
            REAL PM
C
      PWM(0)=REAL( WM(5))
      PWM(1)=REAL( WM(6))
      PWM(2)=AIMAG(WM(6))
      PWM(3)=AIMAG(WM(5))
      PWP(0)=REAL( WP(5))
      PWP(1)=REAL( WP(6))
      PWP(2)=AIMAG(WP(6))
      PWP(3)=AIMAG(WP(5))
      PW31(0)=REAL( W31(5))
      PW31(1)=REAL( W31(6))
      PW31(2)=AIMAG(W31(6))
      PW31(3)=AIMAG(W31(5))
      PW32(0)=REAL( W32(5))
      PW32(1)=REAL( W32(6))
      PW32(2)=AIMAG(W32(6))
      PW32(3)=AIMAG(W32(5))
C
C .........CHECK........................................................
          IF ( ABS(WM(1))+ABS(WM(2))
     &        +ABS(WM(3))+ABS(WM(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WM(6) in W3W3XX is zero vector'
          IF (ABS(WM(5))+ABS(WM(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WM(6) in W3W3XX has zero momentum'
          IF ( ABS(W31(1))+ABS(W31(2))
     &        +ABS(W31(3))+ABS(W31(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W31(6) in W3W3XX is zero vector'
          IF (ABS(W31(5))+ABS(W31(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W31(6) in W3W3XX has zero momentum'
          IF ( ABS(WP(1))+ABS(WP(2))
     &        +ABS(WP(3))+ABS(WP(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : WP(6) in W3W3XX is zero vector'
          IF (ABS(WP(5))+ABS(WP(5)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : WP(6) in W3W3XX has zero momentum'
          IF ( ABS(W32(1))+ABS(W32(2))
     &        +ABS(W32(3))+ABS(W32(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W32(6) in W3W3XX is zero vector'
          IF (ABS(W32(5))+ABS(W32(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W32(6) in W3W3XX has zero momentum'
          PM=MAX( ABS(PWM(0)),ABS(PW31(0)),ABS(PWP(0)),ABS(PW32(0)),
     &            ABS(PWM(1)),ABS(PW31(1)),ABS(PWP(1)),ABS(PW32(1)),
     &            ABS(PWM(2)),ABS(PW31(2)),ABS(PWP(2)),ABS(PW32(2)),
     &            ABS(PWM(3)),ABS(PW31(3)),ABS(PWP(3)),ABS(PW32(3)) )
          IF (( ABS(WM(5)+W31(5)+WP(5)+W32(5))
     &         +ABS(WM(6)+W31(6)+WP(6)+W32(6))).GE.PM*4.E-5) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : WM(6),W31(6),WP(6),W32(6) in W3W3XX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (G31.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G31 in W3W3XX is zero coupling'
          IF (G32.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G32 in W3W3XX is zero coupling'
          IF (G31.LT.0.) THEN
          WRITE(1,*) 
     &    ' HELAS-warn  : G31 in W3W3XX is non-standard coupling'
          WRITE(1,*) '               G31 = ',G31
          ENDIF
          IF (G32.LT.0.) THEN
          WRITE(1,*) 
     &    ' HELAS-warn  : G32 in W3W3XX is non-standard coupling'
          WRITE(1,*) '               G32 = ',G32
          ENDIF
          IF (WMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : WMASS in W3W3XX is negative'
          WRITE(1,*) '               WMASS = ',WMASS
          ENDIF
          IF (WWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : WWIDTH in W3W3XX is negative'
          WRITE(1,*) '               WWIDTH = ',WWIDTH
          ENDIF
C .........CHECK........................................................
C
      DV1(0)=DCMPLX(WM(1))
      DV1(1)=DCMPLX(WM(2))
      DV1(2)=DCMPLX(WM(3))
      DV1(3)=DCMPLX(WM(4))
      DP1(0)=DBLE(PWM(0))
      DP1(1)=DBLE(PWM(1))
      DP1(2)=DBLE(PWM(2))
      DP1(3)=DBLE(PWM(3))
      DV2(0)=DCMPLX(W31(1))
      DV2(1)=DCMPLX(W31(2))
      DV2(2)=DCMPLX(W31(3))
      DV2(3)=DCMPLX(W31(4))
      DP2(0)=DBLE(PW31(0))
      DP2(1)=DBLE(PW31(1))
      DP2(2)=DBLE(PW31(2))
      DP2(3)=DBLE(PW31(3))
      DV3(0)=DCMPLX(WP(1))
      DV3(1)=DCMPLX(WP(2))
      DV3(2)=DCMPLX(WP(3))
      DV3(3)=DCMPLX(WP(4))
      DP3(0)=DBLE(PWP(0))
      DP3(1)=DBLE(PWP(1))
      DP3(2)=DBLE(PWP(2))
      DP3(3)=DBLE(PWP(3))
      DV4(0)=DCMPLX(W32(1))
      DV4(1)=DCMPLX(W32(2))
      DV4(2)=DCMPLX(W32(3))
      DV4(3)=DCMPLX(W32(4))
      DP4(0)=DBLE(PW32(0))
      DP4(1)=DBLE(PW32(1))
      DP4(2)=DBLE(PW32(2))
      DP4(3)=DBLE(PW32(3))
      DMW   =DBLE(WMASS)
      DWIDTH=DBLE(WWIDTH)
      DMW2  =DMW**2
C
      IF (WMASS.EQ.0.) GOTO 10
C
      DM2INV=1.0D0/DMW2
C
      V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
      K(0)=DP1(0)+DP4(0)
      K(1)=DP1(1)+DP4(1)
      K(2)=DP1(2)+DP4(2)
      K(3)=DP1(3)+DP4(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      T=K(0)**2-K(1)**2-K(2)**2-K(3)**2
      DWS=-1.D0/DCMPLX( S-DMW2 , DMAX1(DSIGN(DMW*DWIDTH,S),0.D0) )
      DWT=-1.D0/DCMPLX( T-DMW2 , DMAX1(DSIGN(DMW*DWIDTH,T),0.D0) )
C
      SV1= (DP2(0)+Q(0))*DV1(0)-(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2)-(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0)+(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2)+(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0)-(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2)-(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0)+(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2)+(DP3(3)-Q(3))*DV4(3)
C
      TV1= (DP4(0)+K(0))*DV1(0)-(DP4(1)+K(1))*DV1(1)
     &    -(DP4(2)+K(2))*DV1(2)-(DP4(3)+K(3))*DV1(3)
      TV2=-(DP3(0)-K(0))*DV2(0)+(DP3(1)-K(1))*DV2(1)
     &    +(DP3(2)-K(2))*DV2(2)+(DP3(3)-K(3))*DV2(3)
      TV3= (DP2(0)-K(0))*DV3(0)-(DP2(1)-K(1))*DV3(1)
     &    -(DP2(2)-K(2))*DV3(2)-(DP2(3)-K(3))*DV3(3)
      TV4=-(DP1(0)+K(0))*DV4(0)+(DP1(1)+K(1))*DV4(1)
     &    +(DP1(2)+K(2))*DV4(2)+(DP1(3)+K(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      J14(0)=(DP1(0)-DP4(0))*V14 +TV1*DV4(0) +TV4*DV1(0)
      J14(1)=(DP1(1)-DP4(1))*V14 +TV1*DV4(1) +TV4*DV1(1)
      J14(2)=(DP1(2)-DP4(2))*V14 +TV1*DV4(2) +TV4*DV1(2)
      J14(3)=(DP1(3)-DP4(3))*V14 +TV1*DV4(3) +TV4*DV1(3)
      J32(0)=(DP3(0)-DP2(0))*V23 +TV3*DV2(0) +TV2*DV3(0)
      J32(1)=(DP3(1)-DP2(1))*V23 +TV3*DV2(1) +TV2*DV3(1)
      J32(2)=(DP3(2)-DP2(2))*V23 +TV3*DV2(2) +TV2*DV3(2)
      J32(3)=(DP3(3)-DP2(3))*V23 +TV3*DV2(3) +TV2*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
      JS14=K(0)*J14(0)-K(1)*J14(1)-K(2)*J14(2)-K(3)*J14(3)
      JS32=K(0)*J32(0)-K(1)*J32(1)-K(2)*J32(2)-K(3)*J32(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
      JT=J14(0)*J32(0)-J14(1)*J32(1)-J14(2)*J32(2)-J14(3)*J32(3)
C
      DVERTX = V12*V34 +V14*V23 -2.D0*V13*V24
     &        +DWS*(JS -JS12*JS34*DM2INV) +DWT*(JT -JS14*JS32*DM2INV)
C
      VERTEX = CMPLX( DVERTX ) * (G31*G32)
C
      RETURN
C
  10  V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      DWS=-1.D0/DCMPLX( S )
C
      SV1= (DP2(0)+Q(0))*DV1(0)-(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2)-(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0)+(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2)+(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0)-(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2)-(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0)+(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2)+(DP3(3)-Q(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
C
      DVERTX = V14*V23 -V13*V24 +DWS*JS
C
      VERTEX = CMPLX( DVERTX ) * (G31*G32)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JW3WXX(W1,W2,W3,G1,G2,WMASS,WWIDTH,VMASS,VWIDTH , JW3W)
C
C This subroutine computes an off-shell W+, W-, W3, Z or photon current 
C from the four-point gauge boson coupling, including the contributions 
C of W exchange diagrams.  The vector propagator is given in Feynman    
C gauge for a photon and in unitary gauge for W and Z bosons.  If one   
C sets WMASS=0.0, then the ggg-->g current is given (see sect 2.9.1 of 
C the manual).                                                          
C                                                                       
C INPUT:                                                                
C       complex W1(6)          : first  vector                        W1
C       complex W2(6)          : second vector                        W2
C       complex W3(6)          : third  vector                        W3
C       real    G1             : first  coupling constant               
C       real    G2             : second coupling constant               
C                                                  (see the table below)
C       real    WMASS          : mass  of internal W                    
C       real    WWIDTH         : width of internal W                    
C       real    VMASS          : mass  of OUTPUT W'                     
C       real    VWIDTH         : width of OUTPUT W'                     
C                                                                       
C The possible sets of the inputs are as follows:                       
C   ------------------------------------------------------------------- 
C   |  W1  |  W2  |  W3  | G1 | G2 |WMASS|WWIDTH|VMASS|VWIDTH || JW3W | 
C   ------------------------------------------------------------------- 
C   |  W-  |  W3  |  W+  | GW |GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   | 
C   |  W-  |  W3  |  W+  | GW |GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   | 
C   |  W-  |  Z   |  W+  |GWWZ|GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   | 
C   |  W-  |  Z   |  W+  |GWWZ|GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   | 
C   |  W-  |  A   |  W+  |GWWA|GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   | 
C   |  W-  |  A   |  W+  |GWWA|GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   | 
C   ------------------------------------------------------------------- 
C   |  W3  |  W-  |  W3  | GW | GW |WMASS|WWIDTH|WMASS|WWIDTH ||  W+  | 
C   |  W3  |  W+  |  W3  | GW | GW |WMASS|WWIDTH|WMASS|WWIDTH ||  W-  | 
C   |  W3  |  W-  |  Z   | GW |GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  | 
C   |  W3  |  W+  |  Z   | GW |GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  | 
C   |  W3  |  W-  |  A   | GW |GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  | 
C   |  W3  |  W+  |  A   | GW |GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  | 
C   |  Z   |  W-  |  Z   |GWWZ|GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  | 
C   |  Z   |  W+  |  Z   |GWWZ|GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  | 
C   |  Z   |  W-  |  A   |GWWZ|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  | 
C   |  Z   |  W+  |  A   |GWWZ|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  | 
C   |  A   |  W-  |  A   |GWWA|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  | 
C   |  A   |  W+  |  A   |GWWA|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  | 
C   ------------------------------------------------------------------- 
C where all the bosons are defined by the flowing-OUT quantum number.   
C                                                                       
C OUTPUT:                                                               
C       complex JW3W(6)        : W current             J^mu(W':W1,W2,W3)
C
      COMPLEX*8  W1(6),W2(6),W3(6),JW3W(6)
      COMPLEX*16 DW1(0:3),DW2(0:3),DW3(0:3),
     &           JJ(0:3),JS(0:3),JT(0:3),J4(0:3),
     &           JT12(0:3),JT32(0:3),J12(0:3),J32(0:3),
     &           DWS,DWT,DV,W12,W32,W13,P1W2,P2W1,P3W2,P2W3,
     &           JK12,JK32,JSW3,JTW1,P3JS,KSW3,P1JT,KTW1,JQ
      REAL*4     G1,G2,WMASS,WWIDTH,VMASS,VWIDTH
      REAL*8     P1(0:3),P2(0:3),P3(0:3),Q(0:3),KS(0:3),KT(0:3),
     &           DG2,DMW,DWW,DMV,DWV,MW2,MV2,MW2INV,Q2,KS2,KT2
C
C .........CHECK........................................................
          IF (ABS(W1(1))+ABS(W1(2))+ABS(W1(3))+ABS(W1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W1(6) in JW3WXX is zero vector'
          IF (ABS(W1(5))+ABS(W1(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W1(6) in JW3WXX has zero momentum'
          IF (ABS(W2(1))+ABS(W2(2))+ABS(W2(3))+ABS(W2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W2(6) in JW3WXX is zero vector'
          IF (ABS(W2(5))+ABS(W2(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W2(6) in JW3WXX has zero momentum'
          IF (ABS(W3(1))+ABS(W3(2))+ABS(W3(3))+ABS(W3(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : W3(6) in JW3WXX is zero vector'
          IF (ABS(W3(5))+ABS(W3(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : W3(6) in JW3WXX has zero momentum'
          IF (G1.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G1 in JW3WXX is zero coupling'
          IF (G2.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G2 in JW3WXX is zero coupling'
          IF (G1.LT.0.) THEN
          WRITE(1,*) 
     &    ' HELAS-warn  : G1 in JW3WXX is non-standard coupling'
          WRITE(1,*) '               G1 = ',G1
          ENDIF
          IF (G2.LT.0.) THEN
          WRITE(1,*) 
     &    ' HELAS-warn  : G2 in JW3WXX is non-standard coupling'
          WRITE(1,*) '               G2 = ',G2
          ENDIF
          IF (WMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : WMASS in JW3WXX is negative'
          WRITE(1,*) '               WMASS = ',WMASS
          ENDIF
          IF (WWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : WWIDTH in JW3WXX is negative'
          WRITE(1,*) '               WWIDTH = ',WWIDTH
          ENDIF
          IF (VMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VMASS in JW3WXX is negative'
          WRITE(1,*) '               VMASS = ',VMASS
          ENDIF
          IF (VWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VWIDTH in JW3WXX is negative'
          WRITE(1,*) '               VWIDTH = ',VWIDTH
          ENDIF
C .........CHECK........................................................
C
      JW3W(5) = W1(5)+W2(5)+W3(5)
      JW3W(6) = W1(6)+W2(6)+W3(6)
C
      DW1(0)=DCMPLX(W1(1))
      DW1(1)=DCMPLX(W1(2))
      DW1(2)=DCMPLX(W1(3))
      DW1(3)=DCMPLX(W1(4))
      DW2(0)=DCMPLX(W2(1))
      DW2(1)=DCMPLX(W2(2))
      DW2(2)=DCMPLX(W2(3))
      DW2(3)=DCMPLX(W2(4))
      DW3(0)=DCMPLX(W3(1))
      DW3(1)=DCMPLX(W3(2))
      DW3(2)=DCMPLX(W3(3))
      DW3(3)=DCMPLX(W3(4))
      P1(0)=DBLE(      W1(5))
      P1(1)=DBLE(      W1(6))
      P1(2)=DBLE(AIMAG(W1(6)))
      P1(3)=DBLE(AIMAG(W1(5)))
      P2(0)=DBLE(      W2(5))
      P2(1)=DBLE(      W2(6))
      P2(2)=DBLE(AIMAG(W2(6)))
      P2(3)=DBLE(AIMAG(W2(5)))
      P3(0)=DBLE(      W3(5))
      P3(1)=DBLE(      W3(6))
      P3(2)=DBLE(AIMAG(W3(6)))
      P3(3)=DBLE(AIMAG(W3(5)))
      Q(0)=-(P1(0)+P2(0)+P3(0))
      Q(1)=-(P1(1)+P2(1)+P3(1))
      Q(2)=-(P1(2)+P2(2)+P3(2))
      Q(3)=-(P1(3)+P2(3)+P3(3))
      KS(0)=P1(0)+P2(0)
      KS(1)=P1(1)+P2(1)
      KS(2)=P1(2)+P2(2)
      KS(3)=P1(3)+P2(3)
      KT(0)=P2(0)+P3(0)
      KT(1)=P2(1)+P3(1)
      KT(2)=P2(2)+P3(2)
      KT(3)=P2(3)+P3(3)
      Q2 =Q(0)**2 -(Q(1)**2 +Q(2)**2 +Q(3)**2)
      KS2=KS(0)**2-(KS(1)**2+KS(2)**2+KS(3)**2)
      KT2=KT(0)**2-(KT(1)**2+KT(2)**2+KT(3)**2)
      DG2=DBLE(G1)*DBLE(G2)
      DMW=DBLE(WMASS)
      DWW=DBLE(WWIDTH)
      DMV=DBLE(VMASS)
      DWV=DBLE(VWIDTH)
      MW2=DMW**2
      MV2=DMV**2
      MW2INV=1.0D0/MW2
C
C .........CHECK........................................................
          IF (ABS(JW3W(5))+ABS(JW3W(6)).EQ.0.)
     &    WRITE(1,*) 
     &    ' HELAS-ERROR : JW3W(6) in JW3WXX has zero momentum'
          IF (WWIDTH.EQ.0.) THEN
           IF (KS2.EQ.MW2) THEN
          WRITE(1,*) 
     &  ' HELAS-ERROR : JW3W(6) in JW3WXX is on WMASS pole in s-channel'
          WRITE(1,*) '               k_s     = ',
     &    REAL(KS(0)),REAL(KS(1)),REAL(KS(2)),REAL(KS(3))
          WRITE(1,*) '               abs(k_s)= ',SQRT(ABS(REAL(KS2)))
          JW3W(1)=CMPLX(0.)
          JW3W(2)=CMPLX(0.)
          JW3W(3)=CMPLX(0.)
          JW3W(4)=CMPLX(0.)
          RETURN
           ELSE
           IF (KT2.EQ.MW2) THEN
          WRITE(1,*) 
     &  ' HELAS-ERROR : JW3W(6) in JW3WXX is on WMASS pole in t-channel'
          WRITE(1,*) '               k_t     = ',
     &    REAL(KT(0)),REAL(KT(1)),REAL(KT(2)),REAL(KT(3))
          WRITE(1,*) '               abs(k_t)= ',SQRT(ABS(REAL(KT2)))
          JW3W(1)=CMPLX(0.)
          JW3W(2)=CMPLX(0.)
          JW3W(3)=CMPLX(0.)
          JW3W(4)=CMPLX(0.)
          RETURN
           ENDIF
           ENDIF
          ENDIF
          IF ((VWIDTH.EQ.0.).AND.(Q2.EQ.MV2)) THEN
          WRITE(1,*) ' HELAS-ERROR : JW3W(6) in JW3WXX is on VMASS pole'
          WRITE(1,*) '               Q     = ',
     &    REAL(Q(0)),REAL(Q(1)),REAL(Q(2)),REAL(Q(3))
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(REAL(Q2)))
          JW3W(1)=CMPLX(0.)
          JW3W(2)=CMPLX(0.)
          JW3W(3)=CMPLX(0.)
          JW3W(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DWS=-DG2/DCMPLX( KS2-MW2 , DMAX1(DSIGN(DMW*DWW,KS2),0.D0) )
      DWT=-DG2/DCMPLX( KT2-MW2 , DMAX1(DSIGN(DMW*DWW,KT2),0.D0) )
      IF (VMASS.EQ.0.) THEN
      DV = 1.0D0/DCMPLX( Q2 )
      ELSE
      DV = 1.0D0/DCMPLX( Q2 -MV2 , DMAX1(DSIGN(DMV*DWV,Q2 ),0.D0) )
      ENDIF
C  For the running width, use below instead of the above DV.
C      DV = 1.0D0/DCMPLX( Q2 -MV2 , DMAX1(DWV*Q2/DMV,0.D0) )
C
C
      W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      IF (WMASS.EQ.0.) GOTO 10
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
      P3W2= (P3(0)+KT(0))*DW2(0)-(P3(1)+KT(1))*DW2(1)
     &     -(P3(2)+KT(2))*DW2(2)-(P3(3)+KT(3))*DW2(3)
      P2W3= (P2(0)+KT(0))*DW3(0)-(P2(1)+KT(1))*DW3(1)
     &     -(P2(2)+KT(2))*DW3(2)-(P2(3)+KT(3))*DW3(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
      JT32(0)= (P3(0)-P2(0))*W32 + P2W3*DW2(0) - P3W2*DW3(0)
      JT32(1)= (P3(1)-P2(1))*W32 + P2W3*DW2(1) - P3W2*DW3(1)
      JT32(2)= (P3(2)-P2(2))*W32 + P2W3*DW2(2) - P3W2*DW3(2)
      JT32(3)= (P3(3)-P2(3))*W32 + P2W3*DW2(3) - P3W2*DW3(3)
C
      JK12=(JT12(0)*KS(0)-JT12(1)*KS(1)-JT12(2)*KS(2)-JT12(3)*KS(3))
     &     *MW2INV
      JK32=(JT32(0)*KT(0)-JT32(1)*KT(1)-JT32(2)*KT(2)-JT32(3)*KT(3))
     &     *MW2INV
C
      J12(0)=(JT12(0)-KS(0)*JK12)*DWS
      J12(1)=(JT12(1)-KS(1)*JK12)*DWS
      J12(2)=(JT12(2)-KS(2)*JK12)*DWS
      J12(3)=(JT12(3)-KS(3)*JK12)*DWS
      J32(0)=(JT32(0)-KT(0)*JK32)*DWT
      J32(1)=(JT32(1)-KT(1)*JK32)*DWT
      J32(2)=(JT32(2)-KT(2)*JK32)*DWT
      J32(3)=(JT32(3)-KT(3)*JK32)*DWT
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
      JTW1=J32(0)*DW1(0)-J32(1)*DW1(1)-J32(2)*DW1(2)-J32(3)*DW1(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
      P1JT= (P1(0)-Q(0))*J32(0)-(P1(1)-Q(1))*J32(1)
     &     -(P1(2)-Q(2))*J32(2)-(P1(3)-Q(3))*J32(3)
      KTW1= (KT(0)-Q(0))*DW1(0)-(KT(1)-Q(1))*DW1(1)
     &     -(KT(2)-Q(2))*DW1(2)-(KT(3)-Q(3))*DW1(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
      JT(0)= (KT(0)-P1(0))*JTW1 + P1JT*DW1(0) - KTW1*J32(0)
      JT(1)= (KT(1)-P1(1))*JTW1 + P1JT*DW1(1) - KTW1*J32(1)
      JT(2)= (KT(2)-P1(2))*JTW1 + P1JT*DW1(2) - KTW1*J32(2)
      JT(3)= (KT(3)-P1(3))*JTW1 + P1JT*DW1(3) - KTW1*J32(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DG2*( DW1(0)*W32 + DW3(0)*W12 - 2.D0*DW2(0)*W13 )
      J4(1)=DG2*( DW1(1)*W32 + DW3(1)*W12 - 2.D0*DW2(1)*W13 )
      J4(2)=DG2*( DW1(2)*W32 + DW3(2)*W12 - 2.D0*DW2(2)*W13 )
      J4(3)=DG2*( DW1(3)*W32 + DW3(3)*W12 - 2.D0*DW2(3)*W13 )
C
      JJ(0)=JS(0)+JT(0)+J4(0)
      JJ(1)=JS(1)+JT(1)+J4(1)
      JJ(2)=JS(2)+JT(2)+J4(2)
      JJ(3)=JS(3)+JT(3)+J4(3)
C
      IF (VMASS.EQ.0.) GOTO 20
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MV2
C
      JW3W(1) = CMPLX( (JJ(0)-JQ*Q(0))*DV )
      JW3W(2) = CMPLX( (JJ(1)-JQ*Q(1))*DV )
      JW3W(3) = CMPLX( (JJ(2)-JQ*Q(2))*DV )
      JW3W(4) = CMPLX( (JJ(3)-JQ*Q(3))*DV )
C
      RETURN
C
  10  W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
C
      J12(0)=JT12(0)*DWS
      J12(1)=JT12(1)*DWS
      J12(2)=JT12(2)*DWS
      J12(3)=JT12(3)*DWS
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DG2*( DW1(0)*W32 - DW2(0)*W13 )
      J4(1)=DG2*( DW1(1)*W32 - DW2(1)*W13 )
      J4(2)=DG2*( DW1(2)*W32 - DW2(2)*W13 )
      J4(3)=DG2*( DW1(3)*W32 - DW2(3)*W13 )
C
      JJ(0)=JS(0)+J4(0)
      JJ(1)=JS(1)+J4(1)
      JJ(2)=JS(2)+J4(2)
      JJ(3)=JS(3)+J4(3)
C
      IF (VMASS.EQ.0.) GOTO 20
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MV2
C
      JW3W(1) = CMPLX( (JJ(0)-JQ*Q(0))*DV )
      JW3W(2) = CMPLX( (JJ(1)-JQ*Q(1))*DV )
      JW3W(3) = CMPLX( (JJ(2)-JQ*Q(2))*DV )
      JW3W(4) = CMPLX( (JJ(3)-JQ*Q(3))*DV )
C
      RETURN
C
  20  JW3W(1) = CMPLX( JJ(0)*DV )
      JW3W(2) = CMPLX( JJ(1)*DV )
      JW3W(3) = CMPLX( JJ(2)*DV )
      JW3W(4) = CMPLX( JJ(3)*DV )
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VVSSXX(V1,V2,S1,S2,G , VERTEX)
C
C This subroutine computes an amplitude of the vector-vector-scalar-    
C scalar coupling.                                                      
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                 GVVHH
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude            Gamma(V1,V2,S1,S2)
C
      COMPLEX V1(6),V2(6),S1(3),S2(3),VERTEX
      REAL    G
         REAL P0,P1,P2,P3,Q0,Q1,Q2,Q3,R0,R1,R2,R3,O0,O1,O2,O3,PM
C
C .........CHECK........................................................
          P0=REAL( V1(2))
          P1=REAL( V1(3))
          P2=AIMAG(V1(3))
          P3=AIMAG(V1(2))
          Q0=REAL( V2(2))
          Q1=REAL( V2(3))
          Q2=AIMAG(V2(3))
          Q3=AIMAG(V2(2))
          R0=REAL( S1(2))
          R1=REAL( S1(3))
          R2=AIMAG(S1(3))
          R3=AIMAG(S1(2))
          O0=REAL( S2(2))
          O1=REAL( S2(3))
          O2=AIMAG(S2(3))
          O3=AIMAG(S2(2))
          IF (ABS(V1(1))+ABS(V1(2))+ABS(V1(3))+ABS(V1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V1(6) in VVSSXX is zero vector'
          IF (ABS(V1(5))+ABS(V1(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V1(6) in VVSSXX has zero momentum'
          IF (ABS(V2(1))+ABS(V2(2))+ABS(V2(3))+ABS(V2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V2(6) in VVSSXX is zero vector'
          IF (ABS(V2(5))+ABS(V2(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V2(6) in VVSSXX has zero momentum'
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in VVSSXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in VVSSXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in VVSSXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in VVSSXX has zero momentum'
          PM=MAX( ABS(P0),ABS(Q0),ABS(R0),ABS(O0),
     &            ABS(P1),ABS(Q1),ABS(R1),ABS(O1),
     &            ABS(P2),ABS(Q2),ABS(R2),ABS(O2),
     &            ABS(P3),ABS(Q3),ABS(R3),ABS(O3) )
          IF (( ABS(V1(5)+V2(5)+S1(2)+S2(2))
     &         +ABS(V1(6)+V2(6)+S1(3)+S2(3))).GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : V1(6),V2(6),S1(3),S2(3) in VVSSXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in VVSSXX is zero coupling'
C .........CHECK........................................................
C
      VERTEX = G*S1(1)*S2(1)
     &        *(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JVSSXX(VC,S1,S2,G,VMASS,VWIDTH , JVSS)
C
C This subroutine computes an off-shell vector current from the vector- 
C vector-scalar-scalar coupling.  The vector propagator is given in     
C Feynman gauge for a massless vector and in unitary gauge for a massive
C vector.                                                               
C                                                                       
C INPUT:                                                                
C       complex VC(6)          : input  vector                        V 
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                 GVVHH
C       real    VMASS          : mass  of OUTPUT vector V'              
C       real    VWIDTH         : width of OUTPUT vector V'              
C                                                                       
C OUTPUT:                                                               
C       complex JVSS(6)        : vector current         J^mu(V':V,S1,S2)
C
      COMPLEX VC(6),S1(3),S2(3),JVSS(6),DG
      REAL    Q(0:3),G,VMASS,VWIDTH,Q2,VK,VM2
C
C .........CHECK........................................................
          IF (ABS(VC(1))+ABS(VC(2))+ABS(VC(3))+ABS(VC(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : VC(6) in JVSSXX is zero vector'
          IF (ABS(VC(5))+ABS(VC(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : VC(6) in JVSSXX has zero momentum'
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in JVSSXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in JVSSXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in JVSSXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in JVSSXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in JVSSXX is zero coupling'
          IF (VMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VMASS in JVSSXX is negative'
          WRITE(1,*) '               VMASS = ',VMASS
          ENDIF
          IF (VWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : VWIDTH in JVSSXX is negative'
          WRITE(1,*) '               VWIDTH = ',VWIDTH
          ENDIF
C .........CHECK........................................................
C
      JVSS(5) = VC(5)+S1(2)+S2(2)
      JVSS(6) = VC(6)+S1(3)+S2(3)
C
      Q(0)=REAL( JVSS(5))
      Q(1)=REAL( JVSS(6))
      Q(2)=AIMAG(JVSS(6))
      Q(3)=AIMAG(JVSS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
C .........CHECK........................................................
          IF (ABS(JVSS(5))+ABS(JVSS(6)).EQ.0.)
     &    WRITE(1,*) 
     &    ' HELAS-ERROR : JVSS(6) in JVSSXX has zero momentum'
          IF ((VWIDTH.EQ.0.).AND.(Q2.EQ.VM2)) THEN
          WRITE(1,*) ' HELAS-ERROR : JVSS(6) in JVSSXX is on VMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          JVSS(1)=CMPLX(0.)
          JVSS(2)=CMPLX(0.)
          JVSS(3)=CMPLX(0.)
          JVSS(4)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G*S1(1)*S2(1)/CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above DG.
C      DG=G*S1(1)*S2(1)/CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      VK=(Q(0)*VC(1)-Q(1)*VC(2)-Q(2)*VC(3)-Q(3)*VC(4))/VM2
C
      JVSS(1) = DG*(VC(1)-VK*Q(0))
      JVSS(2) = DG*(VC(2)-VK*Q(1))
      JVSS(3) = DG*(VC(3)-VK*Q(2))
      JVSS(4) = DG*(VC(4)-VK*Q(3))
C
      RETURN
C
  10  DG= G*S1(1)*S2(1)/Q2
C
      JVSS(1) = DG*VC(1)
      JVSS(2) = DG*VC(2)
      JVSS(3) = DG*VC(3)
      JVSS(4) = DG*VC(4)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HVVSXX(V1,V2,SC,G,SMASS,SWIDTH , HVVS)
C
C This subroutine computes an off-shell scalar current of the vector-   
C vector-scalar-scalar coupling.                                        
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex SC(3)          : input  scalar                        S 
C       real    G              : coupling constant                 GVVHH
C       real    SMASS          : mass  of OUTPUT scalar S'              
C       real    SWIDTH         : width of OUTPUT scalar S'              
C                                                                       
C OUTPUT:                                                               
C       complex HVVS(3)        : scalar current            J(S':V1,V2,S)
C
      COMPLEX V1(6),V2(6),SC(3),HVVS(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
C .........CHECK........................................................
          IF (ABS(V1(1))+ABS(V1(2))+ABS(V1(3))+ABS(V1(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V1(6) in HVVSXX is zero vector'
          IF (ABS(V1(5))+ABS(V1(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V1(6) in HVVSXX has zero momentum'
          IF (ABS(V2(1))+ABS(V2(2))+ABS(V2(3))+ABS(V2(4)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : V2(6) in HVVSXX is zero vector'
          IF (ABS(V2(5))+ABS(V2(6)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : V2(6) in HVVSXX has zero momentum'
          IF (ABS(SC(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : SC(3) in HVVSXX is zero scalar'
          IF (ABS(SC(2))+ABS(SC(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : SC(3) in HVVSXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in HVVSXX is zero coupling'
          IF (SMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SMASS in HVVSXX is negative'
          WRITE(1,*) '               SMASS = ',SMASS
          ENDIF
          IF (SWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SWIDTH in HVVSXX is negative'
          WRITE(1,*) '               SWIDTH = ',SWIDTH
          ENDIF
C .........CHECK........................................................
C
      HVVS(2) = V1(5)+V2(5)+SC(2)
      HVVS(3) = V1(6)+V2(6)+SC(3)
C
      Q(0)=REAL( HVVS(2))
      Q(1)=REAL( HVVS(3))
      Q(2)=AIMAG(HVVS(3))
      Q(3)=AIMAG(HVVS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
C .........CHECK........................................................
          IF (ABS(HVVS(2))+ABS(HVVS(3)).EQ.0.)
     &    WRITE(1,*)
     &    ' HELAS-ERROR : HVVS(3) in HVVSXX has zero momentum'
          IF ((SWIDTH.EQ.0.).AND.(Q2.EQ.SMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : HVVS(3) in HVVSXX is on SMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          HVVS(1)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HVVS(1) = DG * SC(1)
     &         *(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE SSSSXX(S1,S2,S3,S4,G , VERTEX)
C
C This subroutine computes an amplitude of the four-scalar coupling.    
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       complex S4(3)          : fourth scalar                        S4
C       real    G              : coupling constant                 GHHHH
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude            Gamma(S1,S2,S3,S4)
C
      COMPLEX S1(3),S2(3),S3(3),S4(3),VERTEX
      REAL    G
         REAL P0,P1,P2,P3,Q0,Q1,Q2,Q3,R0,R1,R2,R3,O0,O1,O2,O3,PM
C
C .........CHECK........................................................
          P0=REAL( S1(2))
          P1=REAL( S1(3))
          P2=AIMAG(S1(3))
          P3=AIMAG(S1(2))
          Q0=REAL( S2(2))
          Q1=REAL( S2(3))
          Q2=AIMAG(S2(3))
          Q3=AIMAG(S2(2))
          R0=REAL( S3(2))
          R1=REAL( S3(3))
          R2=AIMAG(S3(3))
          R3=AIMAG(S3(2))
          O0=REAL( S4(2))
          O1=REAL( S4(3))
          O2=AIMAG(S4(3))
          O3=AIMAG(S4(2))
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in SSSSXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in SSSSXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in SSSSXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in SSSSXX has zero momentum'
          IF (ABS(S3(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S3(3) in SSSSXX is zero scalar'
          IF (ABS(S3(2))+ABS(S3(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S3(3) in SSSSXX has zero momentum'
          IF (ABS(S4(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S4(3) in SSSSXX is zero scalar'
          IF (ABS(S4(2))+ABS(S4(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S4(3) in SSSSXX has zero momentum'
          PM=MAX( ABS(P0),ABS(Q0),ABS(R0),ABS(O0),
     &            ABS(P1),ABS(Q1),ABS(R1),ABS(O1),
     &            ABS(P2),ABS(Q2),ABS(R2),ABS(O2),
     &            ABS(P3),ABS(Q3),ABS(R3),ABS(O3) )
          IF (( ABS(S1(2)+S2(2)+S3(2)+S4(2))
     &         +ABS(S1(3)+S2(3)+S3(3)+S4(3))).GE.PM*4.E-5) THEN
          WRITE(1,*) ' HELAS-ERROR : S1(3),S2(3),S3(3),S4(3) in SSSSXX'
          WRITE(1,*) '                        have not balanced momenta'
          ENDIF
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in SSSSXX is zero coupling'
C .........CHECK........................................................
C
      VERTEX = G*S1(1)*S2(1)*S3(1)*S4(1)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HSSSXX(S1,S2,S3,G,SMASS,SWIDTH , HSSS)
C
C This subroutine computes an off-shell scalar current from the four-   
C scalar coupling.                                                      
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       real    G              : coupling constant                 GHHHH
C       real    SMASS          : mass  of OUTPUT scalar S'              
C       real    SWIDTH         : width of OUTPUT scalar S'              
C                                                                       
C OUTPUT:                                                               
C       complex HSSS(3)        : scalar current           J(S':S1,S2,S3)
C
      COMPLEX S1(3),S2(3),S3(3),HSSS(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
C .........CHECK........................................................
          IF (ABS(S1(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S1(3) in HSSSXX is zero scalar'
          IF (ABS(S1(2))+ABS(S1(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S1(3) in HSSSXX has zero momentum'
          IF (ABS(S2(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S2(3) in HSSSXX is zero scalar'
          IF (ABS(S2(2))+ABS(S2(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S2(3) in HSSSXX has zero momentum'
          IF (ABS(S3(1)).EQ.0.)
     &    WRITE(1,*) ' HELAS-warn  : S3(3) in HSSSXX is zero scalar'
          IF (ABS(S3(2))+ABS(S3(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : S3(3) in HSSSXX has zero momentum'
          IF (G.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : G in HSSSXX is zero coupling'
          IF (SMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SMASS in HSSSXX is negative'
          WRITE(1,*) '               SMASS = ',SMASS
          ENDIF
          IF (SWIDTH.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : SWIDTH in HSSSXX is negative'
          WRITE(1,*) '               SWIDTH = ',SWIDTH
          ENDIF
C .........CHECK........................................................
C
      HSSS(2) = S1(2)+S2(2)+S3(2)
      HSSS(3) = S1(3)+S2(3)+S3(3)
C
      Q(0)=REAL( HSSS(2))
      Q(1)=REAL( HSSS(3))
      Q(2)=AIMAG(HSSS(3))
      Q(3)=AIMAG(HSSS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
C .........CHECK........................................................
          IF (ABS(HSSS(2))+ABS(HSSS(3)).EQ.0.)
     &    WRITE(1,*)
     &    ' HELAS-ERROR : HSSS(3) in HSSSXX has zero momentum'
          IF ((SWIDTH.EQ.0.).AND.(Q2.EQ.SMASS**2)) THEN
          WRITE(1,*) ' HELAS-ERROR : HSSS(3) in HSSSXX is on SMASS pole'
          WRITE(1,*) '               Q     = ',Q(0),Q(1),Q(2),Q(3)
          WRITE(1,*) '               abs(Q)= ',SQRT(ABS(Q2))
          HSSS(1)=CMPLX(0.)
          RETURN
          ENDIF
C .........CHECK........................................................
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HSSS(1) = DG * S1(1)*S2(1)*S3(1)
C
      RETURN
      END
C
C ======================================================================
C
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
      COMPLEX EAI(6),PHS
      REAL    EB,EA,SHLF,CHLF,PHI,ME,ALPHA,GAL,RNHE,X,C,S,D,COEFF,
     &        XNNP,XNNM,SNP,CSP
      INTEGER NHE,NHA,NN
         REAL ZERO
C
C .........CHECK........................................................
          IF (EB.LE.0.)
     &    WRITE(1,*) ' HELAS-ERROR : EB in EAIXXX is not positive'
          IF (EA.LE.0.)
     &    WRITE(1,*) ' HELAS-ERROR : EA in EAIXXX is not positive'
          IF (EA.GT.EB) THEN
          WRITE(1,*) ' HELAS-ERROR : EA in EAIXXX is greater than EB'
          WRITE(1,*) '               EB = ',EB,' : EA = ',EA
          ENDIF
          IF ((SHLF.LT.0.).OR.(SHLF.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : SHLF in EAIXXX is improper'
          WRITE(1,*) '               SHLF = ',SHLF
          ENDIF
          IF ((CHLF.LT.0.).OR.(CHLF.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : CHLF in EAIXXX is improper'
          WRITE(1,*) '               CHLF = ',CHLF
          ENDIF
          ZERO=ABS(SHLF**2+CHLF**2-1.0)
          IF (ZERO.GT.1.E-5) THEN
          WRITE(1,*) 
     &    ' HELAS-ERROR : SHLF and CHLF in EAIXXX are inconsistent'
          WRITE(1,*) '               SHLF,CHLF = ',SHLF,CHLF
          ENDIF
          IF ((PHI.LT.0.).OR.(PHI*.5.GT.3.14160)) THEN
          WRITE(1,*)
     &    ' HELAS-warn  : PHI in EAIXXX does not lie on 0.0 thru 2.0*PI'
          WRITE(1,*) '               PHI = ',PHI
          ENDIF
          IF (ABS(NHE).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHE in EAIXXX is not -1,1'
          WRITE(1,*) '               NHE = ',NHE
          ENDIF
          IF (ABS(NHA).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHA in EAIXXX is not -1,1'
          WRITE(1,*) '               NHA = ',NHA
          ENDIF
          IF (EB.LT.1.) THEN
          WRITE(1,*) 
     &    ' HELAS-warn  : Use of EAIXXX is not appropriate: EB too low'
          WRITE(1,*) '               EB = ',EB
          ENDIF
C .........CHECK........................................................
C
      ME   =REAL(0.51099906D-3)
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*REAL(3.14159265D0))
C
      NN=NHA*NHE
      RNHE=REAL(NHE)
      X=EA/EB
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
      D=-1./(EA*EB*(4.*SHLF**2+(ME/EB)**2*C))
      COEFF=-REAL(NN)*GAL*SQRT(EB)*D
      XNNP=X*REAL(1+NN)
      XNNM=X*REAL(1-NN)
      SNP=SIN(PHI)
      CSP=COS(PHI)
      PHS=CMPLX( CSP , RNHE*SNP )
C
      EAI((5-3*NHE)/2) = -RNHE*COEFF*ME*S*(1.+XNNP*.5)
      EAI((5-NHE)/2)   =  XNNP*COEFF*ME*CHLF**2*PHS
      EAI((5+NHE)/2)   =  RNHE*COEFF*EB*S*(-2.+XNNM)
      EAI((5+3*NHE)/2) =  XNNM*COEFF*EB*SHLF**2*PHS*2.
C
      EAI(5) =  EB*CMPLX( 1.-X , 1.-X*C )
      EAI(6) = -EB*X*S*CMPLX( CSP , SNP )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
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
      COMPLEX EAO(6),PHS
      REAL    EB,EA,SHLF,CHLF,PHI,ME,ALPHA,GAL,RNHE,X,C,S,D,COEFF,
     &        XNNP,XNNM,SNP,CSP
      INTEGER NHE,NHA,NN
         REAL ZERO
C
C .........CHECK........................................................
          IF (EB.LE.0.)
     &    WRITE(1,*) ' HELAS-ERROR : EB in EAOXXX is not positive'
          IF (EA.LE.0.)
     &    WRITE(1,*) ' HELAS-ERROR : EA in EAOXXX is not positive'
          IF (EA.GT.EB) THEN
          WRITE(1,*) ' HELAS-ERROR : EA in EAOXXX is greater than EB'
          WRITE(1,*) '               EB = ',EB,' : EA = ',EA
          ENDIF
          IF ((SHLF.LT.0.).OR.(SHLF.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : SHLF in EAOXXX is improper'
          WRITE(1,*) '               SHLF = ',SHLF
          ENDIF
          IF ((CHLF.LT.0.).OR.(CHLF.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : CHLF in EAOXXX is improper'
          WRITE(1,*) '               CHLF = ',CHLF
          ENDIF
          ZERO=ABS(SHLF**2+CHLF**2-1.0)
          IF (ZERO.GT.1.E-5) THEN
          WRITE(1,*) 
     &    ' HELAS-ERROR : SHLF and CHLF in EAOXXX are inconsistent'
          WRITE(1,*) '               SHLF,CHLF = ',SHLF,CHLF
          ENDIF
          IF ((PHI.LT.0.).OR.(PHI*.5.GT.3.14160)) THEN
          WRITE(1,*)
     &    ' HELAS-warn  : PHI in EAOXXX does not lie on 0.0 thru 2.0*PI'
          WRITE(1,*) '               PHI = ',PHI
          ENDIF
          IF (ABS(NHE).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHE in EAOXXX is not -1,1'
          WRITE(1,*) '               NHE = ',NHE
          ENDIF
          IF (ABS(NHA).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHA in EAOXXX is not -1,1'
          WRITE(1,*) '               NHA = ',NHA
          ENDIF
          IF (EB.LT.1.) THEN
          WRITE(1,*) 
     &    ' HELAS-warn  : Use of EAOXXX is not appropriate: EB too low'
          WRITE(1,*) '               EB = ',EB
          ENDIF
C .........CHECK........................................................
C
      ME   =REAL(0.51099906D-3)
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*REAL(3.14159265D0))
C
      NN=NHA*NHE
      RNHE=REAL(NHE)
      X=EA/EB
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
      D=-1./(EA*EB*(4.*CHLF**2-(ME/EB)**2*C))
      COEFF=REAL(NN)*GAL*SQRT(EB)*D
      XNNP=X*REAL(1+NN)
      XNNM=X*REAL(1-NN)
      SNP=SIN(PHI)
      CSP=COS(PHI)
      PHS=CMPLX( CSP ,-RNHE*SNP )
C
      EAO((5-3*NHE)/2) =              COEFF*ME*S*(1.+XNNP*.5)
      EAO((5-NHE)/2)   = RNHE*XNNP    *COEFF*ME*SHLF**2*PHS
      EAO((5+NHE)/2)   =              COEFF*EB*S*(-2.+XNNM)
      EAO((5+3*NHE)/2) = REAL(NHA-NHE)*COEFF*EB*X*CHLF**2*PHS*2.
C
      EAO(5) = EB*CMPLX( X-1. , X*C+1. )
      EAO(6) = EB*X*S*CMPLX( CSP , SNP )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
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
      COMPLEX JEE(6),COEFF
      REAL    CS(2),EB,EF,SHLF,CHLF,PHI,ME,ALPHA,GAL,HI,SF,SFH,X,ME2,Q2,
     &        RFP,RFM,SNP,CSP,RXC,C,S
      INTEGER NHB,NHF,NSF
         REAL ZERO
C
C .........CHECK........................................................
          IF (EB.LE.0.)
     &    WRITE(1,*) ' HELAS-ERROR : EB in JEEXXX is not positive'
          IF (EF.LE.0.)
     &    WRITE(1,*) ' HELAS-ERROR : EF in JEEXXX is not positive'
          IF (EF.GT.EB) THEN
          WRITE(1,*) ' HELAS-ERROR : EF in JEEXXX is greater than EB'
          WRITE(1,*) '               EB = ',EB,' : EF = ',EF
          ENDIF
          IF ((SHLF.LT.0.).OR.(SHLF.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : SHLF in JEEXXX is improper'
          WRITE(1,*) '               SHLF = ',SHLF
          ENDIF
          IF ((CHLF.LT.0.).OR.(CHLF.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : CHLF in JEEXXX is improper'
          WRITE(1,*) '               CHLF = ',CHLF
          ENDIF
          ZERO=ABS(SHLF**2+CHLF**2-1.0)
          IF (ZERO.GT.1.E-5) THEN
          WRITE(1,*) 
     &    ' HELAS-ERROR : SHLF and CHLF in JEEXXX are inconsistent'
          WRITE(1,*) '               SHLF,CHLF = ',SHLF,CHLF
          ENDIF
          IF ((PHI.LT.0.).OR.(PHI*.5.GT.3.14160)) THEN
          WRITE(1,*)
     &    ' HELAS-warn  : PHI in JEEXXX does not lie on 0.0 thru 2.0*PI'
          WRITE(1,*) '               PHI = ',PHI
          ENDIF
          IF (ABS(NHB).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHB in JEEXXX is not -1,1'
          WRITE(1,*) '               NHB = ',NHB
          ENDIF
          IF (ABS(NHF).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NHF in JEEXXX is not -1,1'
          WRITE(1,*) '               NHF = ',NHF
          ENDIF
          IF (ABS(NSF).NE.1) THEN
          WRITE(1,*) ' HELAS-ERROR : NSF in JEEXXX is not -1,1'
          WRITE(1,*) '               NSF = ',NSF
          ENDIF
          IF (EB.LT.1.) THEN
          WRITE(1,*) 
     &    ' HELAS-warn  : Use of JEEXXX is not appropriate: EB too low'
          WRITE(1,*) '               EB = ',EB
          ENDIF
C .........CHECK........................................................
C
      ME   =REAL(0.51099906D-3)
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*REAL(3.14159265D0))
C
      HI =REAL(NHB)
      SF =REAL(NSF)
      SFH=REAL(NHB*NSF)
      CS((3+NSF)/2)=SHLF
      CS((3-NSF)/2)=CHLF
C CS(1)=CHLF and CS(2)=SHLF for electron
C CS(1)=SHLF and CS(2)=CHLF for positron
      X=EF/EB
      ME2=ME**2
      Q2=-4.*CS(2)**2*(EF*EB-ME2)
     &   +SF*(1.-X)**2/X*(SHLF+CHLF)*(SHLF-CHLF)*ME2
      RFP=REAL(1+NSF)
      RFM=REAL(1-NSF)
      SNP=SIN(PHI)
      CSP=COS(PHI)
C
      IF (NHB.EQ.NHF) THEN
         RXC=2.*X/(1.-X)*CS(1)**2
         COEFF= GAL*2.*EB*SQRT(X)*CS(2)/Q2
     &         *(CMPLX( RFP )-RFM*CMPLX( CSP ,-SNP*HI ))*.5
         JEE(1) =  CMPLX( 0. )
         JEE(2) =  COEFF*CMPLX( (1.+RXC)*CSP ,-SFH*SNP )
         JEE(3) =  COEFF*CMPLX( (1.+RXC)*SNP , SFH*CSP )
         JEE(4) =  COEFF*(-SF*RXC/CS(1)*CS(2))
      ELSE
         COEFF= GAL*ME/Q2/SQRT(X)
     &         *(CMPLX( RFP )+RFM*CMPLX( CSP , SNP*HI ))*.5*HI
         JEE(1) = -COEFF*(1.+X)*CS(2)*CMPLX( CSP , SFH*SNP )
         JEE(2) =  COEFF*(1.-X)*CS(1)
         JEE(3) =  JEE(2)*CMPLX( 0. , SFH )
         JEE(4) =  JEE(1)*SF*(1.-X)/(1.+X)
      ENDIF
C
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
C
      JEE(5) = -EB*CMPLX( 1.-X , SF-X*C )
      JEE(6) =  EB*X*S*CMPLX( CSP , SNP )
C
      RETURN          
      END
C
C **********************************************************************
C
      SUBROUTINE MOMNTX(ENERGY,MASS,COSTH,PHI , P)
C
C This subroutine sets up a four-momentum from the four inputs.         
C                                                                       
C INPUT:                                                                
C       real    ENERGY         : energy                                 
C       real    MASS           : mass                                   
C       real    COSTH          : cos(theta)                             
C       real    PHI            : azimuthal angle                        
C                                                                       
C OUTPUT:                                                               
C       real    P(0:3)         : four-momentum                          
C
      REAL    P(0:3),ENERGY,MASS,COSTH,PHI,PP,SINTH
C
C .........CHECK........................................................
          IF (ENERGY.LT.MASS) THEN
          WRITE(1,*) ' HELAS-ERROR : ENERGY in MOMNTX is less than MASS'
          WRITE(1,*) '               ENERGY = ',ENERGY,' : MASS = ',MASS
          ENDIF
          IF (MASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : MASS in MOMNTX is negative'
          WRITE(1,*) '               MASS = ',MASS
          ENDIF
          IF (ABS(COSTH).GT.1.) THEN
          WRITE(1,*) ' HELAS-ERROR : COSTH in MOMNTX is improper'
          WRITE(1,*) '               COSTH = ',COSTH
          ENDIF
          IF ((PHI.LT.0.).OR.(PHI*.5.GT.3.14160)) THEN
          WRITE(1,*)
     &    ' HELAS-warn  : PHI in MOMNTX does not lie on 0.0 thru 2.0*PI'
          WRITE(1,*) '               PHI = ',PHI
          ENDIF
C .........CHECK........................................................
C
      P(0) = ENERGY
      IF (ENERGY.EQ.MASS) THEN
         P(1) = 0.
         P(2) = 0.
         P(3) = 0.
      ELSE
         PP=SQRT((ENERGY-MASS)*(ENERGY+MASS))
         SINTH=SQRT((1.-COSTH)*(1.+COSTH))
         P(3) = PP*COSTH
         IF (PHI.EQ.0.) THEN
            P(1) = PP*SINTH
            P(2) = 0.
         ELSE
            P(1) = PP*SINTH*COS(PHI)
            P(2) = PP*SINTH*SIN(PHI)
         ENDIF
      ENDIF
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE MOM2CX(ESUM,MASS1,MASS2,COSTH1,PHI1 , P1,P2)
C
C This subroutine sets up two four-momenta in the two particle rest     
C frame.                                                                
C                                                                       
C INPUT:                                                                
C       real    ESUM           : energy sum of particle 1 and 2         
C       real    MASS1          : mass            of particle 1          
C       real    MASS2          : mass            of particle 2          
C       real    COSTH1         : cos(theta)      of particle 1          
C       real    PHI1           : azimuthal angle of particle 1          
C                                                                       
C OUTPUT:                                                               
C       real    P1(0:3)        : four-momentum of particle 1            
C       real    P2(0:3)        : four-momentum of particle 2            
C
      REAL    P1(0:3),P2(0:3),
     &        ESUM,MASS1,MASS2,COSTH1,PHI1,MD2,ED,PP,SINTH1
C
C .........CHECK........................................................
          IF (ESUM.LT.MASS1+MASS2) THEN
          WRITE(1,*) 
     &    ' HELAS-ERROR : ESUM in MOM2CX is less than MASS1+MASS2'
          WRITE(1,*) 
     &    '               ESUM = ',ESUM,' : MASS1+MASS2 = ',MASS1,MASS2
          ENDIF
          IF (MASS1.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : MASS1 in MOM2CX is negative'
          WRITE(1,*) '               MASS1 = ',MASS1
          ENDIF
          IF (MASS2.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : MASS2 in MOM2CX is negative'
          WRITE(1,*) '               MASS2 = ',MASS2
          ENDIF
          IF (ABS(COSTH1).GT.1.) THEN
          WRITE(1,*) ' HELAS-ERROR : COSTH1 in MOM2CX is improper'
          WRITE(1,*) '               COSTH1 = ',COSTH1
          ENDIF
          IF ((PHI1.LT.0.).OR.(PHI1*.5.GT.3.14160)) THEN
          WRITE(1,*)
     &  ' HELAS-warn  : PHI1 in MOM2CX does not lie on 0.0 thru 2.0*PI'
          WRITE(1,*) '               PHI1 = ',PHI1
          ENDIF
C .........CHECK........................................................
C
      MD2=(MASS1-MASS2)*(MASS1+MASS2)
      ED=MD2/ESUM
      IF (MASS1*MASS2.EQ.0.) THEN
      PP=(ESUM-ABS(ED))*0.5
C
      ELSE
      PP=SQRT((MD2/ESUM)**2-2.0*(MASS1**2+MASS2**2)+ESUM**2)*0.5
      ENDIF
      SINTH1=SQRT((1.0-COSTH1)*(1.0+COSTH1))
C
      P1(0) = MAX((ESUM+ED)*0.5,0.)
      P1(1) = PP*SINTH1*COS(PHI1)
      P1(2) = PP*SINTH1*SIN(PHI1)
      P1(3) = PP*COSTH1
C
      P2(0) = MAX((ESUM-ED)*0.5,0.)
      P2(1) = -P1(1)
      P2(2) = -P1(2)
      P2(3) = -P1(3)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE BOOSTX(P,Q , PBOOST)
C
C This subroutine performs the Lorentz boost of a four-momentum.  The   
C momentum P is assumed to be given in the rest frame of Q.  PBOOST is  
C the momentum P boosted to the frame in which Q is given.  Q must be a 
C timelike momentum.                                                    
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum P in the Q rest  frame   
C       real    Q(0:3)         : four-momentum Q in the boosted frame   
C                                                                       
C OUTPUT:                                                               
C       real    PBOOST(0:3)    : four-momentum P in the boosted frame   
C
      REAL    P(0:3),Q(0:3),PBOOST(0:3),PQ,QQ,M,LF
         REAL PP
C
      QQ=Q(1)**2+Q(2)**2+Q(3)**2
C
C .........CHECK........................................................
          IF (ABS(P(0))+ABS(P(1))+ABS(P(2))+ABS(P(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : P(0:3) in BOOSTX is zero momentum'
          IF (ABS(Q(0))+QQ.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : Q(0:3) in BOOSTX is zero momentum'
          IF (P(0).LE.0.) THEN
          WRITE(1,*)
     &    ' HELAS-warn  : P(0:3) in BOOSTX has not positive energy'
          WRITE(1,*) '               P(0) = ',P(0)
          ENDIF
          IF (Q(0).LE.0.) THEN
          WRITE(1,*)
     &    ' HELAS-ERROR : Q(0:3) in BOOSTX has not positive energy'
          WRITE(1,*) '               Q(0) = ',Q(0)
          ENDIF
          PP=P(0)**2-P(1)**2-P(2)**2-P(3)**2
          IF (PP.LT.0.) THEN
          WRITE(1,*) ' HELAS-warn  : P(0:3) in BOOSTX is spacelike'
          WRITE(1,*) ' P**2 = ',PP
          ENDIF
          IF (Q(0)**2-QQ.LE.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : Q(0:3) in BOOSTX is not timelike'
          WRITE(1,*) '               Q**2 = ',Q(0)**2-QQ
          ENDIF
          IF (QQ.EQ.0.)
     &    WRITE(1,*) 
     &    ' HELAS-warn  : Q(0:3) in BOOSTX has zero spacial components'
C .........CHECK........................................................
C
      IF (QQ.NE.0.) THEN
         PQ=P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3)
         M=SQRT(Q(0)**2-QQ)
         LF=((Q(0)-M)*PQ/QQ+P(0))/M
         PBOOST(0) = (P(0)*Q(0)+PQ)/M
         PBOOST(1) =  P(1)+Q(1)*LF
         PBOOST(2) =  P(2)+Q(2)*LF
         PBOOST(3) =  P(3)+Q(3)*LF
      ELSE
         PBOOST(0)=P(0)
         PBOOST(1)=P(1)
         PBOOST(2)=P(2)
         PBOOST(3)=P(3)
      ENDIF
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE ROTXXX(P,Q , PROT)
C
C This subroutine performs the spacial rotation of a four-momentum.     
C The momentum P is assumed to be given in the frame where the spacial  
C component of Q points the positive z-axis.  PROT is the momentum P    
C rotated to the frame where Q is given.                                
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum P in Q(1)=Q(2)=0 frame   
C       real    Q(0:3)         : four-momentum Q in the rotated frame   
C                                                                       
C OUTPUT:                                                               
C       real    PROT(0:3)      : four-momentum P in the rotated frame   
C
      REAL    P(0:3),Q(0:3),PROT(0:3),QT2,QT,PSGN,QQ,P1
C
      PROT(0) = P(0)
C
      QT2=Q(1)**2+Q(2)**2
C
C .........CHECK........................................................
          IF (ABS(P(0))+ABS(P(1))+ABS(P(2))+ABS(P(3)).EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : P(0:3) in ROTXXX is zero momentum'
          IF (ABS(Q(0))+ABS(Q(3))+QT2.EQ.0.)
     &    WRITE(1,*) ' HELAS-ERROR : Q(0:3) in ROTXXX is zero momentum'
          IF (QT2+ABS(Q(3)).EQ.0.)
     &    WRITE(1,*) 
     &    ' HELAS-warn  : Q(0:3) in ROTXXX has zero spacial momentum'
C .........CHECK........................................................
C
      IF (QT2.EQ.0.) THEN
          IF (Q(3).EQ.0.) THEN
             PROT(1) = P(1)
             PROT(2) = P(2)
             PROT(3) = P(3)
          ELSE
             PSGN=SIGN(1.,Q(3))
             PROT(1) = P(1)*PSGN
             PROT(2) = P(2)*PSGN
             PROT(3) = P(3)*PSGN
          ENDIF
      ELSE
          QQ=SQRT(QT2+Q(3)**2)
          QT=SQRT(QT2)
          P1=P(1)
          PROT(1) = Q(1)*Q(3)/QQ/QT*P1 -Q(2)/QT*P(2) +Q(1)/QQ*P(3)
          PROT(2) = Q(2)*Q(3)/QQ/QT*P1 +Q(1)/QT*P(2) +Q(2)/QQ*P(3)
          PROT(3) =          -QT/QQ*P1               +Q(3)/QQ*P(3)
      ENDIF
      RETURN
      END
C
C **********************************************************************
C
      SUBROUTINE COUP1X(SW2 , GW,GWWA,GWWZ)
C
C This subroutine sets up the coupling constants of the gauge bosons in 
C the STANDARD MODEL.                                                   
C                                                                       
C INPUT:                                                                
C       real    SW2            : square of sine of the weak angle       
C                                                                       
C OUTPUT:                                                               
C       real    GW             : weak coupling constant                 
C       real    GWWA           : dimensionLESS coupling of W-,W+,A      
C       real    GWWZ           : dimensionLESS coupling of W-,W+,Z      
C
      REAL    SW2,GW,GWWA,GWWZ,ALPHA,FOURPI,EE,SW,CW
C
C .........CHECK........................................................
          IF ((SW2.LT.0.).OR.(SW2.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : SW2 in COUP1 is improper'
          WRITE(1,*) '               SW2 = ',SW2
          ENDIF
C .........CHECK........................................................
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EE=SQRT(ALPHA*FOURPI)
      SW=SQRT(SW2)
      CW=SQRT(1.-SW2)
C
      GW    =  EE/SW
      GWWA  =  EE
      GWWZ  =  EE*CW/SW
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE COUP2X(SW2 , GAL,GAU,GAD,GWF,GZN,GZL,GZU,GZD,G1)
C
C This subroutine sets up the coupling constants for the fermion-       
C fermion-vector vertices in the STANDARD MODEL.  The array of the      
C couplings specifies the chirality of the flowing-IN fermion.  G??(1)  
C denotes a left-handed coupling, and G??(2) a right-handed coupling.   
C                                                                       
C INPUT:                                                                
C       real    SW2            : square of sine of the weak angle       
C                                                                       
C OUTPUT:                                                               
C       real    GAL(2)         : coupling with A of charged leptons     
C       real    GAU(2)         : coupling with A of up-type quarks      
C       real    GAD(2)         : coupling with A of down-type quarks    
C       real    GWF(2)         : coupling with W-,W+ of fermions        
C       real    GZN(2)         : coupling with Z of neutrinos           
C       real    GZL(2)         : coupling with Z of charged leptons     
C       real    GZU(2)         : coupling with Z of up-type quarks      
C       real    GZD(2)         : coupling with Z of down-type quarks    
C       real    G1(2)          : unit coupling of fermions              
C
      REAL GAL(2),GAU(2),GAD(2),GWF(2),GZN(2),GZL(2),GZU(2),GZD(2),
     &     G1(2),SW2,ALPHA,FOURPI,EE,SW,CW,EZ,EY
C
C .........CHECK........................................................
          IF ((SW2.LT.0.).OR.(SW2.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : SW2 in COUP2X is improper'
          WRITE(1,*) '               SW2 = ',SW2
          ENDIF
C .........CHECK........................................................
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EE=SQRT(ALPHA*FOURPI)
      SW=SQRT(SW2)
      CW=SQRT(1.-SW2)
      EZ=EE/(SW*CW)
      EY=EE*(SW/CW)
C
      GAL(1) =  EE
      GAL(2) =  EE
      GAU(1) = -EE*2./3.
      GAU(2) = -EE*2./3.
      GAD(1) =  EE   /3.
      GAD(2) =  EE   /3.
      GWF(1) = -EE/SQRT(2.*SW2)
      GWF(2) =  0.
      GZN(1) = -EZ*  0.5
      GZN(2) =  0.
      GZL(1) = -EZ*(-0.5+SW2)
      GZL(2) = -EY
      GZU(1) = -EZ*( 0.5-SW2*2./3.)
      GZU(2) =  EY*          2./3.
      GZD(1) = -EZ*(-0.5+SW2   /3.)
      GZD(2) = -EY             /3.
      G1(1)  =  1.
      G1(2)  =  1.
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE COUP3X(SW2,ZMASS,HMASS , 
     &                  GWWH,GZZH,GHHH,GWWHH,GZZHH,GHHHH)
C
C This subroutine sets up the coupling constants of the gauge bosons and
C Higgs boson in the STANDARD MODEL.                                    
C                                                                       
C INPUT:                                                                
C       real    SW2            : square of sine of the weak angle       
C       real    ZMASS          : mass of Z                              
C       real    HMASS          : mass of Higgs                          
C                                                                       
C OUTPUT:                                                               
C       real    GWWH           : dimensionFUL  coupling of W-,W+,H      
C       real    GZZH           : dimensionFUL  coupling of Z, Z, H      
C       real    GHHH           : dimensionFUL  coupling of H, H, H      
C       real    GWWHH          : dimensionFUL  coupling of W-,W+,H, H   
C       real    GZZHH          : dimensionFUL  coupling of Z, Z, H, H   
C       real    GHHHH          : dimensionLESS coupling of H, H, H, H   
C
      REAL    SW2,ZMASS,HMASS,GWWH,GZZH,GHHH,GWWHH,GZZHH,GHHHH,
     &        ALPHA,FOURPI,EE2,SC2,V
C
C .........CHECK........................................................
          IF ((SW2.LT.0.).OR.(SW2.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : SW2 in COUP3 is improper'
          WRITE(1,*) '               SW2 = ',SW2
          ENDIF
          IF (ZMASS.LE.0.0) THEN
          WRITE(1,*) ' HELAS-ERROR : ZMASS in COUP3 is not positive'
          WRITE(1,*) '               ZMASS = ',ZMASS
          ENDIF
          IF (HMASS.LT.0.0) THEN
          WRITE(1,*) ' HELAS-ERROR : HMASS in COUP3 is negative'
          WRITE(1,*) '               HMASS = ',HMASS
          ENDIF
C .........CHECK........................................................
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EE2=ALPHA*FOURPI
      SC2=SW2*(1.0-SW2)
      V=2.0*ZMASS*SQRT(SC2)/SQRT(EE2)
C
      GWWH  =   EE2/SW2*0.5*V
      GZZH  =   EE2/SC2*0.5*V
      GHHH  =  -HMASS**2/V*3.0
      GWWHH =   EE2/SW2*0.5
      GZZHH =   EE2/SC2*0.5
      GHHHH = -(HMASS/V)**2*3.0
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE COUP4X(SW2,ZMASS,FMASS , GCHF)
C
C This subroutine sets up the coupling constant for the fermion-fermion-
C Higgs vertex in the STANDARD MODEL.  The coupling is COMPLEX and the  
C array of the coupling specifies the chirality of the flowing-IN       
C fermion.  GCHF(1) denotes a left-handed coupling, and GCHF(2) a right-
C handed coupling.                                                      
C                                                                       
C INPUT:                                                                
C       real    SW2            : square of sine of the weak angle       
C       real    ZMASS          : Z       mass                           
C       real    FMASS          : fermion mass                           
C                                                                       
C OUTPUT:                                                               
C       complex GCHF(2)        : coupling of fermion and Higgs          
C
      COMPLEX GCHF(2)
      REAL    SW2,ZMASS,FMASS,ALPHA,FOURPI,EZ,G
C
C .........CHECK........................................................
          IF ((SW2.LT.0.).OR.(SW2.GT.1.)) THEN
          WRITE(1,*) ' HELAS-ERROR : SW2 in COUP4X is improper'
          WRITE(1,*) '               SW2 = ',SW2
          ENDIF
          IF (ZMASS.LE.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : ZMASS in COUP4X is not positive'
          WRITE(1,*) '               ZMASS = ',ZMASS
          ENDIF
          IF (FMASS.LT.0.) THEN
          WRITE(1,*) ' HELAS-ERROR : FMASS in COUP4X is negative'
          WRITE(1,*) '               FMASS = ',FMASS
          ENDIF
C .........CHECK........................................................
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EZ=SQRT(ALPHA*FOURPI)/SQRT(SW2*(1.-SW2))
      G=EZ*FMASS*0.5/ZMASS
C
      GCHF(1) = CMPLX( -G )
      GCHF(2) = CMPLX( -G )
C
      RETURN
      END
