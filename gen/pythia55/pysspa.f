 
C*********************************************************************
 
      SUBROUTINE PYSSPA(IPU1,IPU2)
 
C...Generates spacelike parton showers.
      IMPLICIT DOUBLE PRECISION(D)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/,/PYINT2/,/PYINT3/
      DIMENSION KFLS(4),IS(2),XS(2),ZS(2),Q2S(2),TEVCSV(2),TEVESV(2),
     &XFS(2,-25:25),XFA(-25:25),XFB(-25:25),XFN(-25:25),WTAPC(-25:25),
     &WTAPE(-25:25),WTSF(-25:25),THE2(2),ALAM(2),DQ2(3),DPC(3),DPD(4),
     &DPB(4),ROBO(5),MORE(2),XFGA(-25:25)
 
C...Read out basic information; set global Q^2 scale.
      IPUS1=IPU1
      IPUS2=IPU2
      ISUB=MINT(1)
      Q2MX=VINT(54)
      IF(ISET(ISUB).EQ.2) Q2MX=PARP(67)*VINT(54)
 
C...Initialize QCD evolution and check phase space.
      MCEV=0
      XEC0=2.*PARP(65)/VINT(1)
      ALAMS=PARU(111)
      PARU(111)=PARP(61)
      FQ2C=1.
      TCMX=0.
      IF(MINT(47).GE.2.AND.(MINT(47).NE.5.OR.MSTP(12).GE.1)) THEN
        MCEV=1
        IF(MSTP(64).EQ.1) FQ2C=PARP(63)
        IF(MSTP(64).EQ.2) FQ2C=PARP(64)
        TCMX=LOG(FQ2C*Q2MX/PARP(61)**2)
        IF(Q2MX.LT.MAX(PARP(62)**2,2.*PARP(61)**2).OR.TCMX.LT.0.2)
     &  MCEV=0
      ENDIF
 
C...Initialize QED evolution and check phase space.
      MEEV=0
      XEE=1E-6
      SPME=PMAS(11,1)**2
      TEMX=0.
      FWTE=10.
      IF(MINT(45).EQ.3.OR.MINT(46).EQ.3) THEN
        MEEV=1
        TEMX=LOG(Q2MX/SPME)
        IF(Q2MX.LE.PARP(68)**2.OR.TEMX.LT.0.2) MEEV=0
      ENDIF
      IF(MCEV.EQ.0.AND.MEEV.EQ.0) RETURN
 
C...Initial values: flavours, momenta, virtualities.
      NS=N
  100 N=NS
      DO 110 JT=1,2
      MORE(JT)=1
      KFLS(JT)=MINT(14+JT)
      KFLS(JT+2)=KFLS(JT)
      XS(JT)=VINT(40+JT)
      ZS(JT)=1.
      Q2S(JT)=Q2MX
      TEVCSV(JT)=TCMX
      ALAM(JT)=PARP(61)
      THE2(JT)=100.
      TEVESV(JT)=TEMX
      VINT(154+JT)=0.
      DO 110 KFL=-25,25
  110 XFS(JT,KFL)=XSFX(JT,KFL)
      DSH=VINT(44)
      IF(ISET(ISUB).GE.3.AND.ISET(ISUB).LE.5) DSH=VINT(26)*VINT(2)
 
C...Pick up leg with highest virtuality.
  120 N=N+1
      JT=1
      IF(N.GT.NS+1.AND.Q2S(2).GT.Q2S(1)) JT=2
      IF(MORE(JT).EQ.0) JT=3-JT
      KFLB=KFLS(JT)
      XB=XS(JT)
      DO 130 KFL=-25,25
  130 XFB(KFL)=XFS(JT,KFL)
      DSHR=2D0*SQRT(DSH)
      DSHZ=DSH/DBLE(ZS(JT))
 
C...Check if allowed to branch.
      MCEV=0
      IF(IABS(KFLB).LE.10.OR.KFLB.EQ.21) THEN
        MCEV=1
        XEC=MAX(XEC0,XB*(1./(1.-PARP(66))-1.))
        IF(XB.GE.1.-2.*XEC) MCEV=0
      ENDIF
      MEEV=0
      IF(MINT(44+JT).EQ.3) THEN
        MEEV=1
        IF(XB.GE.1.-2.*XEE) MEEV=0
        IF((IABS(KFLB).LE.10.OR.KFLB.EQ.21).AND.XB.GE.1.-2.*XEC) MEEV=0
C***Currently still kill joint QED/QCD shower and W shower.
        IF(IABS(KFLB).LE.10.OR.KFLB.EQ.21.OR.IABS(KFLB).EQ.24) THEN
          MCEV=0
          MEEV=0
        ENDIF
      ENDIF
      IF(MCEV.EQ.0.AND.MEEV.EQ.0) THEN
        Q2B=0.
        GOTO 220
      ENDIF
 
C...Maximum Q2 with or without Q2 ordering. Effective Lambda and n_f.
      Q2B=Q2S(JT)
      TEVCB=TEVCSV(JT)
      TEVEB=TEVESV(JT)
      IF(MSTP(62).LE.1) THEN
        Q2B=0.5*(1./ZS(JT)+1.)*Q2S(JT)+0.5*(1./ZS(JT)-1.)*(Q2S(3-JT)-
     &  SNGL(DSH)+SQRT((SNGL(DSH)+Q2S(1)+Q2S(2))**2+8.*Q2S(1)*Q2S(2)*
     &  ZS(JT)/(1.-ZS(JT))))
        IF(MCEV.EQ.1) TEVCB=LOG(FQ2C*Q2B/ALAM(JT)**2)
        IF(MEEV.EQ.1) TEVEB=LOG(Q2B/SPME)
      ENDIF
      IF(MCEV.EQ.1) THEN
        ALSDUM=ULALPS(FQ2C*Q2B)
        TEVCB=TEVCB+2.*LOG(ALAM(JT)/PARU(117))
        ALAM(JT)=PARU(117)
        B0=(33.-2.*MSTU(118))/6.
      ENDIF
      TEVCBS=TEVCB
      TEVEBS=TEVEB
 
C...Calculate Altarelli-Parisi and structure function weights.
      DO 140 KFL=-25,25
      WTAPC(KFL)=0.
      WTAPE(KFL)=0.
  140 WTSF(KFL)=0.
C...q -> q, g -> q, gamma -> q.
      IF(IABS(KFLB).LE.10) THEN
        WTAPC(KFLB)=(8./3.)*LOG((1.-XEC-XB)*(XB+XEC)/(XEC*(1.-XEC)))
        WTAPC(21)=0.5*(XB/(XB+XEC)-XB/(1.-XEC))
        IF(MEEV.EQ.1) THEN
          CALL PYSTFU(22,XB,Q2B,XFGA)
          WTAPE(22)=(PARU(2)/PARU(101))*XFGA(KFLB)*
     &    (XB/(XB+XEC)-XB/(1.-XEC))
          XFGAS=XFGA(KFLB)
        ENDIF
C...f -> f, gamma -> f.
      ELSEIF(IABS(KFLB).LE.20) THEN
        WTAPF1=LOG((1.-XEE-XB)*(XB+XEE)/(XEE*(1.-XEE)))
        WTAPF2=LOG((1.-XEE-XB)*(1.-XEE)/(XEE*(XB+XEE)))
        WTAPE(KFLB)=2.*(WTAPF1+WTAPF2)
        WTAPE(22)=XB/(XB+XEE)-XB/(1.-XEE)
C...f -> g, g -> g, gamma -> g.
      ELSEIF(KFLB.EQ.21) THEN
        WTAPQ=(16./3.)*(SQRT((1.-XEC)/XB)-SQRT((XB+XEC)/XB))
        DO 150 KFL=1,MSTP(54)
        WTAPC(KFL)=WTAPQ
  150   WTAPC(-KFL)=WTAPQ
        WTAPC(21)=6.*LOG((1.-XEC-XB)/XEC)
        IF(MEEV.EQ.1) THEN
          CALL PYSTFU(22,XB,Q2B,XFGA)
          WTAPE(22)=(PARU(2)/PARU(101))*XFGA(21)*
     &    (XB/(XB+XEC)-XB/(1.-XEC))
          XFGAS=XFGA(21)
          WTGASV=XFGA(21)
          Q2BSVV=Q2B
        ENDIF
C...f -> gamma, W+, W-.
      ELSEIF(KFLB.EQ.22) THEN
        WTAPF=LOG((1.-XEE-XB)*(1.-XEE)/(XEE*(XB+XEE)))/XB
        WTAPE(11)=WTAPF
        WTAPE(-11)=WTAPF
      ELSEIF(KFLB.EQ.24) THEN
        WTAPE(-11)=1./(4.*PARU(102))*LOG((1.-XEE-XB)*(1.-XEE)/
     &  (XEE*(XB+XEE)))/XB
      ELSEIF(KFLB.EQ.-24) THEN
        WTAPE(11)=1./(4.*PARU(102))*LOG((1.-XEE-XB)*(1.-XEE)/
     &  (XEE*(XB+XEE)))/XB
      ENDIF
  160 WTSUMC=0.
      WTSUME=0.
      XFBO=XFB(KFLB)
      DO 170 KFL=-25,25
      WTSF(KFL)=XFB(KFL)/XFBO
      WTSUMC=WTSUMC+WTAPC(KFL)*WTSF(KFL)
  170 WTSUME=WTSUME+WTAPE(KFL)*WTSF(KFL)
      WTSUMC=MAX(0.0001,WTSUMC)
      WTSUME=MAX(0.0001/FWTE,WTSUME)
 
C...Choose new t: fix alpha_s, alpha_s(Q^2), alpha_s(k_T^2).
  180 IF(MCEV.EQ.1) THEN
        IF(MSTP(64).LE.0) THEN
          TEVCB=TEVCB+LOG(RLU(0))*PARU(2)/(PARU(111)*WTSUMC)
        ELSEIF(MSTP(64).EQ.1) THEN
          TEVCB=TEVCB*EXP(MAX(-50.,LOG(RLU(0))*B0/WTSUMC))
        ELSE
          TEVCB=TEVCB*EXP(MAX(-50.,LOG(RLU(0))*B0/(5.*WTSUMC)))
        ENDIF
      ENDIF
      IF(MEEV.EQ.1) THEN
        TEVEB=TEVEB*EXP(MAX(-50.,LOG(RLU(0))*PARU(2)/
     &  (PARU(101)*FWTE*WTSUME*TEMX)))
      ENDIF
 
C...Translate t into Q2 scale; choose between QCD and QED evolution.
  190 IF(MCEV.EQ.1) Q2CB=ALAM(JT)**2*EXP(MAX(-50.,TEVCB))/FQ2C
      IF(MEEV.EQ.1) Q2EB=SPME*EXP(MAX(-50.,TEVEB))
      MCE=0
      IF(MCEV.EQ.0.AND.MEEV.EQ.0) THEN
      ELSEIF(MCEV.EQ.1.AND.MEEV.EQ.0) THEN
        IF(Q2CB.GT.PARP(62)**2) MCE=1
      ELSEIF(MCEV.EQ.0.AND.MEEV.EQ.1) THEN
        IF(Q2EB.GT.PARP(68)**2) MCE=2
      ELSEIF(PARP(62).GT.PARP(68)) THEN
        MCE=1
        IF(Q2EB.GT.Q2CB.OR.Q2CB.LE.PARP(62)**2) MCE=2
        IF(MCE.EQ.2.AND.Q2EB.LE.PARP(68)**2) MCE=0
      ELSE
        MCE=2
        IF(Q2CB.GT.Q2EB.OR.Q2EB.LE.PARP(68)**2) MCE=1
        IF(MCE.EQ.1.AND.Q2CB.LE.PARP(62)**2) MCE=0
      ENDIF
 
C...Evolution possibly ended. Update t values.
      IF(MCE.EQ.0) THEN
        Q2B=0.
        GOTO 220
      ELSEIF(MCE.EQ.1) THEN
        Q2B=Q2CB
        Q2REF=FQ2C*Q2B
        IF(MEEV.EQ.1) TEVEB=ALOG(Q2B/SPME)
      ELSE
        Q2B=Q2EB
        Q2REF=Q2B
        IF(MCEV.EQ.1) TEVCB=ALOG(FQ2C*Q2B/ALAM(JT)**2)
      ENDIF
 
C...Select flavour for branching parton.
      IF(MCE.EQ.1) WTRAN=RLU(0)*WTSUMC
      IF(MCE.EQ.2) WTRAN=RLU(0)*WTSUME
      KFLA=-25
  200 KFLA=KFLA+1
      IF(MCE.EQ.1) WTRAN=WTRAN-WTAPC(KFLA)*WTSF(KFLA)
      IF(MCE.EQ.2) WTRAN=WTRAN-WTAPE(KFLA)*WTSF(KFLA)
      IF(KFLA.LE.24.AND.WTRAN.GT.0.) GOTO 200
      IF(KFLA.EQ.25) THEN
        Q2B=0.
        GOTO 220
      ENDIF
 
C...Choose z value and corrective weight.
      WTZ=0.
C...q -> q + g.
      IF(IABS(KFLA).LE.10.AND.IABS(KFLB).LE.10) THEN
        Z=1.-((1.-XB-XEC)/(1.-XEC))*
     &  (XEC*(1.-XEC)/((XB+XEC)*(1.-XB-XEC)))**RLU(0)
        WTZ=0.5*(1.+Z**2)
C...q -> g + q.
      ELSEIF(IABS(KFLA).LE.10.AND.KFLB.EQ.21) THEN
        Z=XB/(SQRT(XB+XEC)+RLU(0)*(SQRT(1.-XEC)-SQRT(XB+XEC)))**2
        WTZ=0.5*(1.+(1.-Z)**2)*SQRT(Z)
C...f -> f + gamma.
      ELSEIF(IABS(KFLA).LE.20.AND.IABS(KFLB).LE.20) THEN
        IF(WTAPF1.GT.RLU(0)*(WTAPF1+WTAPF2)) THEN
          Z=1.-((1.-XB-XEE)/(1.-XEE))*
     &    (XEE*(1.-XEE)/((XB+XEE)*(1.-XB-XEE)))**RLU(0)
        ELSE
          Z=XB+XB*(XEE/(1.-XEE))*
     &    ((1.-XB-XEE)*(1.-XEE)/(XEE*(XB+XEE)))**RLU(0)
        ENDIF
        WTZ=0.5*(1.+Z**2)*(Z-XB)/(1.-XB)
C...f -> gamma + f.
      ELSEIF(IABS(KFLA).LE.20.AND.KFLB.EQ.22) THEN
        Z=XB+XB*(XEE/(1.-XEE))*
     &  ((1.-XB-XEE)*(1.-XEE)/(XEE*(XB+XEE)))**RLU(0)
        WTZ=0.5*(1.+(1.-Z)**2)*XB*(Z-XB)/Z
C...f -> W+- + f'.
      ELSEIF(IABS(KFLA).LE.20.AND.IABS(KFLB).EQ.24) THEN
        Z=XB+XB*(XEE/(1.-XEE))*
     &  ((1.-XB-XEE)*(1.-XEE)/(XEE*(XB+XEE)))**RLU(0)
        WTZ=0.5*(1.+(1.-Z)**2)*(XB*(Z-XB)/Z)*(Q2B/(Q2B+PMAS(24,1)**2))
C...g -> q + q~.
      ELSEIF(KFLA.EQ.21.AND.IABS(KFLB).LE.10) THEN
        Z=XB/(1.-XEC)+RLU(0)*(XB/(XB+XEC)-XB/(1.-XEC))
        WTZ=1.-2.*Z*(1.-Z)
C...g -> g + g.
      ELSEIF(KFLA.EQ.21.AND.KFLB.EQ.21) THEN
        Z=1./(1.+((1.-XEC-XB)/XB)*(XEC/(1.-XEC-XB))**RLU(0))
        WTZ=(1.-Z*(1.-Z))**2
C...gamma -> q + q~.
      ELSEIF(KFLA.EQ.22.AND.IABS(KFLB).LE.10) THEN
        Z=XB/(1.-XEC)+RLU(0)*(XB/(XB+XEC)-XB/(1.-XEC))
        CALL PYSTFU(22,Z,Q2B,XFGA)
        WTZ=XFGA(KFLB)/(Z*XFGAS)
C...gamma -> f + f~.
      ELSEIF(KFLA.EQ.22.AND.IABS(KFLB).LE.20) THEN
        Z=XB/(1.-XEE)+RLU(0)*(XB/(XB+XEE)-XB/(1.-XEE))
        WTZ=1.-2.*Z*(1.-Z)
C...gamma -> g + g.
      ELSEIF(KFLA.EQ.22.AND.KFLB.EQ.21) THEN
        Z=XB/(1.-XEC)+RLU(0)*(XB/(XB+XEC)-XB/(1.-XEC))
        CALL PYSTFU(22,Z,Q2B,XFGA)
        WTZ=XFGA(21)/(Z*XFGAS)
      ENDIF
      IF(MCE.EQ.2) WTZ=(WTZ/FWTE)*(TEVEB/TEMX)
 
C...Option with resummation of soft gluon emission as effective z shift.
      IF(MCE.EQ.1) THEN
        IF(MSTP(65).GE.1) THEN
          RSOFT=6.
          IF(KFLB.NE.21) RSOFT=8./3.
          Z=Z*(TEVCB/TEVCSV(JT))**(RSOFT*XEC/((XB+XEC)*B0))
          IF(Z.LE.XB) GOTO 180
        ENDIF
 
C...Option with alpha_s(k_T^2): demand k_T^2 > cutoff, reweight.
        IF(MSTP(64).GE.2) THEN
          IF((1.-Z)*Q2B.LT.PARP(62)**2) GOTO 180
          ALPRAT=TEVCB/(TEVCB+LOG(1.-Z))
          IF(ALPRAT.LT.5.*RLU(0)) GOTO 180
          IF(ALPRAT.GT.5.) WTZ=WTZ*ALPRAT/5.
        ENDIF
 
C...Option with angular ordering requirement.
        IF(MSTP(62).GE.3) THEN
          THE2T=(4.*Z**2*Q2B)/(VINT(2)*(1.-Z)*XB**2)
          IF(THE2T.GT.THE2(JT)) GOTO 180
        ENDIF
      ENDIF
 
C...Weighting with new structure functions.
      CALL PYSTFU(MINT(10+JT),XB,Q2REF,XFN)
      XFBN=XFN(KFLB)
      IF(XFBN.LT.1E-20) THEN
        IF(KFLA.EQ.KFLB) THEN
          TEVCB=TEVCBS
          TEVEB=TEVEBS
          WTAPC(KFLB)=0.
          WTAPE(KFLB)=0.
          GOTO 160
        ELSEIF(MCE.EQ.1.AND.TEVCBS-TEVCB.GT.0.2) THEN
          TEVCB=0.5*(TEVCBS+TEVCB)
          GOTO 190
        ELSEIF(MCE.EQ.2.AND.TEVEBS-TEVEB.GT.0.2) THEN
          TEVEB=0.5*(TEVEBS+TEVEB)
          GOTO 190
        ELSE
          XFBN=1E-10
          XFN(KFLB)=XFBN
        ENDIF
      ENDIF
      DO 210 KFL=-25,25
  210 XFB(KFL)=XFN(KFL)
      XA=XB/Z
      CALL PYSTFU(MINT(10+JT),XA,Q2REF,XFA)
      XFAN=XFA(KFLA)
      IF(XFAN.LT.1E-20) GOTO 160
      WTSFA=WTSF(KFLA)
      IF(WTZ*XFAN/XFBN.LT.RLU(0)*WTSFA) GOTO 160
 
C...Define two hard scatterers in their CM-frame.
  220 IF(N.EQ.NS+2) THEN
        DQ2(JT)=Q2B
        DPLCM=SQRT((DSH+DQ2(1)+DQ2(2))**2-4D0*DQ2(1)*DQ2(2))/DSHR
        DO 240 JR=1,2
        I=NS+JR
        IF(JR.EQ.1) IPO=IPUS1
        IF(JR.EQ.2) IPO=IPUS2
        DO 230 J=1,5
        K(I,J)=0
        P(I,J)=0.
  230   V(I,J)=0.
        K(I,1)=14
        K(I,2)=KFLS(JR+2)
        K(I,4)=IPO
        K(I,5)=IPO
        P(I,3)=DPLCM*(-1)**(JR+1)
        P(I,4)=(DSH+DQ2(3-JR)-DQ2(JR))/DSHR
        P(I,5)=-SQRT(SNGL(DQ2(JR)))
        K(IPO,1)=14
        K(IPO,3)=I
        K(IPO,4)=MOD(K(IPO,4),MSTU(5))+MSTU(5)*I
  240   K(IPO,5)=MOD(K(IPO,5),MSTU(5))+MSTU(5)*I
 
C...Find maximum allowed mass of timelike parton.
      ELSEIF(N.GT.NS+2) THEN
        JR=3-JT
        DQ2(3)=Q2B
        DPC(1)=P(IS(1),4)
        DPC(2)=P(IS(2),4)
        DPC(3)=0.5*(ABS(P(IS(1),3))+ABS(P(IS(2),3)))
        DPD(1)=DSH+DQ2(JR)+DQ2(JT)
        DPD(2)=DSHZ+DQ2(JR)+DQ2(3)
        DPD(3)=SQRT(DPD(1)**2-4D0*DQ2(JR)*DQ2(JT))
        DPD(4)=SQRT(DPD(2)**2-4D0*DQ2(JR)*DQ2(3))
        IKIN=0
        IF(Q2S(JR).GE.(0.5*PARP(62))**2.AND.DPD(1)-DPD(3).GE.
     &  1D-10*DPD(1)) IKIN=1
        IF(IKIN.EQ.0) DMSMA=(DQ2(JT)/DBLE(ZS(JT))-DQ2(3))*(DSH/
     &  (DSH+DQ2(JT))-DSH/(DSHZ+DQ2(3)))
        IF(IKIN.EQ.1) DMSMA=(DPD(1)*DPD(2)-DPD(3)*DPD(4))/(2.*
     &  DQ2(JR))-DQ2(JT)-DQ2(3)
 
C...Generate timelike parton shower (if required).
        IT=N
        DO 250 J=1,5
        K(IT,J)=0
        P(IT,J)=0.
  250   V(IT,J)=0.
        K(IT,1)=3
C...f -> f + g (gamma).
        IF(IABS(KFLB).LE.20.AND.IABS(KFLS(JT+2)).LE.20) THEN
          K(IT,2)=21
          IF(IABS(KFLB).GE.11) K(IT,2)=22
C...f -> g (gamma, W+-) + f.
        ELSEIF(IABS(KFLB).LE.20.AND.IABS(KFLS(JT+2)).GT.20) THEN
          K(IT,2)=KFLB
          IF(KFLS(JT+2).EQ.24) THEN
            K(IT,2)=-12
          ELSEIF(KFLS(JT+2).EQ.-24) THEN
            K(IT,2)=12
          ENDIF
C...g (gamma) -> f + f~, g + g.
        ELSE
          K(IT,2)=-KFLS(JT+2)
          IF(KFLS(JT+2).GT.20) K(IT,2)=KFLS(JT+2)
        ENDIF
        P(IT,5)=ULMASS(K(IT,2))
        IF(SNGL(DMSMA).LE.P(IT,5)**2) GOTO 100
        IF(MSTP(63).GE.1.AND.MCE.EQ.1) THEN
          P(IT,4)=(DSHZ-DSH-P(IT,5)**2)/DSHR
          P(IT,3)=SQRT(P(IT,4)**2-P(IT,5)**2)
          IF(MSTP(63).EQ.1) THEN
            Q2TIM=DMSMA
          ELSEIF(MSTP(63).EQ.2) THEN
            Q2TIM=MIN(SNGL(DMSMA),PARP(71)*Q2S(JT))
          ELSE
            Q2TIM=DMSMA
            MSTJ(48)=1
            IF(IKIN.EQ.0) DPT2=DMSMA*(DSHZ+DQ2(3))/(DSH+DQ2(JT))
            IF(IKIN.EQ.1) DPT2=DMSMA*(0.5*DPD(1)*DPD(2)+0.5*DPD(3)*
     &      DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)))/(4.*DSH*DPC(3)**2)
            PARJ(85)=SQRT(MAX(0.,SNGL(DPT2)))*
     &      (1./P(IT,4)+1./P(IS(JT),4))
          ENDIF
          CALL LUSHOW(IT,0,SQRT(Q2TIM))
          MSTJ(48)=0
          IF(N.GE.IT+1) P(IT,5)=P(IT+1,5)
        ENDIF
 
C...Reconstruct kinematics of branching: timelike parton shower.
        DMS=P(IT,5)**2
        IF(IKIN.EQ.0) DPT2=(DMSMA-DMS)*(DSHZ+DQ2(3))/(DSH+DQ2(JT))
        IF(IKIN.EQ.1) DPT2=(DMSMA-DMS)*(0.5*DPD(1)*DPD(2)+0.5*DPD(3)*
     &  DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)+DMS))/(4.*DSH*DPC(3)**2)
        IF(DPT2.LT.0.) GOTO 100
        DPB(1)=(0.5*DPD(2)-DPC(JR)*(DSHZ+DQ2(JR)-DQ2(JT)-DMS)/
     &  DSHR)/DPC(3)-DPC(3)
        P(IT,1)=SQRT(SNGL(DPT2))
        P(IT,3)=DPB(1)*(-1)**(JT+1)
        P(IT,4)=(DSHZ-DSH-DMS)/DSHR
        IF(N.GE.IT+1) THEN
          DPB(1)=SQRT(DPB(1)**2+DPT2)
          DPB(2)=SQRT(DPB(1)**2+DMS)
          DPB(3)=P(IT+1,3)
          DPB(4)=SQRT(DPB(3)**2+DMS)
          DBEZ=(DPB(4)*DPB(1)-DPB(3)*DPB(2))/(DPB(4)*DPB(2)-DPB(3)*
     &    DPB(1))
          CALL LUDBRB(IT+1,N,0.,0.,0D0,0D0,DBEZ)
          THE=ULANGL(P(IT,3),P(IT,1))
          CALL LUDBRB(IT+1,N,THE,0.,0D0,0D0,0D0)
        ENDIF
 
C...Reconstruct kinematics of branching: spacelike parton.
        DO 260 J=1,5
        K(N+1,J)=0
        P(N+1,J)=0.
  260   V(N+1,J)=0.
        K(N+1,1)=14
        K(N+1,2)=KFLB
        P(N+1,1)=P(IT,1)
        P(N+1,3)=P(IT,3)+P(IS(JT),3)
        P(N+1,4)=P(IT,4)+P(IS(JT),4)
        P(N+1,5)=-SQRT(SNGL(DQ2(3)))
 
C...Define colour flow of branching.
        K(IS(JT),3)=N+1
        K(IT,3)=N+1
        IM1=N+1
        IM2=N+1
C...f -> f + gamma (Z, W).
        IF(IABS(K(IT,2)).GE.22) THEN
          K(IT,1)=1
          ID1=IS(JT)
          ID2=IS(JT)
C...f -> gamma (Z, W) + f.
        ELSEIF(IABS(K(IS(JT),2)).GE.22) THEN
          ID1=IT
          ID2=IT
C...gamma -> q + q~, g + g.
        ELSEIF(K(N+1,2).EQ.22) THEN
          ID1=IS(JT)
          ID2=IT
          IM1=ID2
          IM2=ID1
C...q -> q + g.
        ELSEIF(K(N+1,2).GT.0.AND.K(N+1,2).NE.21.AND.K(IT,2).EQ.21) THEN
          ID1=IT
          ID2=IS(JT)
C...q -> g + q.
        ELSEIF(K(N+1,2).GT.0.AND.K(N+1,2).NE.21) THEN
          ID1=IS(JT)
          ID2=IT
C...q~ -> q~ + g.
        ELSEIF(K(N+1,2).LT.0.AND.K(IT,2).EQ.21) THEN
          ID1=IS(JT)
          ID2=IT
C...q~ -> g + q~.
        ELSEIF(K(N+1,2).LT.0) THEN
          ID1=IT
          ID2=IS(JT)
C...g -> g + g; g -> q + q~.
        ELSEIF((K(IT,2).EQ.21.AND.RLU(0).GT.0.5).OR.K(IT,2).LT.0) THEN
          ID1=IS(JT)
          ID2=IT
        ELSE
          ID1=IT
          ID2=IS(JT)
        ENDIF
        IF(IM1.EQ.N+1) K(IM1,4)=K(IM1,4)+ID1
        IF(IM2.EQ.N+1) K(IM2,5)=K(IM2,5)+ID2
        K(ID1,4)=K(ID1,4)+MSTU(5)*IM1
        K(ID2,5)=K(ID2,5)+MSTU(5)*IM2
        IF(ID1.NE.ID2) THEN
          K(ID1,5)=K(ID1,5)+MSTU(5)*ID2
          K(ID2,4)=K(ID2,4)+MSTU(5)*ID1
        ENDIF
        N=N+1
 
C...Boost to new CM-frame.
        DBSVX=DBLE((P(N,1)+P(IS(JR),1))/(P(N,4)+P(IS(JR),4)))
        DBSVZ=DBLE((P(N,3)+P(IS(JR),3))/(P(N,4)+P(IS(JR),4)))
        IF(DBSVX**2+DBSVZ**2.GE.1D0) GOTO 100
        CALL LUDBRB(NS+1,N,0.,0.,-DBSVX,0D0,-DBSVZ)
        IR=N+(JT-1)*(IS(1)-N)
        CALL LUDBRB(NS+1,N,-ULANGL(P(IR,3),P(IR,1)),PARU(2)*RLU(0),
     &  0D0,0D0,0D0)
      ENDIF
 
C...Redefine maximum Q^2 when QED shower <- QCD shower.
      IS(JT)=N
      DQ2(JT)=Q2B
      IF(MSTP(62).GE.3) THE2(JT)=THE2T
      DSH=DSHZ
      IF(KFLA.EQ.22.AND.(IABS(KFLB).LE.10.OR.KFLB.EQ.21)) THEN
        VINT(154+JT)=XA
        MEEV=1
        IF(MSTP(13).LE.1) THEN
          Q2B=Q2MX
        ELSE
          Q2B=PARP(13)
        ENDIF
        TEVEB=LOG(Q2B/SPME)
        CALL PYSTFU(MINT(10+JT),XA,Q2B,XFA)
      ENDIF
 
C...Save quantities; loop back.
      Q2S(JT)=Q2B
      IF((MCEV.EQ.1.AND.Q2B.GE.(0.5*PARP(62))**2).OR.
     &(MEEV.EQ.1.AND.Q2B.GE.PARP(68)**2)) THEN
        KFLS(JT+2)=KFLS(JT)
        KFLS(JT)=KFLA
        XS(JT)=XA
        ZS(JT)=Z
        DO 270 KFL=-25,25
  270   XFS(JT,KFL)=XFA(KFL)
        TEVCSV(JT)=TEVCB
        TEVESV(JT)=TEVEB
      ELSE
        MORE(JT)=0
        IF(JT.EQ.1) IPU1=N
        IF(JT.EQ.2) IPU2=N
      ENDIF
      IF(N.GT.MSTU(4)-MSTU(32)-10) THEN
        CALL LUERRM(11,'(PYSSPA:) no more memory left in LUJETS')
        IF(MSTU(21).GE.1) N=NS
        IF(MSTU(21).GE.1) RETURN
      ENDIF
      IF(MORE(1).EQ.1.OR.MORE(2).EQ.1) GOTO 120
 
C...Boost hard scattering partons to frame of shower initiators.
      DO 280 J=1,3
  280 ROBO(J+2)=(P(NS+1,J)+P(NS+2,J))/(P(NS+1,4)+P(NS+2,4))
      K(N+2,1)=1
      DO 290 J=1,5
  290 P(N+2,J)=P(NS+1,J)
      ROBOT=ROBO(3)**2+ROBO(4)**2+ROBO(5)**2
      IF(ROBOT.GE.0.999999) THEN
        ROBOT=1.00001*SQRT(ROBOT)
        ROBO(3)=ROBO(3)/ROBOT
        ROBO(4)=ROBO(4)/ROBOT
        ROBO(5)=ROBO(5)/ROBOT
      ENDIF
      CALL LUDBRB(N+2,N+2,0.,0.,-DBLE(ROBO(3)),-DBLE(ROBO(4)),
     &-DBLE(ROBO(5)))
      ROBO(2)=ULANGL(P(N+2,1),P(N+2,2))
      ROBO(1)=ULANGL(P(N+2,3),SQRT(P(N+2,1)**2+P(N+2,2)**2))
      CALL LUDBRB(MINT(83)+5,NS,ROBO(1),ROBO(2),DBLE(ROBO(3)),
     &DBLE(ROBO(4)),DBLE(ROBO(5)))
 
C...Store user information. Reset Lambda value.
      K(IPU1,3)=MINT(83)+3
      K(IPU2,3)=MINT(83)+4
      DO 300 JT=1,2
      MINT(12+JT)=KFLS(JT)
  300 VINT(140+JT)=XS(JT)
      PARU(111)=ALAMS
 
      RETURN
      END
