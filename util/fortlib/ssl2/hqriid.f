C#NUMPAC#HQRIID              REVISED ON 1988-03-07
      SUBROUTINE HQRIID(A,KA,N,E,V,NEV,EPS,W,ILL)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(KA,N),V(KA,1),E(N),W(N,6)
      DATA DMACH/2.D-16/
      NV=IABS(NEV)
      IF(N.LE.1.OR.N.GT.KA.OR.NV.GT.N.OR.EPS.LE.0.D0) GO TO 420
C     HOUSEHOLDER TRANSFORMATION
      DO 60 K=1,N-2
      W(K,2)=A(K,K)
      S=0.D0
      DO 10 J=K+1,N
      W(J,2)=A(K,J)
   10 S=W(J,2)**2+S
      S=DSIGN(DSQRT(S),W(K+1,2))
      W(K,1)=-S
      W(K+1,2)=W(K+1,2)+S
      A(K,K+1)=W(K+1,2)
      H=W(K+1,2)*S
      IF(H.EQ.0.D0) GO TO 60
      DD=0.D0
      DO 40 I=K+1,N
      D=0.D0
      DO 20 J=K+1,I
   20 D=A(J,I)*W(J,2)+D
      DO 30 J=I+1,N
   30 D=A(I,J)*W(J,2)+D
      W(I,1)=D/H
   40 DD=W(I,1)*W(I,2)+DD
      U=DD*0.5D0/H
      DO 50 J=K+1,N
      W(J,1)=W(J,2)*U-W(J,1)
      DO 50 I=K+1,J
   50 A(I,J)=W(I,1)*W(J,2)+W(J,1)*W(I,2)+A(I,J)
   60 A(K,K)=H
      W(N-1,2)=A(N-1,N-1)
      W(N,2)=A(N,N)
      W(N-1,1)=A(N-1,N)
      W(N,1)=0.D0
C     COMPUTATION OF THE MATRIX NORM
      GN=DABS(W(1,2))+DABS(W(1,1))
      DO 70 I=1,N-1
   70 GN=DMAX1(DABS(W(I+1,2))+DABS(W(I,1))+DABS(W(I+1,1)),GN)
      DEL=DMAX1(EPS,DMACH)*GN
      DEL2=DEL*DEL
      DO 80 I=1,N
      E(I)=W(I,2)
   80 IF(DABS(W(I,1)).LT.DEL) W(I,1)=0.D0
      W(1,3)=0.D0
      DO 90 I=1,N-1
   90 W(I+1,3)=W(I,1)*W(I,1)
      IF(GN.EQ.0.D0) GO TO 180
C     QR-METHOD WITH ORIGIN SHIFT WITHOUT DSQUARE ROOT
      DO 170 K=N,2,-1
  100 DO 110 L=K,2,-1
      IF(DABS(W(L,3)).LT.DEL2) GO TO 120
  110 CONTINUE
      L=1
  120 IF(L.EQ.K) GO TO 170
      G=(E(K-1)+E(K))*0.5D0
      F=E(K)-G
      Z=DSIGN(DSQRT(F*F+W(K,3)),F)+G
      IF(L+1.EQ.K) GO TO 140
      DO 130 ITIME=1,2
      F2=(E(K-1)-Z)*(E(K)-Z)-W(K,3)
      F3=(E(K-2)-Z)*F2+(Z-E(K))*W(K-1,3)
      DF3=(E(K-2)-Z)*(Z-E(K)+Z-E(K-1))-F2+W(K-1,3)
      IF(DABS(DF3).GT.DABS(F3)) Z=Z-F3/DF3
  130 CONTINUE
  140 G=E(L)-Z
      PP=G*G
      CC=1.D0
      SS=0.D0
      DO 160 J=L,K-1
      QQ=W(J+1,3)
      TT=PP+QQ
      W(J,3)=SS*TT
      OCC=CC
      SS=QQ/TT
      CC=PP/TT
      OG=G
      IF(CC.EQ.0.D0) GO TO 150
      G=(E(J+1)-Z)*CC-OG*SS
      E(J)=E(J+1)+OG-G
      PP=G*G/CC
      GO TO 160
  150 G=-OG
      E(J)=E(J+1)+OG+OG
      PP=QQ*OCC
  160 CONTINUE
      E(K)=G+Z
      W(K,3)=SS*PP
      GO TO 100
  170 CONTINUE
C     QUICK SORT OF EIGENVALUES
      IND=1
      IF(NEV.LT.0) IND=0
      CALL SORTDK(N,E,IND)
  180 IF(NV.EQ.0) GO TO 410
C     INVERSE ITERATION FOR EIGENVECTORS
      EPS1=DSQRT(DFLOAT(N))*EPS
      EPS2=GN*1.D-3
      RN=0.D0
      RA=EPS*0.61803398874989490D+00
      DO 280 J=1,NV
      DO 190 I=1,N
      W(I,3)=0.D0
      W(I,4)=W(I,1)
      W(I,5)=W(I,2)-E(J)
      RN=RN+RA
      IF(RN.GE.EPS) RN=RN-EPS
  190 V(I,J)=RN
      DO 220 I=1,N-1
      IF(DABS(W(I,5)).GE.DABS(W(I,1))) GO TO 200
      W(I,6)=-W(I,5)/W(I,1)
      W(I,5)=W(I,1)
      T=W(I+1,5)
      W(I+1,5)=W(I,4)
      W(I,4)=T
      W(I,3)=W(I+1,4)
      IF(W(I,3).EQ.0.D0) W(I,3)=DEL
      W(I+1,4)=0.D0
      GO TO 210
  200 IF(W(I,5).EQ.0.D0) W(I,5)=DEL
      W(I,6)=-W(I,1)/W(I,5)
  210 W(I+1,4)=W(I,3)*W(I,6)+W(I+1,4)
  220 W(I+1,5)=W(I,4)*W(I,6)+W(I+1,5)
      IF(W(N,5).EQ.0.D0) W(N,5)=DEL
      DO 270 IT=1,5
      IF(IT.EQ.1) GO TO 240
      DO 230 I=1,N-1
      IF(W(I,3).EQ.0.D0) GO TO 230
      T=V(I,J)
      V(I,J)=V(I+1,J)
      V(I+1,J)=T
  230 V(I+1,J)=V(I,J)*W(I,6)+V(I+1,J)
  240 V(N,J)=V(N,J)/W(N,5)
      V(N-1,J)=(V(N-1,J)-V(N,J)*W(N-1,4))/W(N-1,5)
      VN=DMAX1(DABS(V(N,J)),DABS(V(N-1,J)))
      DO 250 K=N-2,1,-1
      V(K,J)=(V(K,J)-V(K+1,J)*W(K,4)-V(K+2,J)*W(K,3))/W(K,5)
  250 VN=DMAX1(DABS(V(K,J)),VN)
      IF(IT.GT.1.AND.VN.GT.1.D0) GO TO 280
      S=EPS1/VN
      DO 260 I=1,N
  260 V(I,J)=V(I,J)*S
  270 CONTINUE
  280 CONTINUE
C     TRANSFORMATION OF EIGENVECTORS
      DO 330 K=N-2,1,-1
      IF(A(K,K).EQ.0.D0) GO TO 330
      DO 290 I=K,N
  290 W(I,1)=A(K,I)
      DO 320 J=1,NV
      S=0.D0
      DO 300 I=K+1,N
  300 S=W(I,1)*V(I,J)+S
      S=-S/W(K,1)
      DO 310 I=K+1,N
  310 V(I,J)=W(I,1)*S+V(I,J)
  320 CONTINUE
  330 CONTINUE
  340 IG=1
      DO 400 J=1,NV
      DO 350 I=IG,J
      IF(DABS(E(J)-E(I)).LT.EPS2) GO TO 360
  350 CONTINUE
      I=J
  360 IG=I
C     REORTHOGONALIZATION
      DO 380 K=IG,J-1
      S=0.D0
      DO 370 I=1,N
  370 S=V(I,K)*V(I,J)+S
      S=-S
      DO 380 I=1,N
  380 V(I,J)=V(I,K)*S+V(I,J)
C     NORMALIZATION
      S=0.D0
      DO 390 I=1,N
  390 S=V(I,J)**2+S
      S=1.D0/DSQRT(S)
      DO 400 I=1,N
  400 V(I,J)=V(I,J)*S
  410 ILL=0
      RETURN
  420 ILL=30000
      RETURN
      END
