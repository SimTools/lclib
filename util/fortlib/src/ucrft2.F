C  
C   ROUTINE TO DO CIRCLE FIT BY LEAST SQURE FITTING.
C   THE DATA POINTS SHOULD BE ORDERED ACCORDING TO ABS(Z) OF THEM
C   PREVIOUSLY.
C  
C       UCRFT2( IMODE, NP, X, Y, DX, DY, CPARM, RSD, IOUT)
C  
C       GENERAL CIRCLE IS ASSUMED TO BE FITTED.
C            Y = A2 * ( X**2 + Y**2 ) + A1 * X + A0
C            (XC,YC,RHO) IS DEDUCED FROM (A2,A1,A0)
C  
C         IMODE = 1   TO GET CPARM USING GIVEN POINTS (NP) .
C                 2   TO CHECK RESIDUALS USING CPARM .
C  
C         NP          # OF TOTAL POINTS
C         X(NP),Y(NP) COORDINATES  (X,Y)
C         DX,DY       ERRORS
C         CPARM(I)    FITTED PARAMETERS
C              1=XC; 2=YC; 3=RHO; 4-6= ERRORS; 7=XISQ/ND
C         RSD(NP)     RESIDUALS FOR THE POINTS.
C  
C         2-MAR-85    CODED BY H. KICHIMI
C  
      SUBROUTINE  UCRFT2( IMODE, NP, X, Y, DX, DY, CPARM, RSD, IOUT)
C  
        REAL  X(NP),Y(NP),DX(NP),DY(NP),RSD(NP)
        REAL  CPARM(10)
CC      DOUBLE PRECISION  ROW(20),ARRAY(20,20)
        DOUBLE PRECISION  ROW(20),ARRAY(10,10)
        REAL   APRM(3),ERRA(3)
C  
       IF(IMODE.NE.1)     GOTO   1000
C  
C   TO PREPARE MATRIX ELEMENTS
C  
        ARRAY(1,1)=0.
        ARRAY(1,2)=0.
        ARRAY(1,3)=0.
        ARRAY(2,2)=0.
        ARRAY(2,3)=0.
        ARRAY(3,3)=0.
        ROW(1)=0
        ROW(2)=0
        ROW(3)=0
C  
        DO   10  I=1,NP
        SGINV=1.0/(DY(I)*DY(I))
        ARRAY(1,1)=ARRAY(1,1)+SGINV
        ARRAY(1,2)=ARRAY(1,2)+SGINV*X(I)
        XY2= (X(I)*X(I)+Y(I)*Y(I))*SGINV
        ARRAY(1,3)=ARRAY(1,3)+XY2
        ARRAY(2,2)=ARRAY(2,2)+SGINV*X(I)*X(I)
        ARRAY(2,3)=ARRAY(2,3)+XY2*X(I)
        ARRAY(3,3)=ARRAY(3,3)+XY2*(X(I)*X(I)+Y(I)*Y(I))
        ROW(1)=ROW(1)+SGINV*Y(I)
        ROW(2)=ROW(2)+SGINV*X(I)*Y(I)
        ROW(3)=ROW(3)+XY2*Y(I)
10      CONTINUE
C  
        ARRAY(2,1)=ARRAY(1,2)
        ARRAY(3,1)=ARRAY(1,3)
        ARRAY(3,2)=ARRAY(2,3)
C  
C   TO MAKE MATRIX INVERSION
C  
        CALL  MATINV( ARRAY, 3, DET)
C  
C   TO GET (A0,A1,A2)  PARAMETERS
C  
        DO   20  IPM=1,3
        APRM(IPM)=0.
        ERRA(IPM)=SQRT(ABS(ARRAY(IPM,IPM)))
        DO   30  J=1,3
30      APRM(IPM)=APRM(IPM)+ROW(J)*ARRAY(IPM,J)
20      CONTINUE
C  
C   CONVERT (A0,A1,A2)  TO (XC,YC,RHO)
C  
        XC = -APRM(2)/(2.0*APRM(3))
        YC = 1.0/(2.0*APRM(3))
        RHO = SQRT(APRM(2)**2+1.0-4.0*APRM(1)*APRM(3))/(2.0*APRM(3))
        RHO = ABS(RHO)
        DXC = ABS(XC)*SQRT((ERRA(2)/APRM(2))**2+(ERRA(3)/APRM(3))**2)
        DYC = ABS(YC)*ERRA(3)/APRM(3)
        DRHO= ABS(RHO)*( SQRT(
     X          (ERRA(3)/APRM(3))**2
     X        + ( (APRM(2)*ERRA(2))**2
     X        +    4.0*(APRM(3)*ERRA(1))**2+4.0*(APRM(1)*ERRA(3))**2 )
     X             /(APRM(2)**2+1.0-4.0*APRM(1)*APRM(3))**2
     X                 )     )
C  
        CPARM(1)=XC
        CPARM(2)=YC
C  
C   TO GET A SIGN OF RHO
C     RSIGN= SIGN (V(1) X V(NP))
C  
      VEC=(X(1)-XC)*(Y(NP)-YC)
     X  - (Y(1)-YC)*(X(NP)-XC)
      RSIGN=SIGN(1.0,VEC)
C  
C     WRITE(6,692)   VEC,RSIGN
C692  FORMAT(1X,'VEC,RSIGN=',2F10.2)
C  
        CPARM(3)=RHO*RSIGN
        CPARM(4)=DXC
        CPARM(5)=DYC
        CPARM(6)=DRHO
        CPARM(7)=0.0
        CPARM(8)=0.0
        CPARM(9)=0.0
        CPARM(10)=0.0
C  
1000   CONTINUE
C  
C  TO CHECK RESIDUALS FOR ALL POINTS.
C  
        XISQT=0.0
        ARHO=ABS(RHO)
C  
        DO  40  I=1,NP
        DXY2 = ((X(I)-XC)*DX(I))**2+((Y(I)-YC)*DY(I))**2
        RXY  = SQRT( (X(I)-XC)**2+(Y(I)-YC)**2 )
        DR   = SQRT(DXY2)/RXY
        RSD(I) = ( ARHO-RXY )/DR
        XISQT= XISQT+RSD(I)**2
40      CONTINUE
C  
        IF(IMODE.EQ.1)  THEN
        XISQ=XISQT/FLOAT(NP-3)
        CPARM(7)=XISQ
        ENDIF
C  
        IF(IOUT.GE.1)   THEN
        WRITE(6,602)  IMODE,(CPARM(K),K=1,7)
602     FORMAT(1X,'### UCRFT2 ###   IMODE=',I3,'  (XC,YC,RHO)=',3E10.3,
     X     '  ERRORS=',3E10.3,'  XISQ/NHIT=',E10.3)
C  
        DO  50  I=1,NP
        WRITE(6,612)  I,X(I),DX(I),Y(I),DY(I),RSD(I)
612     FORMAT(9X,'#=',I3,' (X,DX,Y,DY,RSD)=',5E12.4)
50      CONTINUE
        ENDIF
C  
5000    CONTINUE
        RETURN
        END
