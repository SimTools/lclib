C***********************************************************************
C*
C*  ----------------------------------------------======
C*   Subroutine WIDHIG( LU, AMH, GCOUPL, AMAS,   WIDTH )
C*  ----------------------------------------------======
C*(Function)
C*   Calculate decay width of
C*             H --> f + fbar and V + V
C*                   f = (e, mu, tau, u, c, t, d, s, c)
C*                   V = ( W, Z, gamma, g )
C*(Input)
C*   LU    ; 0< LU < 99 to list Higgs parameters.
C*   AMH   ; Mass of Higgs
C*   GCOUPL(3) ; Coupling constants.
C*         (1) = ALPHA ; Alpha
C*         (2) = SIN2TW; Weignberg angle.
C*         (3) = Alpha(s)
C*   AMAS(11)  ; Particle masses.
C*            1=e, 2=mu, 3=tau, 4=u, 5=c, 6=t, 7=d, 8=s, 9=b,
C*           10=W, 11=Z
C*(Output)
C*   WIDTH(0:13)  ; Decay width
C*           (0) = Total
C*           (1) = Partial width ( H --> e+ + e-)
C*           (2) =               (   --> mu+ + mu-)
C*           (3) =               (   --> tau+ + tau- )
C*           (4) =               (   --> u + ubar )
C*           (5) =               (   --> c + cbar )
C*           (6) =               (   --> t + tbar )
C*           (7) =               (   --> d + dbar )
C*           (8) =               (   --> s + sbar )
C*           (9) =               (   --> b + bbar )
C*          (10) =               (   --> W+ + W- )
C*          (11) =               (   --> Z  + Z  )
C*          (12) =               (   --> gamma + gamma )
C*          (13) =               (   --> gluon + gluon )
C*
C*(Author)
C*   A. Miyamoto  11-Aug-1990   Original version.
C*                23-Apr-1992   Fix error in h --> gamma + gamma
C*
C***********************************************************************
C
      SUBROUTINE WIDHIG( LU, AMH, GCOUPL, AMAS, WIDTH)
C
      DIMENSION WIDTH(0:13)
      DIMENSION AMAS(11)
      DIMENSION GCOUPL(3)
      DATA      PI/3.141592654/
      COMPLEX*8 CX, II, CF, CFI
      COMPLEX*8 CXSQ
      DATA  II/(0., 1.)/
      DATA  NVERT/1/
C
C For W and Z.
C  If NTYPE = 1, calculate width analytically
C           = 0, calculate width from block data.
C
      DATA  NTYPE/0/
C
C
       PARAMETER (NO$HWW=  110)
       DIMENSION  HTOWW(2,NO$HWW)
      DATA ((HTOWW(I,K),I=1,2),K=    1,   10)/
     1       60.0000,0.114464E-06,
     2       62.0000,0.148397E-06,
     3       64.0000,0.191320E-06,
     4       66.0000,0.245483E-06,
     5       68.0000,0.313533E-06,
     6       70.0000,0.398926E-06,
     7       72.0000,0.506002E-06,
     8       74.0000,0.639752E-06,
     9       76.0000,0.807056E-06,
     X       78.0000,0.101790E-05/
      DATA ((HTOWW(I,K),I=1,2),K=   11,   20)/
     1       80.0000,0.128765E-05,
     2       82.0000,0.162550E-05,
     3       84.0000,0.207223E-05,
     4       86.0000,0.267119E-05,
     5       88.0000,0.353917E-05,
     6       90.0000,0.484108E-05,
     7       92.0000,0.679868E-05,
     8       94.0000,0.970279E-05,
     9       96.0000,0.140065E-04,
     X       98.0000,0.202098E-04 /
C
      DATA ((HTOWW(I,K),I=1,2),K=   21,   39)/
     1      100.000    ,  0.289366E-04,
     2      102.000    ,  0.409256E-04,
     3      104.000    ,  0.570954E-04,
     4      106.000    ,  0.785217E-04,
     5      108.000    ,  0.106517E-03,
     6      110.000    ,  0.142494E-03,
     7      112.000    ,  0.188146E-03,
     8      114.000    ,  0.245440E-03,
     9      116.000    ,  0.316787E-03,
     X      118.000    ,  0.405052E-03,
     1      120.000    ,  0.513470E-03,
     2      122.000    ,  0.646044E-03,
     3      124.000    ,  0.806930E-03,
     4      126.000    ,  0.100255E-02,
     5      128.000    ,  0.123922E-02,
     6      130.000    ,  0.152720E-02,
     7      132.000    ,  0.187547E-02,
     8      134.000    ,  0.229734E-02,
     9      136.000    ,  0.281235E-02/
      DATA ((HTOWW(I,K),I=1,2),K=   40,   58)/
     1      138.000    ,  0.344002E-02,
     2      140.000    ,  0.421137E-02,
     3      141.000    ,  0.466300E-02,
     4      142.000    ,  0.516950E-02,
     5      143.000    ,  0.573299E-02,
     6      144.000    ,  0.636612E-02,
     7      145.000    ,  0.708104E-02,
     8      146.000    ,  0.789216E-02,
     9      147.000    ,  0.881725E-02,
     X      148.000    ,  0.987872E-02,
     1      149.000    ,  0.111056E-01,
     2      150.000    ,  0.125389E-01,
     3      151.000    ,  0.142434E-01,
     4      152.000    ,  0.162749E-01,
     5      153.000    ,  0.187502E-01,
     6      154.000    ,  0.218301E-01,
     7      155.000    ,  0.256952E-01,
     8      156.000    ,  0.309073E-01,
     9      157.000    ,  0.380945E-01/
      DATA ((HTOWW(I,K),I=1,2),K=   59,   77)/
     1      158.000    ,  0.485922E-01,
     2      159.000    ,  0.645985E-01,
     3      160.000    ,  0.885717E-01,
     4      161.000    ,  0.119249    ,
     5      162.000    ,  0.151777    ,
     6      163.000    ,  0.183030    ,
     7      164.000    ,  0.212331    ,
     8      165.000    ,  0.239843    ,
     9      166.000    ,  0.265897    ,
     X      167.000    ,  0.290762    ,
     1      168.000    ,  0.314671    ,
     2      169.000    ,  0.337813    ,
     3      170.000    ,  0.360350    ,
     4      171.000    ,  0.382419    ,
     5      172.000    ,  0.404118    ,
     6      173.000    ,  0.425533    ,
     7      174.000    ,  0.446735    ,
     8      175.000    ,  0.467782    ,
     9      176.000    ,  0.488720    /
      DATA ((HTOWW(I,K),I=1,2),K=   78,   96)/
     1      177.000    ,  0.509590    ,
     2      178.000    ,  0.530440    ,
     3      179.000    ,  0.551302    ,
     4      180.000    ,  0.572205    ,
     5      182.000    ,  0.614211    ,
     6      184.000    ,  0.656664    ,
     7      186.000    ,  0.699715    ,
     8      188.000    ,  0.743488    ,
     9      190.000    ,  0.788093    ,
     X      192.000    ,  0.833622    ,
     1      194.000    ,  0.880173    ,
     2      196.000    ,  0.927796    ,
     3      198.000    ,  0.976562    ,
     4      200.000    ,   1.02654    ,
     5      210.000    ,   1.29619    ,
     6      220.000    ,   1.60245    ,
     7      230.000    ,   1.94920    ,
     8      240.000    ,   2.33954    ,
     9      250.000    ,   2.77615    /
      DATA ((HTOWW(I,K),I=1,2),K=   97,  110)/
     1      260.000    ,   3.26146    ,
     2      270.000    ,   3.79771    ,
     3      280.000    ,   4.38709    ,
     4      290.000    ,   5.03164    ,
     5      300.000    ,   5.73338    ,
     6      350.000    ,   10.1695    ,
     7      400.000    ,   16.3296    ,
     8      450.000    ,   24.4554    ,
     9      500.000    ,   34.7886    ,
     X      550.000    ,   47.5726    ,
     1      600.000    ,   63.0481    ,
     2      650.000    ,   81.4590    ,
     3      700.000    ,   103.048    ,
     4      750.000    ,   128.060    /
       PARAMETER (NO$HZZ=   89)
       DIMENSION  HTOZZ(2,NO$HZZ)
      DATA ((HTOZZ(I,K),I=1,2),K=    1,   19)/
     1      100.000    ,  0.281000E-05,
     2      102.000    ,  0.364737E-05,
     3      104.000    ,  0.486435E-05,
     4      106.000    ,  0.659126E-05,
     5      108.000    ,  0.902291E-05,
     6      110.000    ,  0.123954E-04,
     7      112.000    ,  0.170241E-04,
     8      114.000    ,  0.231821E-04,
     9      116.000    ,  0.313070E-04,
     X      118.000    ,  0.419083E-04,
     1      120.000    ,  0.556671E-04,
     2      122.000    ,  0.727457E-04,
     3      124.000    ,  0.943807E-04,
     4      126.000    ,  0.121135E-03,
     5      128.000    ,  0.154049E-03,
     6      130.000    ,  0.194229E-03,
     7      132.000    ,  0.242866E-03,
     8      134.000    ,  0.301604E-03,
     9      136.000    ,  0.371945E-03/
      DATA ((HTOZZ(I,K),I=1,2),K=   20,   38)/
     1      138.000    ,  0.455991E-03,
     2      140.000    ,  0.555923E-03,
     3      142.000    ,  0.674790E-03,
     4      144.000    ,  0.815514E-03,
     5      146.000    ,  0.981886E-03,
     6      148.000    ,  0.117834E-02,
     7      150.000    ,  0.141192E-02,
     8      152.000    ,  0.168717E-02,
     9      154.000    ,  0.201463E-02,
     X      156.000    ,  0.240227E-02,
     1      158.000    ,  0.286442E-02,
     2      160.000    ,  0.342119E-02,
     3      162.000    ,  0.409548E-02,
     4      164.000    ,  0.491780E-02,
     5      166.000    ,  0.593312E-02,
     6      168.000    ,  0.720840E-02,
     7      170.000    ,  0.884909E-02,
     8      172.000    ,  0.109996E-01,
     9      174.000    ,  0.139554E-01/
      DATA ((HTOZZ(I,K),I=1,2),K=   39,   57)/
     1      176.000    ,  0.182334E-01,
     2      178.000    ,  0.249776E-01,
     3      180.000    ,  0.370237E-01,
     4      180.000    ,  0.370237E-01,
     5      181.000    ,  0.469864E-01,
     6      182.000    ,  0.610655E-01,
     7      183.000    ,  0.790277E-01,
     8      184.000    ,  0.993816E-01,
     9      185.000    ,  0.119643    ,
     X      186.000    ,  0.139309    ,
     1      187.000    ,  0.157954    ,
     2      188.000    ,  0.175649    ,
     3      189.000    ,  0.192531    ,
     4      190.000    ,  0.208738    ,
     5      191.000    ,  0.224386    ,
     6      192.000    ,  0.239434    ,
     7      193.000    ,  0.254223    ,
     8      194.000    ,  0.268701    ,
     9      195.000    ,  0.282926    /
      DATA ((HTOZZ(I,K),I=1,2),K=   58,   76)/
     1      196.000    ,  0.296944    ,
     2      197.000    ,  0.310799    ,
     3      198.000    ,  0.324526    ,
     4      199.000    ,  0.338158    ,
     5      200.000    ,  0.351719    ,
     6      202.000    ,  0.378714    ,
     7      204.000    ,  0.405672    ,
     8      206.000    ,  0.432714    ,
     9      208.000    ,  0.459944    ,
     X      210.000    ,  0.487453    ,
     1      212.000    ,  0.515307    ,
     2      214.000    ,  0.543574    ,
     3      216.000    ,  0.572310    ,
     4      218.000    ,  0.601565    ,
     5      220.000    ,  0.631379    ,
     6      230.000    ,  0.790012    ,
     7      240.000    ,  0.967043    ,
     8      250.000    ,   1.16505    ,
     9      260.000    ,   1.38591    /
      DATA ((HTOZZ(I,K),I=1,2),K=   77,   89)/
     1      270.000    ,   1.63125    ,
     2      280.000    ,   1.90243    ,
     3      290.000    ,   2.20066    ,
     4      300.000    ,   2.52716    ,
     5      350.000    ,   4.62095    ,
     6      400.000    ,   7.57564    ,
     7      450.000    ,   11.5130    ,
     8      500.000    ,   16.5535    ,
     9      550.000    ,   22.8181    ,
     X      600.000    ,   30.4269    ,
     1      650.000    ,   39.5010    ,
     2      700.000    ,   50.1617    ,
     3      750.000    ,   62.5302    /
 
C --- End of block data for H-->VV calculation.
 
C      BETAS(F,H)= SQRT( ( 1.-2.*F/H)*(1.+2.*F/H) )
      BETAS(F,H)= ( 1.-2.*F/H)*(1.+2.*F/H) 
      FES(E,ES) = 3.*(1.-8.*ES+20.*ES*ES)/SQRT(4.*ES-1.)
     >           *ACOS( (3.*ES-1.)/(2.*ES*E) )
     >          - (1.-ES)*(23.5*ES-6.5+1./ES)
     >          - 3.*(1.-6.*ES+4.*ES*ES)*ALOG(E)
C
C
C =====< Entry Point >==================================================
C
      WIDTH(0) = 0
      ALPHA    = GCOUPL(1)
      SIN2TW   = GCOUPL(2)
      ALPHAS   = GCOUPL(3)
      XCOLF    = 3.*(1. + ALPHAS/PI)
C
C (1) Leptonic decay width
C
      FACT     = ALPHA/(8.*SIN2TW*AMAS(10)**2)
      DO 100 I = 1, 3
        WIDTH(I) = 0
        BF     = BETAS(AMAS(I), AMH )
        IF( BF .LE. 0. ) GO TO 100
        WIDTH(I) = FACT * AMH * AMAS(I)**2 * (SQRT(BF))**3
        WIDTH(0) = WIDTH(0) + WIDTH(I)
100   CONTINUE
C
C (2) Hadronic decay width
C
      DO 200 I = 4, 9
        WIDTH(I) = 0
        BF     = BETAS(AMAS(I), AMH )
        IF( BF .LE. 0. ) GO TO 200
        WIDTH(I) = FACT * AMH * AMAS(I)**2 * (SQRT(BF))**3 * XCOLF
        WIDTH(0) = WIDTH(0) + WIDTH(I)
200   CONTINUE
C
C (3) decay width to W+W-
C
      WIDTH(10) = 0
      IF( NTYPE .EQ. 1 .OR.
C     >    AMH .LE. HTOWW(1,1     ) .OR.
     >    AMH .GE. HTOWW(1,NO$HWW) ) THEN
        BW = BETAS(AMAS(10), AMH)
        IF( BW .GT. 0. ) THEN
          XRS       = ( AMAS(10)/AMH )**2
          WIDTH(10) = FACT * 0.5 * AMH**3 * SQRT(BW)
     >            * ( 12.*XRS*XRS - 4.*XRS + 1. )
        ELSEIF( NVERT .NE. 0 ) THEN
C
C ... H --> Wff effect (W.Y.Keung et al.,PRD30(1984)248)
C
          IF( AMH .EQ. 2.*AMAS(10) ) AMH = AMH - 0.1
          EPS = AMAS(10)/AMH
          ES  = EPS*EPS
          WIDTH(10) = 3.*(4.*PI*ALPHA/SIN2TW)**2*AMH/(512.*PI*PI*PI)
     >              * FES(EPS,ES)
          WIDTH(10) = WIDTH(10)*1.E-3
        ENDIF
        IF( WIDTH(10).LE.0. ) WIDTH(10) = 0.0
        WIDTH(0)  = WIDTH(0) + WIDTH(10)
      ELSEIF( AMH .LE. HTOWW(1,1) ) THEN
        WIDTH(10)   = 0.
      ELSE
C
C .. Interpolate
C
        IMDL = NO$HWW/2
        IF( AMH .LE. HTOWW(1,IMDL) ) THEN
          I1 = 1
          I2 = IMDL
        ELSE
          I1 = IMDL
          I2 = NO$HWW
        ENDIF
 
        DO 310 I = I1+1, I2
          IF( AMH .GT. HTOWW(1,I) ) GO TO 310
          XMR       = (AMH-HTOWW(1,I-1))/(HTOWW(1,I)-HTOWW(1,I-1))
          WIDTH(10) = HTOWW(2,I-1) +
     >                (HTOWW(2,I)-HTOWW(2,I-1))*XMR
          GO TO 330
310     CONTINUE
        PRINT *,'%Error..WIDHIG.. Failed to find proper bin'
        PRINT *,' AMH=',AMH
        STOP
330     CONTINUE
        WIDTH(0) = WIDTH(0) + WIDTH(10)
      ENDIF
C
C (4) decay width to ZZ
C
      WIDTH(11) = 0
      IF( NTYPE .EQ. 1 .OR.
C     >    AMH .LE. HTOZZ(1,1     ) .OR.
     >    AMH .GE. HTOZZ(1,NO$HZZ) ) THEN
        BZ = BETAS(AMAS(11), AMH)
        IF( BZ .GT. 0. ) THEN
          XRS       = ( AMAS(11)/AMH )**2
          WIDTH(11) = FACT / 4.  * AMH**3 * SQRT(BZ)
     >              * ( 12.*XRS*XRS - 4.*XRS + 1. )
        ELSEIF( NVERT .NE. 0 ) THEN
          EPS = AMAS(11)/AMH
          ES  = EPS*EPS
          WIDTH(11) = (4.*PI*ALPHA/SIN2TW)**2*AMH
     >                /(2048.*PI*PI*PI*(1.-SIN2TW)**2)
     >                *(7.-40./3.*SIN2TW+160./9.*SIN2TW*SIN2TW)
     >                *FES(EPS,ES)
          WIDTH(11) = WIDTH(11)*1.E-3
        ENDIF
        IF( WIDTH(11).LE.0. ) WIDTH(11) = 0.0
        WIDTH(0)  = WIDTH(0) + WIDTH(11)
 
      ELSEIF( AMH .LE. HTOZZ(1,1) ) THEN
        WIDTH(11)   = 0.
C
C
C
C .. Interpolate
C
      ELSE
        IMDL = NO$HZZ/2
        IF( AMH .LE. HTOZZ(1,IMDL) ) THEN
          I1 = 1
          I2 = IMDL
        ELSE
          I1 = IMDL
          I2 = NO$HZZ
        ENDIF
 
        DO 410 I = I1+1, I2
          IF( AMH .GT. HTOZZ(1,I) ) GO TO 410
          XMR       = (AMH-HTOZZ(1,I-1))/(HTOZZ(1,I)-HTOZZ(1,I-1))
          WIDTH(11) = HTOZZ(2,I-1) +
     >                (HTOZZ(2,I)-HTOZZ(2,I-1))*XMR
          GO TO 430
410     CONTINUE
        PRINT *,'%Error..WIDHIG.. Failed to find proper bin for ZZ'
        PRINT *,' AMH=',AMH
        STOP
430     CONTINUE
        WIDTH(0) = WIDTH(0) + WIDTH(11)
      ENDIF
C
C
C (5) decay width to Gamma+Gamma
C
      WIDTH(12) = 0
      CF        = 0.
      DO 510 IF = 1, 10
        BETA = 4.*AMAS(IF)*AMAS(IF)/(AMH*AMH)
        IF( BETA .LE. 0. ) GO TO 510
        IF( BETA .LT. 1. ) THEN
          XLF = ALOG( (2.-BETA+2.*SQRT(1.-BETA))/BETA)
          CX = 0.5*(PI+II*XLF)
        ELSE
          CX = ATAN2(1.,SQRT(BETA-1.))
        ENDIF
#ifdef C
     PRINT *,' IF=',IF,' BETA=',BETA
#endif 
        CXSQ = CX*CX
        IF( IF .GE. 1 .AND. IF .LE. 3 ) THEN
          QF  = -1.
          COL = 1.
          CFI = -2.*BETA*( (1.-BETA)*CXSQ+1.)
        ELSEIF( IF .GE. 4 .AND. IF .LE. 6 ) THEN
          QF  = 2./3.
          COL = 3
          CFI = -2.*BETA*( (1.-BETA)*CXSQ+1.)
        ELSEIF( IF .GE. 7 .AND. IF.LE.9 ) THEN
          QF  = 1./3.
          COL = 3
          CFI = -2.*BETA*( (1.-BETA)*CXSQ+1.)
        ELSEIF( IF .EQ. 10 ) THEN
          QF  = 1.
          COL = 1.
          CFI = (2.+3.*BETA+3.*BETA*(2.-BETA)*CXSQ)
        ENDIF
        CF = CF + CFI*QF**2*COL
510   CONTINUE
#ifdef C
   WIDTH(12) = (ALPHA/(4*PI))**2 * ABS(CF)*CF
#endif 
      WIDTH(12) = (ALPHA/(4*PI))**2 * ABS(CF)**2
     >          * (PI*ALPHA/(16.*PI*AMAS(10)**2*SIN2TW)) * AMH**3
      WIDTH(0)  = WIDTH(0) + WIDTH(12)
#ifdef C
   PRINT *,' For Gluon-Gluon  '
#endif 
C
C (6) decay width to Gluon+gluon
C
      WIDTH(13) = 0
      CF        = 0.
      DO 610 IF = 4, 9
        IF( AMAS(IF) .LE. 0. ) GO TO 610
        XLAM = AMAS(IF)*AMAS(IF)/(AMH*AMH)
        IF(XLAM .LE. 0.25 ) THEN
          ETAP = 0.5 + SQRT( 0.25 - XLAM )
          ETAM = 0.5 - SQRT( 0.25 - XLAM )
          CXSQ = 0.5*(ALOG(ETAP)-ALOG(ETAM))**2 - PI*PI*0.5
     >         + II*PI*(ALOG(ETAP)-ALOG(ETAM))
        ELSE
          CXSQ = -2.*(ASIN(0.5/SQRT(XLAM)))**2
        ENDIF
        CFI = 3.*(2.*XLAM+XLAM*(4.*XLAM-1)*CXSQ)
        CF  = CF + CFI
610   CONTINUE
      WIDTH(13) = (PI*ALPHA)/(2.*AMAS(10)**2*SIN2TW)
     >          / (36.*PI)* AMH*AMH*AMH
     >          * (ALPHAS/PI)**2 * ABS(CF)**2
      WIDTH(0)  = WIDTH(0) + WIDTH(13)
C
C
      IF( LU .LE. 0 .OR. LU. GE. 100 ) RETURN
C
C ... Print Higgs width information.
C
      WRITE(LU,*) '  '
      WRITE(LU,*) ' Decay parameter of Higgs '
      WRITE(LU,910) AMH, GCOUPL
910   FORMAT('  Mass =',F10.3,' GeV: Alpha=',F10.6,
     >       ' Sin2(Tw)=',F10.6,' Alpha-S=',F10.6)
 
      WRITE(LU,920) WIDTH(0),(WIDTH(I),AMAS(I),I=1,11),
     >             (WIDTH(K),K=12,13)
920   FORMAT('  Decay width (Total)=',F10.4,/,
     >'  Partial width H --> e-   + e+    =',E12.5,' m(e)  =',E12.5,/,
     >'                  --> mu-  + mu+   =',E12.5,' m(mu) =',E12.5,/,
     >'                  --> tau- + tau+  =',E12.5,' m(tau)=',E12.5,/,
     >'                  --> u    + ubar  =',E12.5,' m(u)  =',E12.5,/,
     >'                  --> c    + cbar  =',E12.5,' m(c)  =',E12.5,/,
     >'                  --> t    + tbar  =',E12.5,' m(t)  =',E12.5,/,
     >'                  --> d    + dbar  =',E12.5,' m(d)  =',E12.5,/,
     >'                  --> s    + sbar  =',E12.5,' m(s)  =',E12.5,/,
     >'                  --> b    + bbar  =',E12.5,' m(b)  =',E12.5,/,
     >'                  --> W-   + W+    =',E12.5,' m(b)  =',E12.5,/,
     >'                  --> Z    + Z     =',E12.5,' m(b)  =',E12.5,/,
     >'                  --> gamma+ gamma =',E12.5,/,
     >'                  --> gluon+ gluon =',E12.5 )
      RETURN
C
 
      END
