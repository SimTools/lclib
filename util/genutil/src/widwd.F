C***********************************************************************C
C*
C*  -----------------------------------------------------======
C*   Subroutine WIDWD( LU, AMW, COUPLE, AMAS, VKMMTX, WIDTH )
C*  -----------------------------------------------------======
C*(Function)
C*   Calculate decay width of
C*             W --> f + fbar
C*                   f = (ne, nmu, ntau , e, mu, tau, u, c, t, d, s, b )
C*   Same as WIDW, but REAL*8 interface.
C*(Input)
C*   LU    ; Print out width when 0 < LU < 99
C*   AMW   ; Z mass (GeV)
C*   COUPLE(3) ; coupling data.
C*        (1) = ALPHA
C*        (2) = SIN2TW
C*        (3) = ALPHA(S)
C*   AMAS( 9)  ; Particle masses.
C*            1=e, 2=mu, 3=tau, 4=u, 5=c, 6=t, 7=d, 8=s, 9=b,
C*   VKMMTX(3,3) ; KM Matrix.
C*            VKMMTX(i,j) i,j= (u,d), (c,s), (t,b)
C*(Output)
C*   WIDTH(0: 9)  ; Decay width
C*           (0) = Total
C*           (1) = Partial width ( W --> e + n(e)
C*           (2) =               (   --> mu + n(mu)
C*           (3) =               (   --> tau + n(tau)
C*           (4) =               (   --> u + d
C*           (5) =               (   --> u + s
C*           (6) =               (   --> u + b
C*           (7) =               (   --> c + d
C*           (8) =               (   --> c + s
C*           (9) =               (   --> c + b
C*          (10) =               (   --> t + d
C*          (11) =               (   --> t + s
C*          (12) =               (   --> t + b
C*
C*(Author)
C*   A. Miyamoto 11-Aug-1990  Original version.
C*               18-Mar-1993  Real*8 version prepared.
C*
C***********************************************************************
C
      SUBROUTINE WIDWD( LU, AMW, COUPLE, AMAS, VKMMTX, WIDTH)
C
      IMPLICIT REAL*8 (A-H,O-Z)

      DIMENSION WIDTH(0:12)
      DIMENSION AMAS(9), VKMMTX(3,3)
      DIMENSION COUPLE(3)
C
      DATA      PI/3.141592654/
 
C
C =====< Entry Point >==================================================
C
C (1) Initialization.
C
      ALPHA    = COUPLE(1)
      SIN2TW   = COUPLE(2)
      ALPHAS   = COUPLE(3)
      WIDTH(0) = 0
      CS2TW = 1.D0 - SIN2TW
      FACT  = ( 4.*PI*ALPHA/(8.*SIN2TW) )
     >        / ( 32. * PI*PI *AMW*AMW) * 4. / 3. * 4.*PI
      COLF  = 3.*(1. + ALPHAS/PI)
C
C
C (2) Hadronic decay width
C
      DO 200 IG1 = 1, 2
      DO 200 IG2 = 1, 3
        AM1 = AMAS(3 + IG1)
        AM2 = AMAS(6 + IG2)
C
        IF( AM1 + AM2 .GT. AMW ) GO TO 200
        CA  = -1.
        CV  =  1.
        PF1 = SQRT( (AMW*AMW-(AM1+AM2)**2)*(AMW*AMW-(AM1-AM2)**2) )/
     .             (2.*AMW)
        TTUTT=(CV**2*(2.*AMW**4-AMW**2*AM2**2+6.*AMW**2*AM2*AM1-
     .   AMW**2*AM1**2-AM2**4+2.*AM2**2*AM1**2-AM1**4)+CA**2*(2.*
     .   AMW**4-AMW**2*AM2**2-6.*AMW**2*AM2*AM1-AMW**2*AM1**2-AM2
     .   **4+2.*AM2**2*AM1**2-AM1**4))/(2.*AMW**2)
        IP =  3*IG1 + IG2
        WIDTH( IP ) = TTUTT * FACT * COLF * PF1 * VKMMTX(IG1,IG2)**2
        WIDTH( 0 )  = WIDTH(0) + WIDTH(IP)
200   CONTINUE
      WIDTH(10) = 0
      WIDTH(11) = 0
      WIDTH(12) = 0
C
C
C (3) Leptonic decay width
C
      DO 300 IG = 1, 3
        AM1 = 0
        AM2 = AMAS(IG)
C
        CA  = -1.0
        CV  =  1.0
        IF( AM1 + AM2 .GT. AMW ) GO TO 300
        PF1 = SQRT( (AMW*AMW-(AM1+AM2)**2)*(AMW*AMW-(AM1-AM2)**2) )/
     .             (2.*AMW)
        TTUTT=(CV**2*(2.*AMW**4-AMW**2*AM2**2+6.*AMW**2*AM2*AM1-
     .   AMW**2*AM1**2-AM2**4+2.*AM2**2*AM1**2-AM1**4)+CA**2*(2.*
     .   AMW**4-AMW**2*AM2**2-6.*AMW**2*AM2*AM1-AMW**2*AM1**2-AM2
     .   **4+2.*AM2**2*AM1**2-AM1**4))/(2.*AMW**2)
 
        WIDTH(IG) = TTUTT * FACT * PF1
        WIDTH(0) = WIDTH(0) + WIDTH(IG)
 
300   CONTINUE
C
C ---------------
C (4) Print results.
C ---------------
C
      IF( LU .LE. 0 .OR. LU .GT. 99 ) RETURN
 
      WRITE(LU,*) '  '
      WRITE(LU,*) ' Decay parameter of W '
      WRITE(LU,410) AMW, COUPLE
410   FORMAT('  Mass =',F10.3,' GeV: Alpha=',F10.6,
     >       ' Sin2(Tw)=',F10.6,' Alpha-S=',F10.6)
 
      WRITE(LU,420) WIDTH(0),(WIDTH(I),AMAS(I),I=1,3)
420   FORMAT('  Decay width (Total)=',F10.4,/,
     >'  Partial width W --> e  + n(e)  =',E12.5,' m(e)  =',E12.5,/,
     >'                  --> mu + n(mu) =',E12.5,' m(mu) =',E12.5,/,
     >'                  --> tau+ n(tau)=',E12.5,' m(tau)=',E12.5)
      WRITE(LU,430) WIDTH(4),AMAS(4),AMAS(7),VKMMTX(1,1)
     >             ,WIDTH(5),       AMAS(8),VKMMTX(1,2)
     >             ,WIDTH(6),       AMAS(9),VKMMTX(1,3)
     >             ,WIDTH(7),AMAS(5),AMAS(7),VKMMTX(1,1)
     >             ,WIDTH(8),       AMAS(8),VKMMTX(1,2)
     >             ,WIDTH(9),       AMAS(9),VKMMTX(1,3)
430   FORMAT(
     >'                  --> u  + d     =',E12.5,' m(u)  =',E12.5,
     >                         ' m(d)  =',E12.5,' KM(u,d)=',E12.5,/,
     >'                  --> u  + s     =',E12.5,20X,
     >                         ' m(s)  =',E12.5,' KM(u,s)=',E12.5,/,
     >'                  --> u  + b     =',E12.5,20X,
     >                         ' m(b)  =',E12.5,' KM(u,b)=',E12.5,/,
     >'                  --> c  + d     =',E12.5,' m(c)  =',E12.5,
     >                         ' m(d)  =',E12.5,' KM(c,d)=',E12.5,/,
     >'                  --> c  + s     =',E12.5,20X,
     >                         ' m(s)  =',E12.5,' KM(c,s)=',E12.5,/,
     >'                  --> c  + b     =',E12.5,20X,
     >                         ' m(b)  =',E12.5,' KM(c,b)=',E12.5)
C
 
      RETURN
      END
