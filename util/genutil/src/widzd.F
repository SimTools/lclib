C***********************************************************************
C*
C*  ---------------------------------------------======
C*   Subroutine WIDZD( LU, AMZ, COUPLE, AMAS,  WIDTH )
C*  ---------------------------------------------======
C*(Function)
C*   Calculate decay width of
C*             Z --> f + fbar
C*                   f = (ne, nmu, ntau , e, mu, tau, u, c, t, d, s, b )
C*   Same as WIDZ, but REAL*8 arguments.
C*(Input)
C*   LU    ; 0 < LU < 100 to print out width, LU is logical unit #.
C*   AMZ   ; Z mass (GeV)
C*   COUPLE(3) : Coupling constants.
C*         (1) = ALPHA ; Alpha
C*         (2) = SIN2TW; Weignberg angle.
C*         (3) = ALpha(S)
C*   AMAS( 9)  ; Particle masses.
C*            1=e, 2=mu, 3=tau, 4=u, 5=c, 6=t, 7=d, 8=s, 9=b,
C*(Output)
C*   WIDTH(0:12)  ; Decay width
C*           (0) = Total
C*           (1) = Partial width ( Z --> ne + nebar )
C*           (2) =               (   --> nmu + nmubar )
C*           (3) =               (   --> ntau + ntaubar )
C*           (4) =               (   --> e+ + e-)
C*           (5) =               (   --> mu+ + mu-)
C*           (6) =               (   --> tau+ + tau- )
C*           (7) =               (   --> u + ubar )
C*           (8) =               (   --> c + cbar )
C*           (9) =               (   --> t + tbar )
C*          (10) =               (   --> d + dbar )
C*          (11) =               (   --> s + sbar )
C*          (12) =               (   --> b + bbar )
C*
C*(Author)
C*   A. Miyamoto  11-Aug-1990   Original version.
C*                18-Mar-1993   Real*8 version prepared.
C*
C***********************************************************************
C
      SUBROUTINE WIDZD( LU, AMZ, COUPLE, AMAS, WIDTH)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION WIDTH(0:12)
      DIMENSION AMAS(9)
      DIMENSION COUPLE(3)
C
      INTEGER*4 NQ(4)/  0, -3,  2, -1 /
      INTEGER*4 NA(4)/ -1,  1, -1,  1 /
      REAL*8    QF(4), GA(4), GV(4)
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
      DO 100 IY = 1,4
        QF(IY) = DFLOAT(NQ(IY))/3.D0
        GA(IY) = DFLOAT(NA(IY))/4.D0
        GV(IY) = -GA(IY) - QF(IY)*SIN2TW
100   CONTINUE
C
      CS2TW = 1.D0 - SIN2TW
      FACT  = ( 4.*PI*ALPHA/(SIN2TW*CS2TW) )
     >      / ( 32. * PI*PI *AMZ*AMZ) * 4. / 3. * 4.*PI
      COLF  = 3.*(1.+ALPHAS/PI)
C
C
C (2) Hadronic decay width
C
      DO 200 IY = 3, 4
      DO 200 IG = 1, 3
        IP  = 3*(IY-2) + IG
        AM1 = AMAS( IP )
        AM2 = AM1
C
        WIDTH(IP+3) = 0
        IF( AM1 + AM2 .GT. AMZ ) GO TO 200
        CA  = GA(IY)
        CV  = GV(IY)
        PF1 = SQRT( ( 0.5*AMZ-AM1)*(0.5*AMZ + AM1 ) )
        TTUTT=(CV**2*(2.*AMZ**4-AMZ**2*AM2**2+6.*AMZ**2*AM2*AM1-
     .   AMZ**2*AM1**2-AM2**4+2.*AM2**2*AM1**2-AM1**4)+CA**2*(2.*
     .   AMZ**4-AMZ**2*AM2**2-6.*AMZ**2*AM2*AM1-AMZ**2*AM1**2-AM2
     .   **4+2.*AM2**2*AM1**2-AM1**4))/(2.*AMZ**2)
        WIDTH( IP+3 ) = TTUTT*FACT*COLF*PF1
        WIDTH( 0 )    = WIDTH(0) + WIDTH(IP+3)
200   CONTINUE
C
C
C (3) Leptonic decay width
C
      DO 300 IY = 1, 2
      DO 300 IG = 1, 3
        IF( IY .EQ. 1 ) THEN
          IP = IG
          AM1 = 0
        ELSE
          IP  = IG + 3
          AM1 = AMAS(IG)
        ENDIF
        AM2 = AM1
        WIDTH(IP) = 0
C
        CA  = GA(IY)
        CV  = GV(IY)
        IF( AM1 + AM2 .GT. AMZ ) GO TO 300
        PF1 = SQRT( ( 0.5*AMZ-AM1)*(0.5*AMZ + AM1 ) )
        TTUTT=(CV**2*(2.*AMZ**4-AMZ**2*AM2**2+6.*AMZ**2*AM2*AM1-
     .   AMZ**2*AM1**2-AM2**4+2.*AM2**2*AM1**2-AM1**4)+CA**2*(2.*
     .   AMZ**4-AMZ**2*AM2**2-6.*AMZ**2*AM2*AM1-AMZ**2*AM1**2-AM2
     .   **4+2.*AM2**2*AM1**2-AM1**4))/(2.*AMZ**2)
 
        WIDTH(IP) = TTUTT * FACT * PF1
        WIDTH(0) = WIDTH(0) + WIDTH(IP)
 
300   CONTINUE
C
C    Print out results.
C
C
C
      IF( LU .LE. 0 .OR. LU .GT. 99 ) RETURN
 
      WRITE(LU,*) '  '
      WRITE(LU,*) ' Decay parameter of Z '
      WRITE(LU,410) AMZ, COUPLE
410   FORMAT('  Mass =',F10.3,' GeV: Alpha=',F10.6,
     >       ' Sin2(Tw)=',F10.6,' Alpha-S=',F10.6)
 
      WRITE(LU,420) WIDTH(0),(WIDTH(I),I=1,3)
420   FORMAT('  Decay width (Total)=',F10.4,/,
     >'  Partial width Z --> n(e)  +n(e)  =',E12.5,/,
     >'                  --> n(mu) +n(mu) =',E12.5,/,
     >'                  --> n(tau)+n(tau)=',E12.5)
      WRITE(LU,430) (WIDTH(K),AMAS(K-3),K=4,12)
430   FORMAT(
     >'                  --> e-   + e+    =',E12.5,' m(e)  =',E12.5,/,
     >'                  --> mu-  + mu+   =',E12.5,' m(mu) =',E12.5,/,
     >'                  --> tau- + tau+  =',E12.5,' m(tau)=',E12.5,/,
     >'                  --> u    + ubar  =',E12.5,' m(u)  =',E12.5,/,
     >'                  --> c    + cbar  =',E12.5,' m(c)  =',E12.5,/,
     >'                  --> t    + tbar  =',E12.5,' m(t)  =',E12.5,/,
     >'                  --> d    + dbar  =',E12.5,' m(d)  =',E12.5,/,
     >'                  --> s    + sbar  =',E12.5,' m(s)  =',E12.5,/,
     >'                  --> b    + bbar  =',E12.5,' m(b)  =',E12.5)
C
 
      RETURN
      END
