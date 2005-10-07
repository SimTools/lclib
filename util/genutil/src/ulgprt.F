C***********************************************************************
C*                                                                     *
C*  -----------------------------------------====                      *
C*  Subroutine ULGPRT(LU, NRET)
C*  -----------------------------------------====                      *
C*                                                                     *
C*(Function)                                                           *
C*   List contents of Generator:Particle_list bank
C*                                                                     *
C*(Input)                                                              *
C*  LU   ; Logical unit number to print out.
C*(Output)                                                             *
C*  NRET  ; Return code.                                               *
C*                                                                     *
C*(Author)                                                             *
C*  A.Miyamoto  20-Sep-1994  Original version.                         *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE ULGPRT( LU , NRET)
C
      COMMON /SSWORK/ NOELM(500), RBUF(100)
C
C ==== < Entry Point >==================================================
C
C
      NRET = -1
      CALL TBNOEL(1, 'Generator:Particle_List',NTRK, NOELM)
      IF( NTRK .LE. 0 ) RETURN

      WRITE(LU,110) NTRK
110   FORMAT(1H ,'*** Generator:Particle_List *** ',I4,' tracks',/,
C      x123x234567x23456789x23x234567890x234567890x234567890
     >' Ser#  PID  Mass    Chg   Px(GeV)   Py(GeV)   Pz(GeV)',
C      x234567890x2345678x2345678x2345678x23x234x234',
     >'   E(GeV)   Vx(cm)  Vy(cm)  Vz(cm) #D Pnt Mot',
C      x2345678x234567890x234567890
     >'Trg(nsec) PLife(cm) DecayL(cm)')
C
      DO 100 I = 1, NTRK
         CALL TBGET(1,'Generator:Particle_List',NOELM(I),NW,RBUF, IRET)
         IF( IRET.LT.0 ) GO TO 100
         ISER = RBUF(1)
         IPID = RBUF(2)
         ICRG = RBUF(4)
         NDOU = RBUF(12)
         NPD  = RBUF(13)
         NMO  = RBUF(14)
         WRITE(LU,200) ISER, IPID, RBUF(3), ICRG, (RBUF(K),K=5,11),
     >   NDOU, NPD, NMO, (RBUF(K2),K2=15,17)
 200     FORMAT(1X,I3,I7,F9.4,I3,4F10.4,3F8.3,I3,I4,I4,F8.2,2F10.5)
 100  CONTINUE
      NRET = 0
      RETURN
      END
