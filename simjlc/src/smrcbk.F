C      INCLUDE (SMRCLS)
CC
C      INCLUDE (@SMREGO)
C      INCLUDE (@SMRHGO)
C      x2PI = 2.*ACOS(-1.)
C--
C  Initialize charged track smearing.
C--
#ifdef CC
  DSMPTK = (RCYTK(2)-RCYTK(1))/(NSMPTK+1)
#endif 
C--
C  Initialize EM cluster smearing.
C--
C      DPHIEM = x2PI/NPHIEM
C      THMNEM = ATAN2(RCYEM(2),ZCYEM(2))
C      THMXEM = ATAN2(RCYEM(2),ZCYEM(1))
C      DTHTEM = (THMXEM-THMNEM)/NTHTEM
C     DRADEM = (RCYEM(2)-RCYEM(1))/NRADEM
C--
C  Initialize HD cluster smearing.
C--
C     DPHIHD = x2PI/NPHIHD
C     THMNHD = ATAN2(RCYHD(2),ZCYHD(2))
C     THMXHD = ATAN2(RCYHD(2),ZCYHD(1))
C     DTHTHD = (THMXHD-THMNHD)/NTHTHD
C     DRADHD = (RCYHD(2)-RCYHD(1))/NRADHD
C--
C  That's it.
C--
C     CALL SMRCBK(1, IRET )
C     STOP
C     END
C
C     SUBROUTINE TBGET(ID, BNKNAM,NE, NW, NDATA,IRET)
C     CHARACTER*(*) BNKNAM
C     INTEGER*4 NDATA(*)
C
C     INTEGER*4  IHIT(2,14)
C     DATA  IHIT/ 1502, 54157,    2148, 54158,   107, 55156,
C    >           29505, 55157,   50158, 55158,  152, 55159,
C    >           270,   56156,   67030, 56157,  559,  56159,
C    >          117946, 56158,
C    >           3769, 57157,    6404,  57158,  196,  58157,
C    >           212, 58158 /
C
C     NW = 4
C     NDATA(1) = IHIT(1,NE)
C     NDATA(2) = IHIT(2,NE)
C     NDATA(3) = 1
C     NDATA(4) = 1
C     NRET = 1
C     RETURN
C     END
C
C     SUBROUTINE TBNOEL(ID,BNKNAM,NC,NOELM)
C     CHARACTER*(*) BNKNAM
C     INTEGER*4   NOELM(*)
C
C     NC = 14
C     DO 100 I = 1, 14
C       NOELM(I) = I
C 100   CONTINUE
C     RETURN
C     END
C     BLOCK DATA SMRBLK
C ==EXPAND 'T#FP.S#JLC.V2.FORT(@SMRTGO)'
C ==EXPAND 'T#FP.S#JLC.V2.FORT(@SMREGO)'
C ==EXPAND 'T#FP.S#JLC.V2.FORT(@SMRHGO)'
C ==EXPAND 'T#FP.S#JLC.V2.FORT(@SMRRES)'
C = =EXPAND 'T#FP.S#JLC.V2.FORT(@SMRRND)'
C ==EXPAND 'T#FP.S#JLC.V2.FORT(@SMRFLD)'
C ==EXPAND 'TAYM.S#JLC.FORT(@SMRCLP)'
C     COMMON /SMREGO/ NPHIEM, NTHTEM, NRADEM, DPHIEM, DTHTEM, DRADEM,
C    .                THMNEM, THMXEM, RCYEM(2), ZCYEM(2)
C     INTEGER*4       NPHIEM, NTHTEM, NRADEM
C     REAL   *4       DPHIEM, DTHTEM, DRADEM, THMNEM, THMXEM,
C    .                RCYEM, ZCYEM
C     DATA    NPHIEM /  240  /
C     DATA    NTHTEM /  120  /
C     DATA    NRADEM /   24  /
C     DATA    DPHIEM /    0. /
C     DATA    DTHTEM /    0. /
C     DATA    DRADEM /    0. /
C     DATA    THMNEM /    0. /
C     DATA    THMXEM /    0. /
C     DATA    RCYEM  /   45., 250. /
C     DATA    ZCYEM  / -300., 300. /
C     COMMON /SMRHGO/ NPHIHD, NTHTHD, NRADHD, DPHIHD, DTHTHD, DRADHD,
C    .                THMNHD, THMXHD, RCYHD(2), ZCYHD(2)
C     INTEGER*4       NPHIHD, NTHTHD, NRADHD
C     REAL   *4       DPHIHD, DTHTHD, DRADHD, THMNHD, THMXHD,
C    .                RCYHD, ZCYHD
C     DATA    NPHIHD /  240  /
C     DATA    NTHTHD /  120  /
C     DATA    NRADHD /   24  /
C     DATA    DPHIHD /    0. /
C     DATA    DTHTHD /    0. /
C     DATA    DRADHD /    0. /
C     DATA    THMNHD /    0. /
C     DATA    THMXHD /    0. /
C     DATA    RCYHD  /  60.0, 350. /
C     DATA    ZCYHD  / -400., 400. /
C
C     COMMON /SMRCLP/ CLSPAR(5,4)
C
C  /SMRCLP/ Parameter for Clustering.
C           CLSPAR(i,1) ; For Barrel EMC
C                 (i,2) ; For EndCap EMC
C                 (i,3) ; For Barrel HDC
C                 (i,4) ; For EndCap HDC
C*
C     DATA (CLSPAR(I,1),I=1,5)/  10., 0.65, 0.08, 1.20, 0.3 /
C     DATA (CLSPAR(I,2),I=1,5)/  10., 0.65, 0.08, 1.20, 0.3 /
C     DATA (CLSPAR(I,3),I=1,5)/  10., 0.65, 0.08, 1.20, 0.3 /
C     DATA (CLSPAR(I,4),I=1,5)/  10., 0.65, 0.08, 1.20, 0.3 /
C     END
C***********************************************************************
C*
C*  -----------------
C*  Subroutine SMRCBK(NDEV,NRET)
C*  -----------------
C*(Function)
C*  Clustering Elemag/Hadron Calorimeter signal
C*(Input)
C*  NDEV  : = 1 for EMC, 2=HDC
C*
C*  Input bank
C*       'Production:EMC;Hit_Cell'
C*       'Production:HDC;Hit_Cell'
C*  Output bank
C*       'Produtcion:EMC;Cluster'
C*       'Production:HDC;Cluster'
C*  Assumes common /SMRCLP/, /SMREGO/, /SMRHGO/ are prepared by
C*  Subroutines SMRPRD and SMRRIN
C*(Output)
C*  NRET < 0 in case of error.
C*
C*(Author)
C*  A. Miyamoto
C*  92/06/24  K.Fujii          A bug fixed. -Z endcap treatment
C*                             has been wrong since the update
C*                             on 92/06/11.
C*  92/07/06  A.Miyamoto       Length of PNTBNK increased from 24 to 48
C*  92/07/09  A.Miyamoto       A bug fixed.  in calculating cluster pos.
C*  03/02/14  A.Miyamoto       Increase NCxBUF from 30 to 300
C***********************************************************************
C*
      SUBROUTINE SMRCBK( NDEV, NRET )
C
#include "smrclp.inc"
#include "smrego.inc"
#include "smrhgo.inc"
C
C      INCLUDE (@SMRCLP)
C      INCLUDE (@SMREGO)
C      INCLUDE (@SMRHGO)
C
      PARAMETER (MXxCEL=2000)
      PARAMETER (NCxBUF=300)
      PARAMETER (MXxCLS=1000)
      COMMON/SSWORK/ NOELM(MXxCEL), NHITDV(3), NTEMP(50),
     >               NHCELL(NCxBUF,MXxCEL,3), NSORT(MXxCEL),
     >               INOCLS(3),
     >               NCDATA(3,MXxCEL,3), NCLSPT(2,MXxCLS,3)
C
C  ... NHCELL(i,j,1) ; To store Barrel Cell info.
C            (i,j,2) ; +Z End cap.
C            (i,j,3) ; -Z ENd cap.
C      Corresponding Element # to Hit_Cell is stored in NCxBUF-th word.
C  ... NHITDV(1)     ; # of Cell in Barrel calorimeter.
C            (2)     ; # of Cell in endcap +Z
C            (3)     ; # of Cell in endcap -Z
C
      REAL*4         CLSDAT(100)
      INTEGER*4      NCLSDT(100)
      EQUIVALENCE   (CLSDAT(1), NCLSDT(1))
C
      REAL*4         GEOEM(17), GEOHD(17), GEONOW(17)
      EQUIVALENCE   (GEOEM(1), NPHIEM)
      EQUIVALENCE   (GEOHD(1), NPHIHD)
      EQUIVALENCE   (GEONOW(1), LPHI),
     >              (GEONOW(2), LTHA), (GEONOW(3), LRAD )
      INTEGER*4      NZEEDT(3)/0, 1., -1./
C
      CHARACTER*24   INBNK (2)/'Production:EMC;Hit_Cell',
     >                         'Production:HDC;Hit_Cell'/
      CHARACTER*24   OUTBNK(2)/'Production:EMC;Cluster',
     >                         'Production:HDC;Cluster'/
      CHARACTER*48   PNTBNK(2)/'Production:EMC;Cluster_to_Cell',
     >                         'Production:HDC;Cluster_to_Cell'/
      CHARACTER*4    DEVNAM(2)/'EMC', 'HDC'/
C
      DATA  PI/3.141592654/
C
C ====< Entry Point >==================================================
C
C ---------------------------------------------------------------------
C (1) Get Data, and Prepare NHCELL
C ---------------------------------------------------------------------
C
#ifdef DBG
      WRITE(6,'(1H1)')
      PRINT *,' SMRCBK called with NDEV=',NDEV
#endif 
      NRET = 0
      CALL TBNOEL(1,INBNK(NDEV), NCLS, NOELM )
      IF( NCLS .LE. 0 ) RETURN
#ifdef DBG
      PRINT *,' NCLS =',NCLS
#endif 
C
      NHITDV(1) = 0
      NHITDV(2) = 0
      NHITDV(3) = 0
      DO 100 I = 1, NCLS
        IC = NOELM(I)
        CALL TBGET(1,INBNK(NDEV),IC,NW,NTEMP,IRET)
        IF( NW .GT. NCxBUF - 1 ) THEN
           INW = NCxBUF - 1
           PRINT *,'%Warning SMRCBK..Too many track contribute to the ',
     >             ' cell signal. #Track will be trancated to ',INW
           PRINT *,'         NW =',NW,' in ',DEVNAM(NDEV),':Hit_Cell'
           NRET = 1
           NW = NCxBUF - 1
        ENDIF
        ITYP = 1
        IF( NTEMP(2) .GT. 1000000 ) ITYP = 2
        IF( NTEMP(2) .LT.-1000000 ) ITYP = 3
        NHITDV(ITYP) = NHITDV(ITYP) + 1
#ifdef DBG
        print *,'In 100 Loop  i=',I,' ITYP=',ITYP,' NHITDV=',
     >          NHITDV(ITYP),'NW=',NW,' IC=',IC
#endif
        DO 120 IW = 1, NW
          NHCELL(IW,NHITDV(ITYP),ITYP)= NTEMP(IW)
#ifdef DBG
          PRINT *,' NTEMP(',IW,')=',NTEMP(IW)
#endif
120     CONTINUE
        NHCELL(NCxBUF,NHITDV(ITYP),ITYP) = IC
100   CONTINUE
 
#ifdef DBG
      PRINT *,' NHITDV(1) =',NHITDV(1)
      PRINT *,'       (2) =',NHITDV(2)
      PRINT *,'       (3) =',NHITDV(3)
      PRINT *,' Barrel '
      DO 102 I = 1, NHITDV(1)
        PRINT *,I,NHCELL(1,I,1), NHCELL(2,I,1),' .. ',
     >            NHCELL(NCxBUF,I,1)
102   CONTINUE
      PRINT *,' Endcap(+Z)'
      DO 103 I = 1, NHITDV(2)
        PRINT *,I,NHCELL(1,I,2), NHCELL(2,I,2),' .. ',
     >            NHCELL(NCxBUF,I,2)
103   CONTINUE
      PRINT *,' Endcap(-Z)'
      DO 104 I = 1, NHITDV(3)
        PRINT *,I,NHCELL(1,I,3), NHCELL(2,I,3),' .. ',
     >            NHCELL(NCxBUF,I,3)
104   CONTINUE
#endif 
C
C ---------------------------------------------------------------------
C (2) Sort Cluster, Clustering
C ---------------------------------------------------------------------
C
      IF( NDEV .EQ. 1 ) THEN
        CALL UVCOPY(17, GEOEM, GEONOW)
      ELSE
        CALL UVCOPY(17, GEOHD, GEONOW )
      ENDIF
C
C     PRINT *,' '
C     PRINT *,' After sorting. ....'
C
      NUMCLS = 0
C
C ID = 1  ; Barrel
C    = 2  ; +Z end cap
C    = 3  ; -Z end cap
C
      DO 200 ID = 1, 3
        INOCLS(ID) = 0
        IF( NHITDV(ID) .LE. 0 ) GO TO 200
        CALL USORTI(NHITDV(ID), NCxBUF, 1, NHCELL(1,1,ID), NSORT)
        DO 210 I = 1, NHITDV(ID)
          IP = NSORT(NHITDV(ID)-I+1)
          IF( ID .EQ. 1 ) THEN
            NCDATA(1,I,ID) =  NHCELL(2,IP,ID)
          ELSEIF( ID .EQ. 2 ) THEN
             NCDATA(1,I,ID) =  NHCELL(2,IP,ID) - 1000000
          ELSEIF( ID .EQ. 3 ) THEN
             NCDATA(1,I,ID) = -NHCELL(2,IP,ID) - 1000000
          ENDIF
          NCDATA(2,I,ID) = NHCELL(1,IP,ID)
          NCDATA(3,I,ID) = NHCELL(NCxBUF,IP,ID) + 10000*IP
#ifdef C
       PRINT *,' I=',I,(NCDATA(K,I,ID),K=1,3)
#endif 
210     CONTINUE
C
        KPHI = LPHI
        KTHA = LTHA
        IF( ID.GE.2 ) KTHA = LRAD
C
        IPARD = 1
        IF( ID.GE. 2 ) IPARD=2
        IP    = 2*(NDEV-1) + IPARD
        EWFACT = CLSPAR(5,IP)
C
#ifdef DBG
        PRINT *,' LTHA =',LTHA,' LPHI =',LPHI
        PRINT *,' IP =',IP
#endif 
 
        CALL SMRCLS(NHITDV(ID), NCDATA(1,1,ID), KTHA, LPHI,
     >              CLSPAR(1,IP),
     >              MXxCLS, INOCLS(ID), NCLSPT(1,1,ID) )
 
#ifdef DBG
        PRINT *,' '
        PRINT *,' After Clustering...'
        PRINT *,' # Cluster =',INOCLS(ID)
        DO 202 I = 1, INOCLS(ID)
           PRINT *,' Cluster # ',I
           DO 203 J = NCLSPT(1,I,ID), NCLSPT(2,I,ID)
             PRINT *,' J=',J, (NCDATA(K,J,ID),K=1,3)
  203        CONTINUE
  202     CONTINUE
#endif 
C
C ----------
C (3) Energy weighted cluster position.
C ----------
C
C  Use array NSORT to store element # of contributing Cell.
C
#ifdef DBG
      PRINT *,' Before DO 300... INOCLS(ID)=',INOCLS(ID),' ID=',ID
#endif 
        DO 300 IC = 1, INOCLS(ID)
          XETH = 0
          XEPHI= 0
          XESUM= 0
          XEWGT= 0
          J = NCLSPT(1,IC,ID)
          IPHI = MOD( IABS(NCDATA(1,J,ID)), 1000 )
          IMOD = 0
          IF( IPHI .LE. LPHI/4 .OR. IPHI .GE. LPHI*3/4 ) IMOD = 1
 
#ifdef C
       PRINT *,' IC =',IC
#endif 
 
          NOCEL = 0
          NOTRK = 0
          DO 360 I = 1, 30
            CLSDAT(I) = 0.
360       CONTINUE
          IPRINT = 1
          DO 310 J = NCLSPT(1,IC,ID), NCLSPT(2,IC,ID)
            ICELL = NCDATA(1,J,ID)
            IPHI  = MOD( IABS(ICELL), 1000 )
            IF( IMOD .EQ. 1 .AND. IPHI .LE. LPHI/2 ) IPHI = IPHI+LPHI
            ITHA  = MOD( IABS(ICELL), 1000000 )/1000
            ENG   = FLOAT(NCDATA(2,J,ID))*1.E-3
            ENGW  = ENG**EWFACT
            XESUM = XESUM + ENG
            XEWGT = XEWGT + ENGW
            XETH  = XETH  + ENGW*(FLOAT(ITHA)+0.5)
            XEPHI = XEPHI + ENGW*(FLOAT(IPHI)+0.5)
 
#ifdef C
         PRINT *,' J =',J,' ICELL =',ICELL,' IPHI,ITHA=',IPHI,ITHA,
  >                       ' ENG =',ENG
#endif 
C
C ... Store Contributing Cell and Track information.
            NOCEL = NOCEL + 1
            NSORT(NOCEL) = MOD(NCDATA(3,J,ID),10000)
            IPTRK = NCDATA(3,J,ID)/10000
            DO 320 IT = 1, NHCELL(3,IPTRK,ID)
              DO 330 JT = 1, NOTRK
                IF( NCLSDT(15+JT)
     >                        .EQ. NHCELL(3+IT,IPTRK,ID) ) GO TO 320
330           CONTINUE
              NOTRK = NOTRK + 1
              IF( NOTRK .GT. NCxBUF-4 ) THEN
                IF( IPRINT .EQ. 1 ) THEN
                PRINT *,'%Warning..SMRCBK  Too many generator track ',
     >              'contributing to this cluster..Track Information',
     >              ' is trancated at ',NCxBUF-4, ' Track.'
                PRINT *,' In ',DEVNAM(NDEV),';Cluster'
                NRET = 1
                IPRINT = 0
                ENDIF
                NOTRK = NCxBUF-4
              ENDIF
              NCLSDT(15+NOTRK) = NHCELL(3+IT,IPTRK,ID)
320         CONTINUE
 
 
310       CONTINUE
          NCLSDT(15) = NOTRK
 
#ifdef C
       PRINT *,' XESUM =',XESUM
#endif 
 
 
C
C Prepare data for output
C
          APHI   = XEPHI/XEWGT/FLOAT(LPHI)
          IF( APHI.GT. 1. ) APHI = APHI - 1.
          PHI    = APHI*2.*PI
          NCELPH = ANINT(APHI*FLOAT(LPHI))
          NCELTH = ANINT(XETH/XEWGT)
          IF( ID .EQ. 1 ) THEN
            XI     = GEONOW(5)*XETH/XEWGT + GEONOW(8)
            XTH    = (1. - EXP(2*XI))/(1.+EXP(2*XI))
            CSTH   = XTH
            SNTH   = SQRT( (1.-CSTH)*(1.+CSTH) )
            RAD    = GEONOW(15)
            CLSDAT(7) = RAD*CSTH/SNTH
          ELSEIF( ID.EQ.2 ) THEN
            ETA    = GEONOW(12) + GEONOW(7)*XETH/XEWGT
            SNTH   = (EXP(2*ETA)-1.)/(EXP(2*ETA)+1.)
            CSTH   = SQRT( (1.-SNTH)*(1.+SNTH) )
            CLSDAT(7) = GEONOW(17)
            RAD       = CLSDAT(7)*SNTH/CSTH
          ELSE
            ETA    = GEONOW(10) + GEONOW(6)*XETH/XEWGT
C>>>
            SNTH   =  (EXP(2*ETA)-1.)/(EXP(2*ETA)+1.)
            CSTH   = -SQRT( (1.-SNTH)*(1.+SNTH) )
C>>>
            CLSDAT(7) = GEONOW(16)
            RAD       = CLSDAT(7)*SNTH/CSTH
C           PRINT *,' '
C           PRINT *,' ID=',ID
C           PRINT *,' GEONOW(8)=',GEONOW(8),' GEONOW(9)=',GEONOW(9)
C           PRINT *,' GEONOW(5)=',GEONOW(5)
C           PRINT *,' XETH =',XETH,' XEWGT=',XEWGT
C           PRINT *,' NCELPH,NCELTH=',NCELPH,NCELTH
C           PRINT *,' RAD=',RAD,' CLSDAT(7)=',CLSDAT(7)
C           PRINT *,' XTH =',XTH
          ENDIF
          DIRX = SNTH*COS(PHI)
          DIRY = SNTH*SIN(PHI)
          DIRZ = CSTH
 
          CLSDAT(1) = XESUM*DIRX
          CLSDAT(2) = XESUM*DIRY
          CLSDAT(3) = XESUM*DIRZ
          CLSDAT(4) = XESUM
          CLSDAT(5) = RAD*COS(PHI)
          CLSDAT(6) = RAD*SIN(PHI)
 
          NCLSDT(14) = NCELTH*1000 + NCELPH
          IF( ID.GE.2 ) NCLSDT(14) = NZEEDT(ID)*(1000000+NCLSDT(14))
 
          NW = 15 + NOTRK
          NUMCLS = NUMCLS + 1
          CALL TBPUT(1,OUTBNK(NDEV),NUMCLS, NW, CLSDAT, IRET)
          CALL TBPUT(1,PNTBNK(NDEV),NUMCLS, NOCEL, NSORT, IRET )
 
300     CONTINUE
C
C
200   CONTINUE
C
      RETURN
      END
