C***********************************************************************
C*==================================                                   *
C*    SUBROUTINE BSHGET( ID, MXxBUF, NUMTDR , XHTDR  )
C*==================================                                   *
C*((Purpose))                                                          *
C*     Get the contents of ID-th histogram data.                       *
C*((Input))                                                            *
C*     ID   : Historgram ID                                            *
C*     MXxBUF ; Size of output buffer.                                 *
C*((OUTPUT)
C*     NUMTDR ; # of histogram bin.
C*     XHTDR(3,50)  ; Histogram data, X, Y, and DY
C*((Author))                                                           *
C*      S.Kawabata   June '90 at KEK                                   *
C*      A.Miyamoto   June '91 at KEK                                   *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BSHGET( ID, MXxBUF, NUMTDR, XHTDR )
C
      REAL*8     XHTDR(3,MXxBUF)
C
      REAL*8         SI,SI2,SWGT,SCHI,SCALLS,ATACC,WGT
      COMMON /BASE3/ SI,SI2,SWGT,SCHI,SCALLS,ATACC,NSU,IT,WGT
 
      PARAMETER ( ILH = 50, IDH = 50 )
      INTEGER*4 XHASH,DHASH,MAXL,NHIST,MAPL,IFBASE,MAXD,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(ILH,13),DHASH(IDH,14),IFBASE(ILH),
     .              MAXL, NHIST, MAPL(4,ILH),
     .              MAXD, NSCAT, MAPD(4,IDH),
     .              NW
      PARAMETER (MMIST =20, MMCAT = 10)
      PARAMETER ( ISL = 281*MMIST + 2527*MMCAT + 281 )
      COMMON/PLOTB/ BUFF( ISL )
      INTEGER  IBUF( ISL )
      EQUIVALENCE (IBUF(1),BUFF(1))
      DATA IPRNT / 0/
 
      NUMTDR  = 0
C
C       PRINT *,' ID =',ID
C       PRINT *,' NHIST =',NHIST
 
      IF( NHIST .GT. 0 ) THEN
          IF( IPRNT .EQ. 0 ) THEN
              WRITE(6,9000)
 9000         FORMAT(/1H1)
              IPRNT = 1
          ENDIF
C
          I  = IABS(MOD( ID, 13 )) + 1
          PRINT *,' I =',I
          PRINT *,' XHASH(1,I) =',XHASH(1,I)
          IF( XHASH(1, I) .EQ. 1 ) THEN
            IF( ID .EQ. MAPL( 1, XHASH(2,I))) THEN
                IHIST = XHASH(2,I)
                GO TO 200
            ENDIF
          ELSEIF( XHASH(1, I) .GT. 1 ) THEN
            DO 100 K = 2, XHASH(1,I)+1
               IF( ID .EQ. MAPL( 1, XHASH(K,I))) THEN
                   IHIST = XHASH(K,I)
                   GO TO 200
               ENDIF
  100       CONTINUE
          ENDIF
      ENDIF
 
      WRITE(6,9100) ID
 9100 FORMAT(1X,'************* Warning from XHGET  *************',
     .      /1X,' Histogram ID(',I5,' ) is illegal.',
     .      /1X,' This call is neglected.',
     .      /1X,'**********************************************')
      RETURN
 
 
 
  200 NTOTAL= SCALLS
      IP1   = MAPL(2,IHIST)
      XMIN  = BUFF(IP1)
      XMAX  = BUFF(IP1+1)
      NXBIN = IBUF(IP1+2)
      DEV   = BUFF(IP1+3)
      IP2   = MAPL(3,IHIST)
      IP3   = MAPL(4,IHIST)
 
C      WRITE(6,9200) ID,LUNIT,(BUFF(I),I=IP3+1,IP3+15),
C     .              NTOTAL,NXBIN,DEV
 9200 FORMAT(/1H ,
     .      1X,'** Histogram ID(',I5,' ) was saved in Unit(',I2,') **',
     .      /1X,'Title : ',15A4,
     .      /1X,'Entries     =',I10,
     .      /1X,'No. of bins =',I10,'  Width =',G13.4)
 
C      WRITE( LUNIT, 9300) ID
C 9300 FORMAT('(...... ID =',I5,' ......')
 
C      WRITE( LUNIT, 9400)
C 9400 FORMAT(1X,'NEW FRAME',
C     .      /1X,'SET FONT DUPLEX')
C      WRITE( LUNIT, 9500) (BUFF(I), I=IP3+1,IP3+15)
C9500 FORMAT(1X,'TITLE TOP ','''',15A4,'''')
 
C     WRITE( LUNIT, 9600) XMIN,XMAX
C9600 FORMAT(1X,'SET LIMIT X FROM ',G12.4,'TO ',G12.4)
C
C     WRITE( LUNIT, 9700)
C9700 FORMAT(1X,'SET INTENSITY 4',
C    .      /1X,'SET TICK SIZE 0.04',
C    .      /1X,'SET ORDER X Y DY')
 
      IPF   = IP2 + 156
      IPF2  = IPF + 52
      FACT       = 1./(NTOTAL*DEV)
      DO 400 I = 1, NXBIN
         TX     = BUFF(I+IPF)
         NX     = IBUF(I+IP2)
         VLS    = TX*FACT
         IF( NX .GT. 1 ) THEN
             DEV2   =  NX*BUFF(I+IPF2)-TX*TX
             IF( DEV2 .LE. 0.0 ) THEN
                 VER = 0.0
             ELSE
                 VER = FACT*SQRT( DEV2/( NX-1 ))
             ENDIF
         ELSEIF( NX .EQ. 1 ) THEN
             VER = VLS
         ELSE
             VER = 0.0
         ENDIF
         XX     = XMIN + DEV*(FLOAT(I) - 0.5)
CC       WRITE( LUNIT, 9800) XX, VLS, VER
         IF( NUMTDR .GE. MXxBUF ) RETURN
         NUMTDR = NUMTDR + 1
 
         XHTDR(1,NUMTDR) = XX
         XHTDR(2,NUMTDR) = VLS
         XHTDR(3,NUMTDR) = VER
 9800    FORMAT(1X,E11.4,3X,E14.7,3X,E14.7)
  400 CONTINUE
 
C      WRITE( LUNIT, 9900)
C 9900 FORMAT( 1X,'HIST')
 
      RETURN
      END
