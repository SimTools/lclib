#ifdef C
     INTEGER*4  NHDATA(2,100), NCLSPT(2,100)
#endif 
#ifdef C
     DATA  MXxCLS/100/, MXxTHA/45/, MXxPHI/96/
#endif 
#ifdef C
     REAL*4  CLSPAR(4)/ 1., 0.65, 0.08, 1.2 /
#endif 
#ifdef C
     KK = 1
#endif 
#ifdef CCC
   DO 1000 KK = 1, 10
#endif 
#ifdef C
     NHDATA(1,1) = 45003
#endif 
#ifdef C
     NHDATA(1,2) = 45002
#endif 
#ifdef C
     NHDATA(1,3) = 45001
#endif 
#ifdef C
     NHDATA(1,4) = 45004
#endif 
#ifdef C
     NHDATA(2,1) =  10000
#endif 
#ifdef C
     NHDATA(2,2) =   4000
#endif 
#ifdef C
     NHDATA(2,3) =   2000
#endif 
#ifdef C
    NHDATA(2,4) =    350 + 10*KK
#endif 
CC
#ifdef C
     NHDATA(2,4) = 300
#endif 
#ifdef C
     CALL SMRCLS(4, NHDATA, MXxTHA, MXxPHI, CLSPAR,
#endif 
#ifdef C
    >            MXxCLS, NUMCLS, NCLSPT )
#endif 
#ifdef C
     PRINT *,' NUMCLS=',NUMCLS
#endif 
#ifdef C
     DO 100 I = 1, NUMCLS
#endif 
#ifdef C
       PRINT *,' NCLSPT=',NCLSPT(1,I),NCLSPT(2,I)
#endif 
#ifdef C
       DO 110 J = NCLSPT(1,I), NCLSPT(2,I)
#endif 
#ifdef C
         PRINT *,' NHDATA =',NHDATA(1,J),NHDATA(2,J)
#endif 
#ifdef C110
    CONTINUE
#endif 
#ifdef C100
  CONTINUE
#endif 
#ifdef C1000
  CONTINUE
#endif 
#ifdef C
     STOP
#endif 
#ifdef C
     END
#endif 
CC*********************************************************************C
C*
C*  -----------------------------------------------------------
C*  Subroutine SMRCLS( NHIT,   NHDATA, MXxTHA, MXxPHI, CLSPAR,
C*                     MXxCLS, NUMCLS, NCLSPT )
C*  ---------------------------==============
C*
C*(Function)
C*  Utility routine for the clustering of calorimeter signal.
C*(Input)
C*  NHIT  : # of Hit counters
C*  NHDATA(2,i)  :  Counter data
C*        (1,i)  = Counter ID = 1000*ITH + IPHI
C*        (2,i)  = Energy deposite (MeV)
C*        (3,i)  = Original hit ID
C*        Data should be sorted according to the pulse height
C*        Larger pulse counter to the lower address.
C*  MXxTHA       ; # of Theta binning.
C*  MXxPHI       ; # of Phi binning.
C*                ( Assume 1 is next to MXxPHI)
C*  CLSPAR(4)    ; Clustering parameter, Eth, C1, C2, C3
C*  MXxCLS       ; Size of output cluster buffer.
C*
C*(Output)
C*  NUMCLS       ; # of Cluster
C*             = 0 No cluster.
C*             < 0 error.
C*  NHDATA(2,i)  ; Re-arranged cluster data.
C*  NCLSPT(2,NUMCLS) ; Start and last address of cluster.
C*
C*
C*  Sign of NHDATA(2,i) is changed to negative if counter hit is
C*  included to a cluster.
C*
C*(Author)
C*  A. Miyamoto    9-Oct-1990  Created from BCCLUS
C*
CC*********************************************************************C
C
      SUBROUTINE SMRCLS( NHIT, NHDATA, MXxTHA, MXxPHI, CLSPAR,
     >                   MXxCLS, NUMCLS, NCLSPT )
C
      INTEGER*4  NHDATA(3,NHIT)
      INTEGER*4  NCLSPT(2,MXxCLS)
      REAL*4     CLSPAR(4)
C
C  ... Local variable.
C
      INTEGER*4   ILST(4), NB(4), ISRT(4), JLST(4)
C
C =====================< Entry Point >=================================
C
C --------------------------------------------------------------------
C (1) Initialization.
C --------------------------------------------------------------------
C
#ifdef C
   PRINT *,' CLSPAR =',CLSPAR
#endif 
      NUMCLS = 0
      IF( NHIT .LE. 0 ) RETURN
 
      KEND   = NHIT
      IBL1   = 1
      IBL2   = 1
 
C
100   CONTINUE
      NUMCLS = NUMCLS + 1
      IF( NUMCLS .GT. MXxCLS ) GO TO 900
      NCLSPT(1, NUMCLS) = IBL1
 
#ifdef C
   PRINT *,' NUMCLS =',NUMCLS
#endif 
#ifdef C
   PRINT *,' IBL1=',IBL1
#endif 
C
C ===== Start clustering
C
C         Here we apply the algorythm used by JADE group.
C          i.e.
C           Set parent ctr. = BL1
C            BL1 must satisfy following condition,
C                      E(BL1) > 45 MeV.
C           Daughter ctr. = BL2,
C           BL2 is located next to BL1 with side face.
C           Next find out BL3.
C             BL3 will be selected with following conditions,
C               a)  E(BL2) > E(BL1)*C1
C              or
C               b)  E(BL3) < E(BL1)*C2 .and. E(BL3) < C3*E(BL2) .
C
C      Parameter values
C        C1 = 0.65
C        C2 = 0.08
C        C3 = 1.20
C           are selected.
C        ------------------------------------------------
C
C ----- Set threshold
C
C ...
 
      ECL1 = NHDATA(2,IBL1)
      IF( ECL1 .LT. CLSPAR(1) ) GO TO 850
C ...
      RCUT1 = ECL1*CLSPAR(2)
      RCUT2 = ECL1*CLSPAR(3)
C
C ----- NEXT is the address of the 1-st non-clusterized member
C
      INXT  = IBL1 + 1
      IF(INXT .GT. KEND ) GO TO 800
C
C ====================================================================
C  Obtain list of neighbor counters.
C ====================================================================
C
200   CONTINUE
#ifdef C
   PRINT *,' pass 200 '
#endif 
#ifdef C
   PRINT *,' INXT =',INXT,' IBL2=',IBL2
#endif 
      IDPRNT = NHDATA( 1, IBL2 )
#ifdef C
   PRINT *,' IDPRNT=',IDPRNT
#endif 
C
C
      IPHI   = MOD( IDPRNT, 1000 )
      ITH    = IDPRNT/1000
      NB(1)  = IDPRNT + 1000
      IF( IPHI .EQ. MXxPHI ) THEN
         NB(2) = IDPRNT - MXxPHI + 1
      ELSE
         NB(2) = IDPRNT + 1
      ENDIF
      IF( IPHI .EQ. 1 ) THEN
         NB(3) = IDPRNT - 1 + MXxPHI
      ELSE
         NB(3) = IDPRNT - 1
      ENDIF
      NB(4) = IDPRNT - 1000
#ifdef C
   PRINT *,' NB=',NB
#endif 
C
C ... NBTOT  ; # of neighbour counter.
C
      NBTOT = 0
      DO 210 IB = 1, 4
        INB = NB(IB)
        IF( INB .LE. 0 .OR. INB .GE. (MXxTHA+1)*1000 ) GO TO 210
        DO 220 I = INXT, NHIT
          IF( NHDATA(1,I) .EQ. INB ) THEN
            NBTOT = NBTOT + 1
            JLST(NBTOT) = I
#ifdef C
         PRINT *,' ILST(I)=',JLST(NBTOT),' NHDATA=',NHDATA(1,I)
#endif 
          ENDIF
220     CONTINUE
210   CONTINUE
      IF( NBTOT .LE. 0 ) GO TO 700
C
C ... Sort LIST array
C
      CALL USORTI( NBTOT, 1, 1, JLST, ISRT )
      DO 230 I = 1, NBTOT
        ILST(I) = JLST(ISRT(I))
230   CONTINUE
C ...
C     ==========================================
#ifdef C
    CALL BCNBLS( IDPRNT, NEXT, ILST, IFLAG )
#endif 
C     ==========================================
C
#ifdef C
   PRINT *,' NBTOT =',NBTOT
#endif 
#ifdef C
     DO 240 I = 1, NBTOT
#endif 
#ifdef C
           PRINT *,' ILST(I)=',JLST(I),' NHDATA=',NHDATA(1,I),
#endif 
#ifdef C
  >                  NHDATA(2,I),NHDATA(3,I)
#endif 
#ifdef C240
  CONTINUE
#endif 
       RCUT3 = CLSPAR(4)*REAL( NHDATA( 2, IBL2 ) )
 
#ifdef C
    PRINT *,' RCUT1,RCUT2,RCUT3=',RCUT1,RCUT2,RCUT3
#endif 
C
C ----- Look for daughters
C
       DO 600 I = 1, NBTOT
        IBL3    = ILST(I)
#ifdef C
     PRINT *,' IBL2, IBL3=',IBL2,IBL3
#endif 
        IF( IBL3. LT. INXT ) GO TO 600
        IF( REAL( NHDATA( 2, IBL2 ) ).GT.RCUT1 ) GO TO 300
        IF( REAL( NHDATA( 2, IBL3 ) ).LT.RCUT2.AND.
     +      REAL( NHDATA( 2, IBL3 ) ).LT.RCUT3 ) GO TO 300
        GO TO 600
 300    CONTINUE
C
C ----- Take IBL3 next to BL2 & shift remaining data
C
C ..........
        LWORK1  = NHDATA( 1, IBL3 )
        LWORK2  = NHDATA( 2, IBL3 )
        LWORK3  = NHDATA( 3, IBL3 )
#ifdef C
     PRINT *,' LWORK1 =',LWORK1,' is OK '
#endif 
        IF( IBL3.EQ.INXT ) GO TO 500
        DO 400 J = 1, IBL3 - INXT + 1
          JJ = IBL3 - J
          NHDATA( 1, JJ + 1 ) = NHDATA( 1, JJ )
          NHDATA( 2, JJ + 1 ) = NHDATA( 2, JJ )
          NHDATA( 3, JJ + 1 ) = NHDATA( 3, JJ )
 400    CONTINUE
        NHDATA( 1, INXT ) = LWORK1
        NHDATA( 2, INXT ) = LWORK2
        NHDATA( 3, INXT ) = LWORK3
C
 500    CONTINUE
        INXT = INXT + 1
C ... Debug
C       IF( IFLAG.GE.5 ) THEN
C        WRITE(6,*)' ---- After data shift '
C        WRITE( 6, * ) ' -----  SSWORK/ IBCHIT  /----- '
C        WRITE(6,*)'       NHIT  =',NHIT
C        WRITE(6,*)' -- # +----+ I*4 +----+----+ I*4 +----+----'
C        DO 550 II = 1, NHIT
C         WRITE(6,560) II, IBCHIT(1,II), IBCHIT(2,II)
C 550    CONTINUE
C 560   FORMAT( 2X,I4,6X,I6,10X,I6 )
C       ENDIF
C ..........
C
 600   CONTINUE
       IF( INXT.GT.KEND ) GO TO 800
C
C ----- Search for grand-daughter
C
 700   IBL2 = IBL2 + 1
       IF( IBL2.LT.INXT ) GO TO 200
C
C ----- Search next cluster
C
       NCLSPT( 2, NUMCLS ) = INXT - 1
       IBL1 = INXT
       GO TO 100
C
C ----- All clusters found
C
 800   NCLSPT( 2, NUMCLS ) = INXT - 1
       GO TO 1000
C
 850   NUMCLS = NUMCLS - 1
       GO TO 1000
C
C ===== Too many clusters
C
 900   NUMCLS = NUMCLS - 1
       WRITE( 6, * )' #### SMRCLS #### Too many clusters '
C
CC
 1000  CONTINUE
       RETURN
       END
