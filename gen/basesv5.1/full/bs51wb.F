C*********************************************************************
C*
C*    ------------------------
C*    SUBROUTINE BS51WB(NRET)
C*    ------------------------
C*(Function)
C*   Write BASES DATA into the TBS bank.
C*(Output)
C*   NRET = 0   when normal return
C*        = -1  when some error has occured.
C*(Author)
C*   A.Miyamoto  19-Oct-1994  Constructed from BSWRIT
C*
C*********************************************************************
	
      SUBROUTINE BS51WB( NRET )

************************************************************************
*                                                                      *
*    ==========================                                        *
C      SUBROUTINE BSWRIT( LUN )
*    =====================                                             *
* ((Purpose))                                                          *
*     Read temporary result from disk file.                            *
* ((Auther))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
*                                                                      *
************************************************************************
 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE1/ ND1(5*MXDIM+3)
*     COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
*    .               IG(MXDIM),NCALL
C     COMMON /BASE2/ ND2(6)
*     COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      COMMON /BASE3/ ND3(11)
*     COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
      COMMON /BASE4/ ND4(2*MXDIM*(NDMX+1)+4*LENG+MXDIM+3)
*     COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
*    .               ND,NG,NPG,MA(MXDIM)
      PARAMETER (ITM  = 50 )
      COMMON /BASE5/ ND5(22*ITM)
*     REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT
*     COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),
*    .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),
*    .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)
      COMMON /RANDM/ ND6(45)
 
      PARAMETER ( NHS = 50, NSC = 50 )
      COMMON /PLOTH/ NPH(18*(NHS+NSC)+29),NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
*     INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
*     COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
*    .              NHIST, MAPL(4,NHS),
*    .              NSCAT, MAPD(4,NSC),
*    .              NW
 
      COMMON/NINFO/ NODEID, NUMNOD
 
      NRET = -1
#ifdef TAYM
      IF( NODEID .NE. 0 ) RETURN
#endif 
      IF( NODEID.NE.0 ) THEN
	PRINT *,'in BS51WB .. NODEID .NE. 0 '
	RETURN
      ENDIF
 
#ifdef TAYM
      REWIND LUN
#endif 
#ifdef TAYM
      WRITE(LUN) ND1,ND3,ND4,ND5,ND6,NPH
#endif 
C     WRITE(LUN) ND1,ND2,ND3,ND4,ND5,ND6,NPH
#ifdef TAYM
      IF(NW .EQ. 0 ) NW = 281
#endif 
#ifdef TAYM
      WRITE(LUN) NW,(IBUF(I),I=1,NW)
#endif 
C

      CALL TBCRTE(2,'BASES:Results', 0, 0, IRET)
      IF( IRET .LT. 0 ) THEN
	PRINT *,'in BS51WB .. Error to create the bank',
     >          ' BASES:Results'
        RETURN
      ENDIF
C
      IW = 5*MXDIM+3
      CALL TBPUT(2,'BASES:Results',1,IW, ND1, IRET)
      IF( IRET.LT.0 ) THEN
	PRINT *,'in BS51WB..Error to TBPUT ND1 array, IRET=',IRET
	RETURN
      ENDIF

      CALL TBPUT(2,'BASES:Results',3,11, ND3, IRET)
      IF( IRET.LT.0 ) THEN
	PRINT *,'in BS51WB..Error to TBPUT ND3 array, IRET=',IRET
	RETURN
      ENDIF

      IW = 2*MXDIM*(NDMX+1)+4*LENG+MXDIM+3
      CALL TBPUT(2,'BASES:Results',4, IW, ND4, IRET)
      IF( IRET.LT.0 ) THEN
	PRINT *,'in BS51WB..Error to TBPUT ND4 array, IRET=',IRET
	RETURN
      ENDIF

      IW = 22*ITM
      CALL TBPUT(2,'BASES:Results',5, IW, ND5, IRET)
      IF( IRET.LT.0 ) THEN
	PRINT *,'in BS51WB..Error to TBPUT ND5 array, IRET=',IRET
	RETURN
      ENDIF

      CALL TBPUT(2,'BASES:Results',6, 45, ND6, IRET)
      IF( IRET.LT.0 ) THEN
	PRINT *,'in BS51WB..Error to TBPUT ND6 array, IRET=',IRET
	RETURN
      ENDIF

      IW = 18*(NHS+NSC)+29
      CALL TBPUT(2,'BASES:Results',9, IW, NPH, IRET)
      IF( IRET.LT.0 ) THEN
	PRINT *,'in BS51WB..Error to TBPUT NPH array, IRET=',IRET
	RETURN
      ENDIF

      CALL TBPUT(2,'BASES:Results',10, NW, IBUF, IRET)
      IF( IRET.LT.0 ) THEN
	PRINT *,'in BS51WB..Error to TBPUT IBUF array, IRET=',IRET
	RETURN
      ENDIF

      NRET = 0

      RETURN
      END

