C **********************************************************************
C*                                                                     *
C*============================================================         *
C* Subroutine TBLINK( ID, BNAME, NE, TYPE, LOC, NWORDS, IRET )         *
C*============================================================         *
C*(Purpose)                                                            *
C*   To link the location and word variables to a bank element.        *
C*   After this calling, location and # of words of the bank element   *
C*   are saved in LOC and NWORDS, respectively.                        *
C*   Even after the gabage collection, LOC and NWORDs are updated      *
C*   until the record is on a Data subbuffer.                          *
C*(Usage)                                                              *
C*   COMMON /POINTR/ LOC1, NWRD1                                       *
C*   CALL TBLINK( ID,'Bank_name', NE, 'I*2', LOC1, NWORDS, IRET )      *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NE       : Element_Number                                         *
C*   TYPE     : Type attribution 'I*2' or 'I*4'                        *
C*   /LOC/    : Address of the variable LOC                            *
C*   /NWORDS/ : Address of the variavle NWORDS                         *
C*(Output)                                                             *
C*   LOC      : Location of the bank element in the Data Buffer        *
C*              by I*2 unit.                                           *
C*              I*2 (I*4).                                             *
C*   NWORDS   : Word_Count of the Bank element by I*2 unit.            *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning;                                             *
C*          =-1 : Warning; DSB ID does not exist.                      *
C*          =-2 : Warning; Bank does not exist.                        *
C*          =-3 : Warning; Bank element does not exist.                *
C*     IRET = 1 : Normal return.                                       *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  Further information, see TBREAD.         *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov.27 '85     Version V4.0                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLINK( ID, BNAME, NE,
#ifdef  MSP
     >      TYPE, /LOC/, /NWORDS/, IRET)
#endif
#ifdef  UNIX
     >      TYPE, LOC, NWORDS, IRET)
#endif
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
      CHARACTER*(*) BNAME,TYPE
      CHARACTER*64  TNAME
      CHARACTER*3   I2,I4
      DATA I2,I4 / 'I*2', 'I*4'/
 
C  --------------------------------------------------------------------
C                    Check the several Parameters
C  --------------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBLINK', 1)
 
           IRET     = -1
           RETURN
         ENDIF
C
C  --------------------------------------------------------------------
C                    Find Bank_name: BNAME
C  --------------------------------------------------------------------
 
         TNAME = BNAME
 
         CALL UCUPER( LEN(BNAME), TNAME)
 
C                                   ====  =====  ====
         CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9011)
 9011        FORMAT(' The following bank does not exist.')
             WRITE(MESAGE(2),9012) TNAME
 9012        FORMAT(' ',A64)
             CALL TBxERR( 200,'TBLINK', 2)
 
           IRET = -2
           RETURN
         ENDIF
C
C  --------------------------------------------------------------------
C               Look for the Bank_Element BNAME; NE
C  --------------------------------------------------------------------
C                                =====  ====
         CALL TBxFEL( IPBNK, NE, IPELM, IRET)
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9021)
 9021        FORMAT(' The following bank element does not exist.')
             WRITE(MESAGE(2),9022) TNAME
 9022        FORMAT(' Bank name = ',A64)
             WRITE(MESAGE(3),9023) NE
 9023        FORMAT(' Element # = ',I6)
             CALL TBxERR( 200,'TBLINK', 3)
 
             IRET = -3
 
             RETURN
         ENDIF
 
         IF( LINK(3,IPELM) .LT. 0 ) THEN
             LOC1   = IADTBL(1,IPELM) + NWSUB(1,IPSUB)
             NWORD1 = NWELM(2,IPELM)
             IADTBL(2,IPELM)  = NUADDR( LOC )
             IADTBL(3,IPELM)  = NUADDR( NWORDS )
             IF( TYPE .EQ. I4 ) THEN
                 NPOINT          =  (LOC1+1)/2
                 NWORD           =  (NWORD1+1)/2
                 LINK(3,IPELM)   = 1
             ELSE
                 NPOINT          =  LOC1
                 NWORD           =  NWORD1
                 LINK(3,IPELM)   = 0
                 IF( TYPE .NE. I2 ) THEN
                     WRITE(MESAGE(1),9031) TYPE
 9031                FORMAT(' ',A3,' is illegal ATT parameter.')
                     WRITE(MESAGE(2),9032)
 9032                FORMAT(' Assuming ATT = I*2')
                     CALL TBxERR( 200,'TBLINK', 2)
                     IRET  = -4
                 ENDIF
             ENDIF
             CALL UVSET( IADTBL(2,IPELM), NPOINT )
             CALL UVSET( IADTBL(3,IPELM), NWORD  )
             IRET = 1
         ELSE
             WRITE(MESAGE(1),9041)
 9041        FORMAT(' The following bank element is already linked.')
             WRITE(MESAGE(2),9042) ID, NE
 9042        FORMAT('  RECORD ID =',I4,'  Element# =',I4)
             WRITE(MESAGE(3),9022)  BNKNAM(IPBNK)
             CALL TBxERR( 200,'TBLINK', 3)
             IRET  = -5
         ENDIF
 
         RETURN
         END
