C **********************************************************************
C*                                                                     *
C*===========================================================          *
C* Subroutine TBLOC( ID, BNAME, NE, TYPE, LOC, NWORDS, IRET )          *
C*===========================================================          *
C*(Purpose)                                                            *
C*   Get the Data IDATA from the Bank_Element BNAME:NE.                *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NE       : Element_Number                                         *
C*   TYPE     : Character string (CHARACTER*3)                         *
C*              TYPE should be 'I*2' or 'I*4'                          *
C*(Output)                                                             *
C*   LOC      : Location of the bank element in the Data Buffer.       *
C*              If TYPE='I*2'('I*4'), then LOC is given by the unit    *
C*              I*2 (I*4).                                             *
C*   NWORDS   : Word_Count of the Bank element.                        *
C*              If TYPE='I*2'('I*4'), then LOC is given by the unit    *
C*              I*2 (I*4).                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning;                                             *
C*          =-1 : Warning; DSB ID does not exist.                      *
C*          =-2 : Warning; Bank does not exist.                        *
C*          =-3 : Warning; Bank element does not exist.                *
C*          =-4 : Warning; Illegal TYPE parameter.                     *
C*     IRET = 1 : Normal return.                                       *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  Further information, see TBREAD.         *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov.27 '85     Version V4.0                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLOC( ID, BNAME, NE, TYPE, LOC, NWORDS, IRET )
 
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
           CALL TBxERR( 200,'TBLOC', 1)
 
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
             CALL TBxERR( 200,'TBLOC', 2)
 
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
             CALL TBxERR( 200,'TBLOC', 3)
 
           IRET = -3
 
           RETURN
         ENDIF
 
         IRET = 1
         IF( TYPE .EQ. I4 ) THEN
             LOC    = (IADTBL(1,IPELM)+NWSUB(1,IPSUB)+1)/2
             NWORDS = (NWELM(2,IPELM)+1)/2
         ELSE
             LOC    = IADTBL(1,IPELM) + NWSUB(1,IPSUB)
             NWORDS = NWELM(2,IPELM)
             IF( TYPE .NE. I2 ) THEN
                 WRITE(MESAGE(1),9031) TYPE
 9031            FORMAT(' ',A3,' is illegal ATT parameter.')
                 WRITE(MESAGE(2),9032)
 9032            FORMAT(' Assuming ATT = I*2')
                 CALL TBxERR( 200,'TBLOC', 2)
                 IRET  = -4
             ENDIF
         ENDIF
 
         RETURN
         END
