C **********************************************************************
C*                                                                     *
C*==============================================                       *
C* Subroutine TBRNAM( ID, BNOLD, BNNEW, IRET )                         *
C*==============================================                       *
C*(Purpose)                                                            *
C*   To rename a bank from the old name BNOLD to new name BNNEW.       *
C*(Input)                                                              *
C*   ID       : ID_Number of the Record                                *
C*   BNOLD    : Old bank name                                          *
C*   BNNEW    : New bank name                                          *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning; The Bank could not be created.              *
C*          -1  : DSB ID does not exist.                               *
C*          -2  : Bank name already exist.                             *
C*          -3  : Too many banks are defined                           *
C*     IRET = 1 : Normal Return                                        *
C*(Used COMMON)                                                        *
C*   /TBSBNK/    For detail information, see TBREAD.                   *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85   Version 3.0                      *
C*       S.Kawabata     Nov. 27 '85   Version 4.0                      *
C*       A.Miyamoto     Nov.  4 '88   Fix bug.                         *
C*                                                                     *
C **********************************************************************
C
      SUBROUTINE TBRNAM( ID, BNOLD, BNNEW, IRET )
C
      IMPLICIT INTEGER*4  (H)
#include "tbsbnk.inc"
#include "tbsmes.inc"
      CHARACTER*(*) BNOLD, BNNEW
      CHARACTER*64  TNAME,BNAME
 
C  ------------------------------------------------------------------
C                 Test whether DSB: ID exists.
C  ------------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBRNAM', 1)
 
           IRET     = -1
           RETURN
         ENDIF
C
C --------------------------------------------------------------------
C                       Find Bank_Name: BNAME
C --------------------------------------------------------------------
 
         TNAME   = BNOLD
         CALL UCUPER( LEN(BNOLD), TNAME )
C                                   ====  =====  ====
         CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9011)
 9011        FORMAT(' The following bank does not exist.')
             WRITE(MESAGE(2),9012) TNAME
 9012        FORMAT(' ',A64)
             CALL TBxERR( 200,'TBRNAM', 2)
 
           IRET     = -2
           RETURN
         ENDIF
 
CTAYM881104          TNAME   = BNNEW
CTAYM881104          CALL UCUPER( LEN(BNNEW), TNAME )
         BNAME   = BNNEW
         CALL UCUPER( LEN(BNNEW), BNAME )
C                                   ====  =====  ====
         CALL TBxFBK( IPSUB, BNAME, ISRN, IPBNN, IRET)
C
         IF( IRET .GT. 0 ) THEN
 
             WRITE(MESAGE(1),9021)
 9021        FORMAT(' The following bank exists already.')
             WRITE(MESAGE(2),9012) BNAME
             CALL TBxERR( 200,'TBRNAM', 2)
 
           IRET     = -3
           RETURN
         ENDIF
 
C        Copy the bank key table NWBNK(i,j),KEYBNK(i,IPBNK)
 
         HSALOC  = NWBNK(1,IPBNK)
         HSEXT   = NWBNK(2,IPBNK)
         HBADD   = KEYBNK(1,IPBNK)
         HBELM   = KEYBNK(2,IPBNK)
 
C -------------------------------------------------------------------
C          Store the Bank_Name by Calling Subroutine TBxSBK.
C -------------------------------------------------------------------
C                                                               =====
         CALL TBxSBK( IPSUB, BNAME, ISRN, IPBNN, HSALOC, HSEXT, IRRET)
C
         KEYBNK(1,IPBNN)  = HBADD
         KEYBNK(2,IPBNN)  = HBELM
         IRET   = 1
         IF( IRRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9031)
 9031        FORMAT(' Renaming error because of lack of space')
             WRITE(MESAGE(2),9032)
 9032        FORMAT(' in bank name table.')
             CALL TBxERR( 200,'TBRNAM', 2)
 
             IRET  = -4
             RETURN
         ENDIF
 
C -------------------------------------------------------------------
C          Delete old name bank
C -------------------------------------------------------------------
 
         NWBNK(1,IPBNK)  = 0
         NWBNK(2,IPBNK)  = 0
         KEYBNK(1,IPBNK) = -1
         KEYBNK(2,IPBNK) = 0
 
         CALL TBDELB( ID, TNAME, IRET )
 
C
         RETURN
         END
