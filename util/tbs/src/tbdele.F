C **********************************************************************
C*                                                                     *
C*========================================                             *
C* Subroutine TBDELE( ID, BNAME, NE, IRET)                             *
C*========================================                             *
C*(Purpose)                                                            *
C*   Delete the Bank_Element BNAME:NE.                                 *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NE       : Element_Number                                         *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning;                                             *
C*          =-1 : Warning; DSB ID does not exist.                      *
C*          =-2 : Warning; Bank does not exist.                        *
C*          =-3 : Warning; Bank_Element was not found.                 *
C*     IRET = 1 : Normal return, element was deleted.                  *
C*(Used COMMON)                                                        *
C*     /TBSBNK/   Further information, see TBSREAD.                    *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov. 27'85     Version V4.0                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBDELE( ID, BNAME, NE, IRET)
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
C
C  ------------------------------------------------------------------
C                  Check the several Parameters
C  ------------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBDELE', 1)
 
           IRET     = -1
           RETURN
         ENDIF
C
C  ------------------------------------------------------------------
C                  Find Bank_name: BNAME
C  ------------------------------------------------------------------
 
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
             CALL TBxERR( 200,'TBDELE', 2)
 
           IRET = -2
           RETURN
         ENDIF
C
C  ------------------------------------------------------------------
C                 Delete the Bank_Element BNAME; NE
C  ------------------------------------------------------------------
C                                       ====
         CALL TBxDEL( IPSUB, IPBNK, NE, IRET)
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9021)
 9021        FORMAT(' The following bank element does not exist.')
             WRITE(MESAGE(2),9022) TNAME
 9022        FORMAT(' Bank name = ',A64)
             WRITE(MESAGE(3),9023) NE
 9023        FORMAT(' Element # = ',I6)
             CALL TBxERR( 200,'TBDELE', 3)
 
           IRET = -3
           RETURN
         ENDIF
C
         IRET           = 1
         IDSUB(8,IPSUB) = 1
C
         RETURN
         END
