C **********************************************************************
C*                                                                     *
C*====================================                                 *
C* Subroutine TBDELB( ID, BNAME, IRET)                                 *
C*====================================                                 *
C*(Purpose)                                                            *
C*   Delete the Bank  BNAME                                            *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET =-1 : Warning; Record ID does not exist.                   *
C*          =-2 : Warning; Bank does not exist.                        *
C*          =-3 : Warning; can not delete elements in the bank.        *
C*     IRET = 1 : Warning; normal return                               *
C*(Used COMMON)                                                        *
C*     /TBSBNK/    Further information, see TBREAD.                    *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov.27 '85    Version V4.0                     *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBDELB( ID, BNAME, IRET)
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
C
C  --------------------------------------------------------------------
C               Check the several Parameters
C  --------------------------------------------------------------------
C  ------------ Test whether the DSB: ID exist ------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBDELB', 1)
 
           IRET     = -1
           RETURN
         ENDIF
C
C  ------------ Find Bank_name: BNAME  -------------------------------
 
         TNAME = BNAME
 
         CALL UCUPER( LEN(BNAME), TNAME)
 
C                                   ====  =====  ====
         CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
C
         IF( IRET .LT. 0 ) THEN
C
           WRITE(MESAGE(1),9011)
 9011      FORMAT(' The following bank does not exist.')
           WRITE(MESAGE(2),9012) TNAME
 9012      FORMAT(' ',A64)
           CALL TBxERR( 200,'TBDELB', 2)
C
           IRET = -2
           RETURN
         ENDIF
C
C  -------------------------------------------------------------------
C     If the Number of Elements is greater than zero, then delete
C    NEL Elements.
C  -------------------------------------------------------------------
C
         IF( KEYBNK(2,IPSUB) .GT. 0 ) THEN
           IFDEL = -1
C                                            ====
           CALL TBxDEL( IPSUB, IPBNK, IFDEL, IRET)
C
           IF( IRET .LT. 0 ) THEN
 
              WRITE(MESAGE(1),9021)
 9021         FORMAT(' The following bank could not deleted.')
              WRITE(MESAGE(2),9012) TNAME
              CALL TBxERR( 200,'TBDELB', 2)
 
             IRET = -3
             RETURN
           ENDIF
         ENDIF
 
C------  Delete the bank, identified by IPBNK ----------
 
         CALL TBxBDL( IPSUB, ISRT, IPBNK )
 
         IRET  = 1
C
         RETURN
         END
