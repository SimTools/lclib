C **********************************************************************
C*                                                                     *
C*======================================================               *
C* Subroutine TBCRTE( ID, BNAME, ISALOC, ISEXT, IRET )                 *
C*======================================================               *
C*(Purpose)                                                            *
C*   Create new Bank in the DSB: ID.                                   *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   ISALOC   : Allocated number of words for each element in the Bank *
C*   ISEXT    : Number of words of extension for each element          *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning; The Bank could not be created.              *
C*          -1  : Record ID does not exist.                            *
C*          -2  : Bank name already exist.                             *
C*          -3  : Bank was not created.                                *
C*     IRET = 1 : Normal Return                                        *
C*(Used COMMON)                                                        *
C*   /TBSBNK/    For detail information, see TBREAD.                   *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85   Version 3.0                      *
C*       S.Kawabata     Nov. 27 '85   Version 4.0                      *
C*                                                                     *
C **********************************************************************
C
      SUBROUTINE TBCRTE( ID, BNAME, ISALOC, ISEXT, IRET )
C
      IMPLICIT INTEGER*4  (H)
#include "tbsbnk.inc"
#include "tbsmes.inc"
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
      HSALOC  = 2*ISALOC
      HSEXT   = 2*ISEXT
 
C  ------------------------------------------------------------------
C                 Test whether DSB: ID exists.
C  ------------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBCRTE', 1)
           IRET     = -1
           RETURN
         ENDIF
C
C --------------------------------------------------------------------
C                       Find Bank_Name: BNAME
C --------------------------------------------------------------------
 
         TNAME = BNAME
 
         CALL UCUPER( LEN(BNAME), TNAME)
 
C                                   ====  =====  ====
         CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
C
         IF( IRET .GT. 0 ) THEN
           WRITE(MESAGE(1),9011)
 9011      FORMAT(' The following bank already exist.')
           WRITE(MESAGE(2),9012) TNAME
 9012      FORMAT(' ',A64)
           CALL TBxERR( 200,'TBCRTE', 2)
           IRET     = -2
           RETURN
         ENDIF
C
C -------------------------------------------------------------------
C          Store the Bank_Name by Calling Subroutine TBxSBK.
C -------------------------------------------------------------------
C                                                               =====
         CALL TBxSBK( IPSUB, TNAME, ISRT, IPBNK, HSALOC, HSEXT, IRRET)
C
         IRET   = 1
         IF( IRRET .LT. 0 ) IRET = -3
C
         RETURN
         END
