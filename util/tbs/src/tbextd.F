C **********************************************************************
C*                                                                     *
C*==========================================                           *
C* Subroutine TBEXTD( IDDBS, NWEXTD, IRET )                            *
C*==========================================                           *
C*(Purpose)                                                            *
C*   To define the unit of data_Subbuffer extension.                   *
C*(Input)                                                              *
C*   IDDSB    : DSB ID to be extended.                                 *
C*   NWEXTD   : Unit size of the extension.( in I*4 word count)        *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET >=0 : Normal Return                                        *
C*          < 0 : Warning                                              *
C*          -1  : The allocated record ID does not exist.              *
C*(Used COMMON)                                                        *
#include "bnkexp.inc"
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     Jan.13 '86     V4.0                            *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBEXTD( IDDSB, NWEXTD, IRET )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsmes.inc"
 
C  --------------------------------------------------------------
C                Check the several Parameters
C  --------------------------------------------------------------
C
C    ---------- Test whether the DSB: IDSHAR exist --------------
C                             =====
         CALL TBxFSB( IDDSB,  IPSUB )
 
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) IDDSB
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBEXTD', 1)
 
           IRET = -1
           RETURN
         ENDIF
 
         IF( NWEXTD .GT. MAXSIZ/2) THEN
             WRITE(MESAGE(1),9011) NWEXTD
 9011        FORMAT(' The unit of extension (',I6,' ) is too large')
             WRITE(MESAGE(2),9012) MAXSIZ/2
 9012        FORMAT(' Should be less than ',I10)
             CALL TBxERR( 200,'TBEXTD', 2)
             IRET  = -2
             RETURN
         ENDIF
 
         IDSUB(12,IPSUB) = NWEXTD
 
         IRET  = 1
         RETURN
         END
