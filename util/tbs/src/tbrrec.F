C *********************************************************************C
C*                                                                     *
C*==================================================                   *
C* Subroutine TBRREC( ID, 'New record name', IRET )                    *
C*==================================================                   *
C*(Purpose)                                                            *
C*   To rename record name                                             *
C*(Input)                                                              *
C*   ID       : ID_Number of the Record                                *
C*   'New record name ' up to 64 characters                            *
C*(Used COMMON)                                                        *
C*    /TBSBNK/  For detail information, see TBALOC.                    *
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     Nov. 27 '85    Version 4.0                     *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBRREC( ID, RECNEW, IRET )
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
 
      CHARACTER*(*) RECNEW
      CHARACTER*64  TNAME
C
      IRET  =  1
C
C  ----------------------------------------------------------------
C            Check whether the DSB: ID exist.
C  ---------------------------------------------------------------
C                         =====
         CALL TBxFSB( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBRREC', 1)
 
           IRET = -1
           RETURN
         ENDIF
C
         IF( IDSUB(9,IPSUB) .GT. 0 ) THEN
 
           WRITE(MESAGE(1),9011)
 9011      FORMAT(' Neglect this call, because of shared DSB')
           CALL TBxERR( 200,'TBRREC', 1)
 
           IRET = -2
           RETURN
         ENDIF
C
         TNAME  = RECNEW
         CALL UCUPER( LEN(RECNEW),  TNAME )
 
         SUBNAM(IPSUB)    = TNAME
C
         RETURN
         END
