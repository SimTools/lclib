C **********************************************************************
C*                                                                     *
C*===============================                                      *
C* Subroutine TBSQEZ( ID, IRET )                                       *
C*===============================                                      *
C*(Purpose)                                                            *
C*   Delete those banks, which have no bank element.                   *
C*(Input)                                                              *
C*   ID       : Record ID number                                       *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET >=0 : Number of banks deleted.                             *
C*          =-1 : Warning; Record ID is not found.                     *
C*(Used COMMON)                                                        *
C*     /TBSBNK/    Further information, see TBREAD.                    *
C*(Author)                                                             *
C*       S.Kawabata     Feb. 13 '86   Version V4.0                     *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBSQEZ( ID, IRET )
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
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
           CALL TBxERR( 200,'TBSQEZ', 1)
 
           IRET     = -1
           RETURN
         ENDIF
 
C  ------------ Find those bank, which has no element ----------------
C  ------------ and delete it                         ----------------
 
         IRET   = 0
         IOFT   = IDSUB(2,IPSUB)
         IBEGIN = IOFT + 1
   50    NOBANK = IDSUB(4,IPSUB)
         IF( NOBANK .LE. 0 ) RETURN
 
         ISTART = IBEGIN
         IEND   = IOFT + NOBANK
         DO 100 ISRT = ISTART, IEND
            IPBNK    = ISORT( ISRT )
            IF( KEYBNK(2,IPBNK) .LE. 0 ) THEN
 
                CALL TBxBDL( IPSUB, ISRT, IPBNK)
                IRET  = IRET + 1
                IBEGIN = ISRT
                GO TO 50
 
            ENDIF
  100    CONTINUE
 
         RETURN
         END
