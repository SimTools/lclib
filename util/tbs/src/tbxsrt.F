C **********************************************************************
C*                                                                     *
C*===================================================================  *
C* Subroutine TBxSRT( IPSUB, IRET )
C*===================================================================  *
C*(Purpose)
C*   Re-create the array ISORT, for data format conversion
C*   from MSP to UNIX system
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB: ID in DSB_Parameter_Table: IDSUB   *
C*   BNAME    : Bank_Name                                              *
C*   ISRT     : Address of the Bank in Sort_Table: ISORT               *
C*   IPBNK    : Address of the Bank in Bank_Name_Table: BNKNAM,NWBNK   *
C*   HSALOC   : Allocated number of words for each element in the bank *
C*   HSEXT    : Number of words of extension for each element          *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning; Bank was not created.                       *
C*     IRET >=0 : Normal Return                                        *
C*(Used COMMON)                                                        *
C*    /TBSBNK/     For detail information, see TBREAD.                 *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85    Version up   V 3.0              *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxSRT( IPSUB, IRET )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsmes.inc"
      CHARACTER*64  BNAME
C
      IRET = 0
C
      IOFS = IDSUB(2, IPSUB)
      NBNK = IDSUB(4,IPSUB)
 
 200  CONTINUE
      DO 300 IB = 2, NBNK
        IP1 = ISORT(IOFS+IB-1)
        IP2 = ISORT(IOFS+IB)
        IF( BNKNAM(IP1).LT.BNKNAM(IP2)) GO TO 300
C        PRINT *,BNKNAM(IP1)(:50)
C        PRINT *,BNKNAM(IP2)(:50)
C        PRINT *,'   is Wrong order .'
        ISORT(IOFS+IB-1) = IP2
        ISORT(IOFS+IB)   = IP1
        GO TO 200
 300  CONTINUE
 
      RETURN
      END
