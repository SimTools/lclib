C***********************************************************************
C*                                                                     *
C*====================================================                 *
C* Subroutine TBxFBK( IPSUB, BNAME, ISRT, IPBNK, IRET)                 *
C*====================================================                 *
C*(Purpose)                                                            *
C*   Find Bank by the Bank_Name BNAME                                  *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB: ID in DSB_Parameter_Table: IDSUB   *
C*   BNAME    : Bank_Name                                              *
C*(Output)                                                             *
C*   ISRT     : Address of the Bank in Sort_Table: ISORT               *
C*   IPBNK    : Address of the Bank in Bank_Name_Table: BNKNAM,NWBNK   *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning. No such a Bank was found.                   *
C*     IRET >=0 : Normal Return                                        *
C*(Used COMMON)                                                        *
C       /TBSBNK/   For detail information, see  TBREAD.                *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxFBK( IPSUB, BNAME, ISRT, IPBNK, IRET)
 
#include "tbsbnk.inc"
 
      CHARACTER*64  BNAME
 
C  --------------------------------------------------------------------
C            Check the number of Banks in the IPSUB-th DSB.
C  --------------------------------------------------------------------
C  ------ If no Bank exists, then set IRET < 0 and Return.------------
C
         IRET   = -1
         NOBANK   = IDSUB(4,IPSUB)
         IF( NOBANK .EQ. 0 ) THEN
           ISRT   = IDSUB(2,IPSUB) + 1
           IPBNK  = ISRT
           RETURN
         ENDIF
C
C        PRINT *,'From TBxFBK NOBANK =',NOBANK
C
C  -------------------------------------------------------------------
C     Get the largest pointer and offset of the IPSUB-th DSB in the
C     Sort_Table: ISORT and the Bank_Name_Table: BNKNAM,NWBNK.
C  -------------------------------------------------------------------
C
         IOFSET  = IDSUB(2,IPSUB)
         IPMAX   = IOFSET + NOBANK
C
C  -------------------------------------------------------------------
C     Search the Bank_Name: BNAME by the Binary_Search_Method
C  -------------------------------------------------------------------
 
         IP1     = IOFSET + 1
 
         IF( BNAME .LE. BNKNAM(ISORT(IP1)) ) THEN
             IF( BNAME .LT. BNKNAM(ISORT(IP1)) ) THEN
                 ISRT   = IP1
                 IPBNK  = IPMAX + 1
                 GO TO 1000
C                RETURN
             ELSE
                 IRET   = 1
                 ISRT   = IP1
                 IPBNK  = ISORT(IP1)
                 GO TO 1000
C                RETURN
             ENDIF
         ENDIF
 
         IP2     = IPMAX
 
         IF( BNAME .GE. BNKNAM(ISORT(IP2)) ) THEN
             IF( BNAME .GT. BNKNAM(ISORT(IP2)) ) THEN
                 ISRT   = IP2 + 1
                 IPBNK  = IPMAX + 1
                 GO TO 1000
C                RETURN
             ELSE
                 IRET   = 1
                 ISRT   = IP2
                 IPBNK  = ISORT(IP2)
                 GO TO 1000
C                RETURN
             ENDIF
         ENDIF
 
         IRET = 2
 
 
  100    ISRT    = 0.5*( IP1 + IP2 )
 
         IPBNK   = ISORT(ISRT)
 
         IF( BNAME .EQ. BNKNAM( IPBNK )) GO TO 1000
 
         IF( ISRT .EQ. IP1 ) THEN
             ISRT   = ISRT + 1
             IPBNK  = IPMAX + 1
             IRET = -1
             GO TO 1000
C            RETURN
         ENDIF
 
         IF( BNAME .LT. BNKNAM(IPBNK) ) THEN
             IP2 = ISRT
         ELSE
             IP1 = ISRT
         ENDIF
C
      GO TO 100
C
 
 1000 RETURN
      END
