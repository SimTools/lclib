C **********************************************************************
C*                                                                     *
C*=========================================                            *
C* Subroutine TBxBDL( IPSUB, ISRT, IPBNK )                             *
C*=========================================                            *
C*(Purpose)                                                            *
C*   Delete the Bank, which identified by IPBNK                        *
C*(Input)                                                              *
C*   IPSUB    : Address of the record in Data sub-buffer table         *
C*   ISRT     : Address in the sort table of the bank                  *
C*   IPBNK    : Address in the bank key table of the bank              *
C*(Output)                                                             *
C*   none                                                              *
C*(Used COMMON)                                                        *
C*     /TBSBNK/    Further information, see TBREAD.                    *
C*(Author)                                                             *
C*       S.Kawabata     Feb.13 '86    Version V4.0                     *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxBDL( IPSUB, ISRT, IPBNK )
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
 
 
C  --- Get the maximum address of the Banks in the Bank_Name_Table ---
C
         NOBANK  = IDSUB(4,IPSUB)
         IOFSET  = IDSUB(2,IPSUB)
         MXBANK  = IOFSET + NOBANK - 1
C
C  --- Shift the Bank_Name_Table: BNKNAM and Bank_Key_Table: NWBNK --
C
         IF( IPBNK .LE. MXBANK) THEN
           DO 200  J       = IPBNK, MXBANK
             K             = J + 1
             BNKNAM(J)     = BNKNAM(K)
             DO 100 I      = 1, 2
               NWBNK(I,J)  = NWBNK(I,K)
               KEYBNK(I,J) = KEYBNK(I,K)
  100        CONTINUE
  200      CONTINUE
         ENDIF
C
C  -------------- Shift the Sort_Table: ISORT  ----------------------
C
C
         IOFSET         = IOFSET + 1
         IF( ISRT .LE. MXBANK) THEN
 
           DO 300  J      = IOFSET, MXBANK
             IF( J .LT. ISRT ) THEN
               K          = J
             ELSE
               K          = J + 1
             ENDIF
             IADD         = ISORT(K)
             IF(IADD .GT. IPBNK ) THEN
               IADD       = IADD - 1
             ENDIF
C            PRINT *,' ISORT(K),ISORT(J)=',ISORT(K),ISORT(J)
             ISORT(J)     = IADD
  300      CONTINUE
         ELSEIF( ISRT .EQ. MXBANK+1) THEN
 
           DO 400  J      = IOFSET, MXBANK
             IADD         = ISORT(J)
             IF(IADD .GT. IPBNK ) THEN
               IADD       = IADD - 1
               ISORT(J)     = IADD
             ENDIF
  400      CONTINUE
         ENDIF
C
C  ------- Decrement the Number of Banks in the IPSUB-th DSB. --------
C
         IDSUB(4,IPSUB)   = NOBANK - 1
C
C  ------- Set Flag for Gabage collection  ---------------------------
C
         IDSUB(8,IPSUB)   = 1
C
C
         RETURN
         END
