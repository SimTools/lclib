C **********************************************************************
C*                                                                     *
C*===================================================================  *
C* Subroutine TBxSBK( IPSUB, BNAME, ISRT, IPBNK, HSALOC, HSEXT, IRET)  *
C*===================================================================  *
C*(Purpose)                                                            *
C*   Save the Bank_Name BNAME in Bank_Name_Table etc.                  *
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
 
      SUBROUTINE TBxSBK( IPSUB, BNAME, ISRT, IPBNK, HSALOC, HSEXT, IRET)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsmes.inc"
      CHARACTER*64  BNAME
C
      IRET = 0
C
C  --------------------------------------------------------------------
C          Check the number of Banks in the IPSUB-th DSB.
C  --------------------------------------------------------------------
C  --- Get the number of Banks NOBANK and the allocated number --------
C  --- of Banks MXBANK.                                        --------
C
         MXBANK   = IDSUB(3,IPSUB)
         NOBANK   = IDSUB(4,IPSUB)
C
C  --- If the NOBANK exceeds MXBANK, then set IRET < 0 and Return. ---
C
         IF( NOBANK .GE. MXBANK ) THEN
 
             BNKNO  = 1.1*(MXBANK+1)
             NOBNK  = BNKNO
             CALL TBxBEX( IPSUB, NOBNK, IRRET)
             IF( IRRET .LT. 0 ) THEN
                 IRET   = -1
                 WRITE(MESAGE(1),9000)
 9000            FORMAT(' Too many Banks ')
                 WRITE(MESAGE(2),9001)
 9001            FORMAT(' At creation of the bank')
                 WRITE(MESAGE(3),9002) BNAME
 9002            FORMAT(' ',A64)
                 CALL TBxERR( 100,'TBxSBK', 3)
                 RETURN
             ENDIF
 
             MXBANK  = NOBNK
 
         ENDIF
C  ---------------------------------------------------------------------
C          Shift the Contents of the Sort_Table: ISORT.
C  ---------------------------------------------------------------------
C  --- Get the largest pointer of the Banks in the Sort_Table: ISORT --
C
         IPMAX  = IDSUB(2,IPSUB) + NOBANK + 1
C
C  --- Shift the contents of the Sort_Table: ISORT  -------------------
C
C
         IF( ISRT .LT. IPMAX ) THEN
           NSHFT  = IPMAX - ISRT
           DO 100 I=1,NSHFT
             IP          = IPMAX - I
             ISORT(IP+1) = ISORT(IP)
  100      CONTINUE
         ENDIF
C
C  -- Save the pointer IPBNK of the Bank_Name_Table: BNKNAM into the---
C  -- Sort_Table: ISORT                                              ---
C
         ISORT(ISRT) = IPBNK
C
C  -- Append a new Bank into the Bank_Name_Table and Bank_Key_Table. ---
C
C                                Fill Bank_Name_Table by Bank_Name BNAME
C
         BNKNAM(IPBNK) = BNAME
C
C                                            Fill Bank_Key_Table: NWBNK
C
         NWBNK(1,IPBNK)  = HSALOC
         NWBNK(2,IPBNK)  = HSEXT
         KEYBNK(1,IPBNK) = -1
         KEYBNK(2,IPBNK) =  0
C
C ----- Update the number of Banks in the IPSUB-th DSB  ----------------
C
         IDSUB(4,IPSUB)  = NOBANK + 1
C
         RETURN
         END
