C **********************************************************************
C*                                                                     *
C*============================================                         *
C* Subroutine TBxFEL( IPBNK, NE, IPELM, IRET)                          *
C*============================================                         *
C*(Purpose)                                                            *
C*   Find the Bank_element NE.                                         *
C*(Input)                                                              *
C*   IPBNK    : Address of the Bank in Bank_Name_Table: BNKNAM,NWBNK   *
C*   NE       : Bank_Element_Number                                    *
C*(Output)                                                             *
C*   IPELM    : Address of Element in the Address_Table: IADTBL        *
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning; Bank_Element was not found.                 *
C*     IRET >=0 : Normal Return                                        *
C*(Used COMMON)                                                        *
C*      /TBSBNK/     For detail information, see TBREAD.               *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Aug 23 '84     Update                          *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxFEL( IPBNK, NE, IPELM, IRET)
 
#include "tbsbnk.inc"
C
C  ---- Get the number of Bank_Elements: NBELMT in the Bank.  ---------
C
         NBELMT   = KEYBNK(2,IPBNK)
         IRET     = -1
C//// DEBUG
C        WRITE(6,9000) NBELMT
C9000    FORMAT(1X,'TBxFEL  NBELMT =',I6)
C
C  -- If the NBELMT is zero, i.e. still there is no Element, then ------
C  --   set IRET = -1 and return.                                 ------
C
         IF( NBELMT .GT. 0 ) THEN
C
C  -- If the Bank has element, i.e. NBELMT > 0, then ------------------
C
C                                           Look for the Bank_Element NE
C
           IPELM    = KEYBNK(1,IPBNK)
           DO 100 I = 1, NBELMT
C
             IF( LINK(1,IPELM)-NE ) 10,200,200
C            IF( LINK(1,IPELM) .GE. NE ) GO TO 200
C
   10        IPELM  = LINK(2,IPELM)
  100      CONTINUE
C
C  --  If the Element_Number NE was no found, then set IRET = -1 and --
C  -- return.                                                        --
C
         ENDIF
         RETURN
C
C  --  If the Element_Number NE was found, then set IRET = 1 and  -----
C  -- return.                                                     -----
C
  200    IF( LINK(1,IPELM) .EQ. NE ) IRET = 1
C
         RETURN
         END
