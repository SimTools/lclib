C **********************************************************************
C*                                                                     *
C*==============================================                       *
C* Subroutine TBxDEL( IPSUB, IPBNK, IFDEL, IRET)                       *
C*==============================================                       *
C*(Purpose)                                                            *
C*   Delete the Bank_Element BNAME:NE.                                 *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB in the DSB_Parameter_Table: IDSUB   *
C*   IPBNK    : Address of the Bank in Bank_Name_Table: BNKNAM,NWBNK   *
C*   IFDEL    : Delete Flag                                            *
C*     IFDEL >=0 : Delete the Element IFDEL                            *
C*     IFDEL < 0 : Delete all the elements in the Bank                 *
C*(Output)                                                             *
C*   IRET     : Return Flag                                            *
C*     IRET  > 0 : Succeeded in detetion.                              *
C*     IRET  < 0 : Not succeeded in deletion                           *
C*(Used COMMON)                                                        *
C*     /TBSBNK/   Further information, see TBREAD.                     *
C*(Author)                                                             *
C*       S.Kawabata     June 1 '84                                     *
C*       S.Kawabata     July 20 '85    Version up    V 3.0             *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxDEL( IPSUB, IPBNK, IFDEL, IRET)
 
#include "tbsbnk.inc"
C
C  -- Get the number of elements in this Bank.  ----------------------
C
         NOELMT            = KEYBNK(2,IPBNK)
C
C  -------------------------------------------------------------------
C     The case IFDEL >= 0, i.e. Delete the element IFDEL.
C  -------------------------------------------------------------------
C
         IF( IFDEL .GE. 0 ) THEN
C
C  --- Look for the element IFDEL  -----------------------------------
C
           IADD0       = 0
           IADD1       = KEYBNK(1,IPBNK)
           DO 100  I   = 1, NOELMT
C
             IF( IFDEL .EQ. LINK(1,IADD1) ) GO TO 200
C
             IADD0     = IADD1
             IADD1     = LINK(2,IADD0)
  100      CONTINUE
C
C  --- If the element IFDEL was not exist, then IRET = -1 and return.--
C
           IRET        = -1
           RETURN
C
C  --- If the element IFDEL was found, then  --------------------------
C
C     1. If the element IFDEL is the last element in the Bank, then
C       set LINK(2,IADD0) = -1.
C
  200      IF( LINK(2,IADD1) .EQ. -1 ) THEN
C
                IF(IADD0 .GT. 0 ) THEN
                   LINK(2,IADD0) = -1
                ENDIF
C
C     2. If the element IFDEL is the first element, then change
C       KEYBNK(1,IPBNK) = LINK(2,IADD1).
C
           ELSEIF(IADD0 .EQ. 0 ) THEN
               KEYBNK(1,IPBNK) = LINK(2,IADD1)
C
C     3. If the element IFDEL is in the middle of the elements, then
C       connect the elements prior and after the element IFDEL.
C
           ELSE
               LINK(2,IADD0)   = LINK(2,IADD1)
           ENDIF
C
C     4. Change the Bank_Key_Table
C
           LINK(1,IADD1)     = -1
           IADTBL(1,IADD1)   = -1
           IF( LINK(3,IADD1) .GE. 0 ) THEN
               CALL UVSET( IADTBL(3,IADD1), -1  )
               IADTBL(2,IADD1) =  0
               IADTBL(3,IADD1) =  0
               LINK(3,IADD1)    = -1
           ENDIF
           IF(NOELMT .EQ. 1) THEN
               NWBNK(2,IPBNK) = -1
           ENDIF
           KEYBNK(2,IPBNK)   = NOELMT - 1
           IDSUB(7,IPSUB)    = IDSUB(7,IPSUB) - 1
C
C (2) The case IFDEL < 0, i.e. Delete all elements.
C
         ELSE
             IADD1            = KEYBNK(1,IPBNK)
            DO 300    I       = 1, NOELMT
               IADTBL(1,IADD1)= -1
               IF( LINK(3,IADD1) .GE. 0 ) THEN
                   CALL UVSET( IADTBL(3,IADD1), -1  )
                   IADTBL(2,IADD1) =  0
                   IADTBL(3,IADD1) =  0
                   LINK(3,IADD1)    = -1
               ENDIF
               LINK(1,IADD1)  = -1
               IADD1          = LINK(2,IADD1)
  300       CONTINUE
C
             KEYBNK(1,IPBNK)  = -1
             KEYBNK(2,IPBNK)  = 0
             IDSUB(7,IPSUB)   = IDSUB(7,IPSUB) - NOELMT
         ENDIF
C
        IRET   = 1
C
         RETURN
         END
