      SUBROUTINE TBxSPC( IPSUB,  IPELM, NWDSB, IRTN)
C***********************************************************************
C*                                                                     *
C*=====================================================                *
C* Subroutine TBxSPC( IPSUB, IPELM, NWDSB, IRTN)                       *
C*=====================================================                *
C*(Purpose)                                                            *
C*   Find out whether there is a free room to allocate the NWDSB       *
C* words at the position soon after this element.                      *
C*                                                                     *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB: ID in DSB_Parameter_Table: IDSUB   *
C*   IPELM    : Address of Element in the Element_Number_List: LISTEL  *
C*   NWDSB    : The allocation size.                                   *
C*(Output)                                                             *
C*   IRTN     : Return_Flag                                            *
C*     IRTN=0   : We can extend the allocation of this element.        *
C*     IRTN=-1  : We can not allocate NWDSB words at the position.   . *
C*(Used COMMON)                                                        *
C*     /TBSBNK/, /TBSBUF/ and /TBSCTL/                                 *
C*(Author)                                                             *
C*       A. Miyamoto    Aug 30 '84                                     *
C*                                                                     *
C***********************************************************************
C
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsctl.inc"
C
 
C
C  --------------------------------------------------------------
C    (1) Can this element be contained in the DSB ?
C  --------------------------------------------------------------
C
      NWOFT  = NWSUB(1,IPSUB)
      ILSADR = IADTBL(1,IPELM) + NWDSB + NWOFT
      IF(ILSADR .GT. NWSUB(1,IPSUB) + NWSUB(2,IPSUB) ) THEN
         IRTN = -1
         RETURN
      ENDIF
C
C  -------------------------------------------------------------------
C      (2) Loop over all elements, all banks in this DSB to search
C         any element with the starting address greater than the
C         first address of this element.
C  -------------------------------------------------------------------
C
      IFSADR = IADTBL(1,IPELM) + NWOFT
      JOFT   = IDSUB(2,IPSUB)
 
      DO 200 JBNK = 1, IDSUB(4,IPSUB)
        JPELM = KEYBNK(1,JBNK+JOFT)
        IF( JPELM .LT. 0 ) GO TO 200
210     CONTINUE
        JP1ST = IADTBL(1,JPELM) + NWOFT
C       PRINT *,' %TBxSPC..JBNK,JPELM,JP1ST,IFSADR,ILSADR=',
C    >                     JBNK,JPELM,JP1ST,IFSADR,ILSADR
        IF(JP1ST.GT.IFSADR ) THEN
          IF(JP1ST.LE.ILSADR) THEN
            IRTN = -1
            RETURN
          ENDIF
        ENDIF
 
        JPELM = LINK(2,JPELM)
        IF(JPELM.GT.0) GO TO 210
 
200     CONTINUE
 
        IRTN = 0
 
        RETURN
        END
