      SUBROUTINE TBxFSB( ID, IPSUB)
C***********************************************************************
C*                                                                     *
C*================================                                     *
C* Subroutine TBxFSB( ID, IPSUB )                                      *
C*================================                                     *
C*(Purpose)                                                            *
C*   Find the DSB: ID.                                                 *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*(Output)                                                             *
C*   IPSUB <0 : This Return_Flag means," No such DSB exists."          *
C*   IPSUB >0 : Pointer of the DSB: ID in the DSB_Parameter_Table      *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov 27 '85  last update                        *
C*                                                                     *
C***********************************************************************
 
#include "tbsbnk.inc"
 
C  ----- Look for the ID_Number ---------------------------------------
 
         IF( NOSUB .GT. 0 ) THEN
           DO 100 I= 1,NOSUB
             IF( ID .EQ. IDSUB(1,I) ) GO TO 200
  100      CONTINUE
         ENDIF
C
C  ----- If such DSB was not found, set IPSUB < 0. --------------------
C
         IPSUB    = -1
         RETURN
C
C  ----- If the DSB was found, set IPSUB = I. -------------------------
C
  200    IPSUB    = I
C
         RETURN
         END
