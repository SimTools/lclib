      SUBROUTINE TBxCRD( ID, IPSUB)
C***********************************************************************
C*                                                                     *
C*================================                                     *
C* Subroutine TBxCRD( ID, IPSUB )                                      *
C*================================                                     *
C*(Purpose)                                                            *
C*   Find the record ID in the DSB table and the record table.         *
C*(Input)                                                              *
C*   ID       : ID_Number of the Record                                *
C*(Output)                                                             *
C*   IPSUB <0 : This Return_Flag means," No such Record exists in      *
C*              any DSB and record tables.                             *
C*   IPSUB >0 : Pointer of the DSB in the DSB_Parameter_Table          *
C*              where the record is to be saved.                       *
C*(Author)                                                             *
C*       S.Kawabata     Nov 27 '85                                     *
C*                                                                     *
C***********************************************************************
 
#include "tbsbnk.inc"
 
C  ----- Look for the ID_Number ---------------------------------------
 
         IF( NOSUB .GT. 0 ) THEN
           DO 150 I= 1,NOSUB
             IF( ID .EQ. IDSUB(1,I) ) GO TO 200
             IF( IDSUB(9,I) .GT. 0 ) THEN
                 K        = IDSUB(10,I)
                 DO 100 J = 1, IDSUB(9,I)
                    IF( IDSHR( J, K) .EQ. ID) GO TO 200
  100            CONTINUE
             ENDIF
  150      CONTINUE
         ENDIF
C
C  ----- If such a record ID was not found, set IPSUB < 0. ---------
C
         IPSUB    = -1
         RETURN
C
C  ----- If the record ID was found, set IPSUB = I. -----------------
C
  200    IPSUB    = I
C
         RETURN
         END
