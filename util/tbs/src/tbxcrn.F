      SUBROUTINE TBxCRN( RECNAM, IDSH, IPSUB )
C***********************************************************************
C*                                                                     *
C*=========================================                            *
C* Subroutine TBxCRN( RECNAM, IDSH, IPSUB )                            *
C*=========================================                            *
C*(Purpose)                                                            *
C*   Find the record name RECNAM in the DSB and the Record tables      *
C*(Input)                                                              *
C*   RECNAM   : Record name in character string                        *
C*(Output)                                                             *
C*   IDSH  <0 : The Record name does not exist in the sharing record   *
C*              table.                                                 *
C*   IDSH  >0 : When the record name exists in the sharing record table*
C*              , IDSH gives pointer of the record in the table.       *
C*   IPSUB <0 : This Return_Flag means," No such Record exists in      *
C*              any DSB and record tables.                             *
C*   IPSUB >0 : Pointer of the DSB in the DSB_Parameter_Table          *
C*              where the record is to be saved.                       *
C*(Author)                                                             *
C*       S.Kawabata     Nov 27 '85                                     *
C*                                                                     *
C***********************************************************************
 
#include "tbsbnk.inc"
       CHARACTER*64 RECNAM
 
         IDSH     = -1
         IPSUB    = -1
 
C  ----- Look for the ID_Number ---------------------------------------
 
         IF( NOSUB .GT. 0 ) THEN
           DO 150 I= 1,NOSUB
             IF( RECNAM .EQ. SUBNAM(I) ) GO TO 200
             IF( IDSUB(9,I) .GT. 0 ) THEN
                 K        = IDSUB(10,I)
                 DO 100 J = 1, IDSUB(9,I)
                    IF( RECNAM .EQ. SHRNAM( J, K) ) THEN
                        IDSH  = J
                        GO TO 200
                    ENDIF
  100            CONTINUE
             ENDIF
  150      CONTINUE
         ENDIF
C
C  ----- If such a record ID was not found, set IPSUB < 0. ---------
C
         RETURN
C
C  ----- If the record ID was found, set IPSUB = I. -----------------
C
  200    IPSUB    = I
C
         RETURN
         END
