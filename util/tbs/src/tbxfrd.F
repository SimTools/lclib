      SUBROUTINE TBxFRD( ID, IPSUB)
C***********************************************************************
C*                                                                     *
C*================================                                     *
C* Subroutine TBxFRD( ID, IPSUB )                                      *
C*================================                                     *
C*(Purpose)                                                            *
C*   Find the record ID in the allocated DSBs                          *
C*(Input)                                                              *
C*   ID       : ID_Number of the Record                                *
C*(Output)                                                             *
C*   IPSUB <0 : This Return_Flag means," No such Record exists in      *
C*              any DSB.                                               *
C*   IPSUB >0 : Pointer of the DSB in the DSB_Parameter_Table          *
C*              where the record is currently saved.                   *
C*(Author)                                                             *
C*       S.Kawabata     Nov 27 '85                                     *
C*                                                                     *
C***********************************************************************
 
#include "tbsbnk.inc"
 
C  ----- Look for the ID_Number ---------------------------------------
 
         IF( NOSUB .GT. 0 ) THEN
           DO 100 I= 1,NOSUB
 
             IF(( ID .EQ. IDSUB(1,I)) .AND.
     .          ( IDSUB(11,I) .EQ. 0 ) )    GO TO 200
 
             IF( IDSUB(9,I) .GT. 0 ) THEN
                 IP    = IDSUB(11,I)
                 IF( IP .GT. 0 ) THEN
                     IF( ID .EQ. IDSHR( IP, IDSUB(10,I)) ) GO TO 200
                 ENDIF
             ENDIF
  100      CONTINUE
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
