C **********************************************************************
C*                                                                     *
C*============================================================         *
C* Subroutine TBMOD( ID, BNAME, NE, IADD, NWORDS, IDATA, IRET)         *
C*============================================================         *
C*(Purpose)                                                            *
C*   Put the NWORDS Data: IDATA into the Bank_Element BNAME:NE.        *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NE       : Element_Number                                         *
C*   IADD     : Relative address to be modified position in the element*
C*   NWORDS   : Word_Count of the input data                           *
C*   IDATA    : Data                                                   *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*          -1  : Warning; DSB ID does not exist.                      *
C*          -2  : Warning; Bank BNAME does not exist.                  *
C*          -3  : Warning; Given address is out of range.              *
C*     IRET >=0 : Normal return                                        *
C*           1  : Normal Return;  The element was replaced.            *
C*           2  : Normal Return;  New element was created.             *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  For further information, see TBREAD.     *
C*(Author)                                                             *
C*       S.Kawabata     Sep  1 '84                                     *
C*       S.Kawabata     July 20 '85   Version up  V 3.0                *
C*       S.Kawabata     Nov. 27 '85   Version up  V 4.0                *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBMOD( ID, BNAME, NE, IADD, NWORDS, IDATA, IRET)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
      INTEGER       IDATA(NWORDS)
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
C  -----------------------------------------------------------------
C       (1)  Find Data_Sub_Buffer ID
C  -----------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBMOD', 1)
 
           IRET     = -1
           RETURN
         ENDIF
C
C  -----------------------------------------------------------------
C       (2)  Find Bank_name: BNAME
C  -----------------------------------------------------------------
 
         TNAME = BNAME
 
         CALL UCUPER( LEN(BNAME), TNAME)
 
C                                   ====  =====  ====
         CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9011)
 9011        FORMAT(' The following bank does not exist.')
             WRITE(MESAGE(2),9012) TNAME
 9012        FORMAT(' ',A64)
             CALL TBxERR( 200,'TBMOD', 2)
 
           IRET = -2
           RETURN
         ENDIF
C
C  ----------------------------------------------------------------
C       (3)  find Bank_Element NE
C  ----------------------------------------------------------------
C                                =====  ====
         CALL TBxFEL( IPBNK, NE, IPELM, IRET)
C
C//// DEBUG
C        WRITE(6,9000) IPBNK,NE,IPELM,IRET
C9000 FORMAT(1X,'TBMOD IPBNK,NE,IPELM,IRET=',4I6)
C
C -------------------- Element NE does not exist ------------------
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9021)
 9021        FORMAT(' The following bank element does not exist.')
             WRITE(MESAGE(2),9022) TNAME
 9022        FORMAT(' Bank name = ',A64)
             WRITE(MESAGE(3),9023) NE
 9023        FORMAT(' Element # = ',I6)
             CALL TBxERR( 200,'TBMOD', 3)
 
               IRET = -3
               RETURN
         ENDIF
C
C  ------------------- Element was found --------------------------
C
            HWDSB  = NWELM(1,IPELM)
            NWDSB  = (HWDSB+1)/2
C
         IF( IADD .LE. 0 .OR. IADD .GT. NWDSB ) THEN
 
             WRITE(MESAGE(1),9031)
 9031        FORMAT(' The relative start address is out of range.')
             WRITE(MESAGE(2),9032) IADD
 9032        FORMAT(' Relative start address =',I6)
             WRITE(MESAGE(3),9033) NWDSB
 9033        FORMAT(' out of 0 < Address =<',I6)
             CALL TBxERR( 200,'TBMOD', 3)
 
                 IRET = -4
                 RETURN
         ENDIF
C
            NWNEW  = IADD + NWORDS - 1
C
         IF( NWNEW .GT. NWDSB ) THEN
 
             WRITE(MESAGE(1),9041)
 9041        FORMAT(' The relative end address is out of range.')
             WRITE(MESAGE(2),9042) NWNEW
 9042        FORMAT(' Relative end address =',I6)
             WRITE(MESAGE(3),9033) NWDSB
             CALL TBxERR( 200,'TBMOD', 3)
 
                 IRET = -5
                 RETURN
         ENDIF
C
C  -------------------------------------------------------------------
C               Copy data into the element
C  -------------------------------------------------------------------
 
         HWNEW   = 2*NWNEW
         IF( NWORDS .GT. 0 ) THEN
           IF( HWNEW .GT. NWELM(2,IPELM) ) THEN
              NWELM(2,IPELM)   = HWNEW
           ENDIF
 
           IOFSET  = (IADTBL(1,IPELM)+NWSUB(1,IPSUB)+1)/2 + IADD - 1
 
           CALL UVCOPY( NWORDS, IDATA(1), IBUFF(IOFSET))
 
C The following statements are for the versions before V402.
C          IOFSET  = (IADTBL(1,IPELM)+NWSUB(1,IPSUB)+1)/2 + IADD - 2
C          DO     500     I  = 1, NWORDS
C            IBUFF(I+IOFSET) = IDATA(I)
C 500      CONTINUE
 
         ENDIF
C
         IRET = 1
 
         RETURN
         END
